package com.github.tkawachi.autoami

import java.io.File
import java.text.SimpleDateFormat
import java.util.{ Date, Locale }

import com.amazonaws.auth.DefaultAWSCredentialsProviderChain
import com.amazonaws.regions.{ Region, Regions }
import com.amazonaws.services.ec2.AmazonEC2Client
import com.amazonaws.services.ec2.model._
import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Await, Future }
import scala.util.{ Failure, Success, Try }

/**
 * Main entry point.
 *
 * inspired by http://blog.suz-lab.com/2012/05/ec2ami.html
 */
object Main extends LazyLogging {
  val dateFormat = new SimpleDateFormat("yyyyMMddHHmmss", Locale.US)

  def getRunningInstances(ec2: AmazonEC2Client, config: Config): List[Instance] = {
    @tailrec
    def loop(nextToken: Option[String], accum: List[Reservation]): List[Reservation] = {
      val resp = ec2.describeInstances(
        new DescribeInstancesRequest().withFilters(
          new Filter("instance-state-name").withValues("running"),
          new Filter("tag-key").withValues(config.backupGenerationKey)
        )
      )
      val list = accum ++ resp.getReservations.asScala
      Option(resp.getNextToken) match {
        case nt @ Some(_) => loop(nt, list)
        case None => list
      }
    }

    loop(None, Nil).flatMap(_.getInstances.asScala)
  }

  def backup(ec2: AmazonEC2Client, instance: Instance, now: Date, config: Config): Option[String] = {
    val instanceId = instance.getInstanceId
    logger.info(s"Start backup $instanceId")
    val tags = instance.getTags.asScala
    val imageTag = tags.find(_.getKey == "Name").map(_.getValue).getOrElse(instanceId)
    val generation = tags.find(_.getKey == config.backupGenerationKey)
      .flatMap(tag => Try(tag.getValue.toInt).toOption)

    generation.map { gen =>
      val imageName = s"$imageTag-${dateFormat.format(now)}"
      // Keep (gen - 1) old images.
      val keepOld = gen - 1
      val images = findDeleteImages(ec2, imageTag, keepOld, config)
      val imageId = createImage(ec2, imageName, instanceId)
      deleteImages(ec2, images)
      tagImage(ec2, imageId, imageTag, config)
      tagSnapshots(ec2, imageId, config)
      imageId
    }
  }

  def createImage(ec2: AmazonEC2Client, imageName: String, instanceId: String): String = {
    logger.debug(s"Creating an AMI named $imageName from $instanceId")
    val req = new CreateImageRequest()
      .withInstanceId(instanceId)
      .withName(imageName)
      .withNoReboot(true)
      .withDescription(s"Created from $instanceId.")
    val resp = ec2.createImage(req)
    logger.info(s"AMI ${resp.getImageId} ($imageName) was created from $instanceId")
    resp.getImageId
  }

  def findDeleteImages(ec2: AmazonEC2Client, imageTag: String, generation: Int, config: Config): List[Image] = {
    val filters = config.markerTags.updated("Name", imageTag)
    logger.debug(s"Finding images to delete: $filters")
    val resp = ec2.describeImages(new DescribeImagesRequest().withFilters(filters.toFilters))
    val images = resp.getImages.asScala.toList
    images.sortBy(_.getName).dropRight(generation)
  }

  def deleteImages(ec2: AmazonEC2Client, images: List[Image]): Unit = {
    images.foreach { image =>
      logger.debug(s"AMI ${image.getImageId} (${image.getName}) will be deleted")
      ec2.deregisterImage(new DeregisterImageRequest(image.getImageId))
      logger.info(s"AMI ${image.getImageId} (${image.getName}) was deleted")
      image.getBlockDeviceMappings.asScala.foreach { mapping =>
        val snapshotId = mapping.getEbs.getSnapshotId
        logger.debug(s"Snapshot $snapshotId used by ${image.getImageId} will be deleted")
        ec2.deleteSnapshot(new DeleteSnapshotRequest(snapshotId))
        logger.info(s"Snapshot $snapshotId used by ${image.getImageId} was deleted")
      }
    }
  }

  def tagImage(ec2: AmazonEC2Client, imageId: String, imageTag: String, config: Config): Unit = {
    val tags = config.markerTags.updated("Name", imageTag)
    logger.debug(s"$imageId will be tagged. $tags")
    ec2.createTags(
      new CreateTagsRequest().withResources(imageId).withTags(tags.toTags)
    )
    logger.info(s"$imageId was tagged. $tags")
  }

  def basename(path: String): String = new File(path).getName

  def tagSnapshots(ec2: AmazonEC2Client, imageId: String, config: Config): Unit = {
    @tailrec
    def loop(retryCnt: Int): Option[Image] = {
      if (retryCnt <= 0) {
        logger.error(s"Give up describeImages($imageId)")
        None
      } else {
        val resp = ec2.describeImages(new DescribeImagesRequest().withImageIds(imageId))
        val images = resp.getImages.asScala
        if (images.size <= 0) {
          logger.debug(s"describeImages($imageId) doesn't contain any result, retrying")
          Thread.sleep(config.retryDelay.toMillis)
          loop(retryCnt - 1)
        } else {
          val image = images.head
          val notReady = image.getBlockDeviceMappings.asScala.exists { mapping =>
            Option(mapping.getEbs.getSnapshotId).isEmpty
          }
          if (notReady) {
            logger.debug(s"snapshotId is not ready for $imageId, retrying")
            Thread.sleep(config.retryDelay.toMillis)
            loop(retryCnt - 1)
          } else {
            Some(image)
          }
        }
      }
    }

    loop(config.maxRetry).foreach { image =>
      // TODO BlockDeviceMappings might become empty?
      logger.debug(s"BlockDeviceMappings $imageId: ${image.getBlockDeviceMappings.asScala.map(_.getDeviceName).toList}")
      image.getBlockDeviceMappings.asScala.foreach { mapping =>
        val snapshotId = mapping.getEbs.getSnapshotId
        val snapshotTag = s"${image.getName}-${basename(mapping.getDeviceName)}"
        val tags = config.markerTags.updated("Name", snapshotTag)
        logger.debug(s"$snapshotId will be tagged. $tags")
        ec2.createTags(
          new CreateTagsRequest().withResources(snapshotId)
            .withTags(tags.toTags)
        )
        logger.info(s"$snapshotId was tagged. $tags")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val credentials = new DefaultAWSCredentialsProviderChain
    val config = Config()

    val now = new Date

    val fs = Regions.values().map { regions =>
      val ec2 = new AmazonEC2Client(credentials)
      val region = Region.getRegion(regions)
      val instancesFut = Future {
        ec2.setRegion(region)
        getRunningInstances(ec2, config)
      }
      instancesFut.flatMap { instances =>
        Future.traverse(instances) { instance =>
          Future {
            backup(ec2, instance, now, config)
          }.recover {
            case e: Throwable =>
              logger.error(e.getMessage)
              None
          }
        }
      }
    }

    fs.foreach { f =>
      f.onComplete {
        case Success(l) =>
        case Failure(e) => logger.error(e.getMessage)
      }
      Await.ready(f, config.timeout)
    }
  }

  implicit class ImplicitMapStringString(val m: Map[String, String]) extends AnyVal {
    def toTags: java.util.List[Tag] =
      m.map { case (k, v) => new Tag(k, v) }.toList.asJava

    def toFilters: java.util.List[Filter] =
      m.map { case (k, v) => new Filter(s"tag:$k").withValues(v) }.toList.asJava
  }
}
