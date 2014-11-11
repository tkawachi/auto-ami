package com.github.tkawachi.autoami

import java.io.File
import java.text.SimpleDateFormat
import java.util.{ Date, Locale }

import com.amazonaws.auth.{ AWSCredentialsProvider, DefaultAWSCredentialsProviderChain }
import com.amazonaws.regions.{ Regions, Region }
import com.amazonaws.services.ec2.AmazonEC2Client
import com.amazonaws.services.ec2.model._
import scala.annotation.tailrec
import scala.collection.JavaConverters._

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Try, Failure, Success }

/**
 * Main entry point.
 *
 * inspired by http://blog.suz-lab.com/2012/05/ec2ami.html
 */
object Main {
  val BACKUP_GENERATION = "BackupGeneration"
  val dateFormat = new SimpleDateFormat("yyyyMMddHHmmss", Locale.US)

  def getRunningInstances(ec2: AmazonEC2Client): List[Instance] = {
    @tailrec
    def loop(nextToken: Option[String], accum: List[Reservation]): List[Reservation] = {
      val resp = ec2.describeInstances(
        new DescribeInstancesRequest().withFilters(
          new Filter("instance-state-name").withValues("running"),
          new Filter("tag-key").withValues(BACKUP_GENERATION)
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

  def backup(ec2: AmazonEC2Client, instance: Instance, now: Date) = {
    val instanceId = instance.getInstanceId

    val tags = instance.getTags.asScala
    val imageTag = tags.find(_.getKey == "Name").map(_.getValue).getOrElse(instanceId)
    val generation = tags.find(_.getKey == BACKUP_GENERATION)
      .flatMap(tag => Try(tag.getValue.toInt).toOption)

    generation.map { gen =>
      val imageName = s"$imageTag-${dateFormat.format(now)}"
      // Keep (gen - 1) old images.
      val keepOld = gen - 1
      val images = findDeleteImages(ec2, imageTag, keepOld)
      val imageId = createImage(ec2, imageName, instanceId)
      deleteImages(ec2, images)
      tagImage(ec2, imageId, imageTag)
      tagSnapshots(ec2, imageId)
      imageId
    }
  }

  def createImage(ec2: AmazonEC2Client, imageName: String, instanceId: String): String = {
    val req = new CreateImageRequest()
      .withInstanceId(instanceId)
      .withName(imageName)
      .withNoReboot(true)
      .withDescription(s"Created from $instanceId.")
    val resp = ec2.createImage(req)
    resp.getImageId
  }

  def findDeleteImages(ec2: AmazonEC2Client, imageTag: String, generation: Int): List[Image] = {

    val req = new DescribeImagesRequest().withFilters(
      new Filter("tag:Name").withValues(imageTag),
      new Filter("tag:BackupType").withValues("auto")
    )
    val resp = ec2.describeImages(req)
    val images = resp.getImages.asScala.toList
    images.sortBy(_.getName).dropRight(generation)
  }

  def deleteImages(ec2: AmazonEC2Client, images: List[Image]): Unit = {
    images.foreach { image =>
      ec2.deregisterImage(new DeregisterImageRequest(image.getImageId))
      image.getBlockDeviceMappings.asScala.foreach { mapping =>
        val snapshotId = mapping.getEbs.getSnapshotId
        ec2.deleteSnapshot(new DeleteSnapshotRequest(snapshotId))
      }
    }
  }

  def tagImage(ec2: AmazonEC2Client, imageId: String, imageTag: String): Unit = {
    ec2.createTags(new CreateTagsRequest().withResources(imageId).withTags(new Tag("Name", imageTag), new Tag("BackupType", "auto")))
  }

  def basename(path: String): String = new File(path).getName

  def tagSnapshots(ec2: AmazonEC2Client, imageId: String): Unit = {
    val resp = ec2.describeImages(new DescribeImagesRequest().withImageIds(imageId))
    resp.getImages.asScala.foreach { image =>
      image.getBlockDeviceMappings.asScala.foreach { mapping =>
        val snapshotId = mapping.getEbs.getSnapshotId
        val snapshotTag = s"${image.getName}-${basename(mapping.getDeviceName)}"
        ec2.createTags(
          new CreateTagsRequest().withResources(snapshotId)
            .withTags(
              new Tag("Name", snapshotTag),
              new Tag("BackupType", "auto")
            )
        )
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val credentials = new DefaultAWSCredentialsProviderChain
    val now = new Date

    val fs = Regions.values().map { regions =>
      Future {
        val ec2 = new AmazonEC2Client(credentials)
        ec2.setRegion(Region.getRegion(regions))
        val instances = getRunningInstances(ec2)
        instances.map { i =>
          backup(ec2, i, now)
        }
      }
    }

    fs.foreach { f =>
      f.onComplete {
        case Success(l) => l.foreach(println)
        case Failure(e) => println(e.getMessage)
      }
      Await.ready(f, 1.minute)
    }
  }
}
