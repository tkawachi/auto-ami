package com.github.tkawachi.autoami

import com.github.kxbmap.configs._
import com.typesafe.config.{ ConfigFactory, Config => TSConfig }

import scala.concurrent.duration.Duration

case class Config(
  backupGenerationKey: String,
  markerTags: Map[String, String],
  maxRetry: Int,
  retryDelay: Duration,
  timeout: Duration)

object Config {
  def apply(c: TSConfig): Config = new Config(
    c.get[String]("autoAMI.backupGenerationKey"),
    c.get[Map[String, String]]("autoAMI.markerTags"),
    c.get[Int]("autoAMI.retry.maxAttempts"),
    c.get[Duration]("autoAMI.retry.delay"),
    c.get[Duration]("autoAMI.timeout")
  )

  def apply(): Config = apply(ConfigFactory.load())
}
