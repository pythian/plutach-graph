package plutarch

import com.typesafe.scalalogging.LazyLogging
import plutarch.server.AppServer
import buildinfo.BuildInfo

object Main extends App with LazyLogging {
  logger.info("Starting the {} server", BuildInfo.name)
  AppServer.up()
}
