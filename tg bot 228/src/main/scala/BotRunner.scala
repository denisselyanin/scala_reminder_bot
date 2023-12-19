import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import org.http4s.client.{Client, JavaNetClientBuilder}
import pureconfig._
import pureconfig.generic.auto._
import scala.concurrent.duration._

object BotRunner extends IOApp {
  private val botToken = ConfigSource.default.at("bot_config").loadOrThrow[BotConfig].token

  private def pollUpdates(bot: TelegramBot, lastUpdateId: Option[Long])(implicit client: Client[IO]): IO[Unit] = {
    bot.getUpdates(lastUpdateId).flatMap { updates =>
      processUpdates(bot, updates) >>
        determineNextUpdateId(updates, lastUpdateId).flatMap { nextUpdateId =>
          IO.sleep(1.second) >>
            pollUpdates(bot, nextUpdateId)
        }
    }.handleErrorWith { error =>
      IO(println(s"Error occurred while polling updates: ${error.getMessage}")) >>
        IO.sleep(1.second) >>
        pollUpdates(bot, lastUpdateId)
    }
  }

  private def processUpdates(bot: TelegramBot, updates: List[Update])(implicit client: Client[IO]): IO[Unit] = {
    updates.traverse_(update => bot.handleUpdate(update))
  }

  private def determineNextUpdateId(updates: List[Update], lastUpdateId: Option[Long]): IO[Option[Long]] = {
    IO {
      updates.lastOption match {
        case Some(lastUpdate) => Some(lastUpdate.update_id)
        case None => lastUpdateId
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    JavaNetClientBuilder[IO].resource.use { client =>
      pollUpdates(new TelegramBot(botToken), None)(client)
    }.as(ExitCode.Success)
  }
}