import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.http4s.client.Client
import org.http4s._
import org.http4s.circe._
import io.circe.generic.auto._
import io.circe.JsonObject
import io.circe.Json

import scala.util.control.NonFatal
import org.http4s.circe.CirceEntityDecoder._

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ArrayBuffer
import com.github.nscala_time.time.Imports._
import io.circe.syntax._
import org.http4s.Status
import org.http4s.headers.`Content-Type`
import org.http4s.MediaType



case class BotConfig(token: String)

case class SendMessage(chat_id: Long, text: String)
case class Update(update_id: Long, message: Message)
case class Message(message_id: Long, text: Option[String], chat: Chat)
case class Chat(id: Long)
case class TelegramResponse(ok: Boolean, result: List[Update])
case class DateMessage(id: Long, time: DateTime, text: String)


class TelegramBot(token: String) {
  private val baseUrl = Uri.uri("https://api.telegram.org") / s"bot$token"

  private val messages_map = TrieMap[Chat, ArrayBuffer[DateMessage]]()
  private val ids_map = TrieMap[Chat, TrieMap[Long, DateMessage]]()

  private def sendMessage(message: SendMessage)(implicit client: Client[IO]): IO[Unit] = {
    val jsonRequest = Json.obj(
      "chat_id" -> Json.fromLong(message.chat_id),
      "text" -> Json.fromString(message.text)
    )

    val request = Request[IO](Method.POST, baseUrl / "sendMessage")
      .withEntity(jsonRequest)
      .withContentType(`Content-Type`(MediaType.application.json))

    client.status(request).flatMap { status =>
      if (status.code >= 200 && status.code < 300) {
        IO(println("Message sent successfully"))
      } else {
        IO(println(s"Failed to send message: $status"))
      }
    }
  }

  private def createReminder(chat: Chat, text: String)(implicit client: Client[IO]): IO[Unit] = {
    val text_arr = text.dropWhile(_ != ' ').trim.split(' ')
    if (text_arr.length < 2 || !text_arr.last.forall(_.isDigit)) {
      sendMessage(SendMessage(chat.id, "Мало аргументов для команды или не передано число"))
    } else {
      var valid_id: Long = 0
      if (!messages_map.contains(chat)) {
        messages_map.put(chat, ArrayBuffer.empty[DateMessage])
      }
      if (!ids_map.contains(chat)) {
        ids_map.put(chat, TrieMap[Long, DateMessage]())
      } else {
        valid_id = (ids_map(chat).keySet.max + 1)
      }
      val messages = messages_map(chat)
      val dateMessage = DateMessage(valid_id, DateTime.now().plusMinutes(text_arr.last.toInt), text_arr.slice(0, text_arr.length - 1).mkString(" "))
      messages_map.put(chat, messages.append(dateMessage))
      ids_map(chat).put(valid_id, dateMessage)
      sendMessage(SendMessage(chat.id, "Напоминание сохранено "))
    }
  }

  private def showReminders(chat: Chat)(implicit client: Client[IO]): IO[Unit] = {
    if (!messages_map.contains(chat)) {
      sendMessage(SendMessage(chat.id, "Напоминаний пока нет"))
    } else {
      var message = ""
      for (m <- messages_map(chat).sortBy(_.time)) message += "Сообщение \"" + m.text + "\" c ID: " + m.id.toString +
        " будет показано в " + m.time.toString() + "\n"
      sendMessage(SendMessage(chat.id, message))
    }
  }

  private def sendOnTime()(implicit client: Client[IO]): Unit = {
    for (it <- messages_map.iterator) {
      val chat = it._1
      val date_message_arr = it._2
      val toRemoveArr = ArrayBuffer.empty[DateMessage]
      for (date_message <- date_message_arr) {
        if (DateTime.now().isAfter(date_message.time)) {
          toRemoveArr.append(date_message)
          val action = for {
            _ <- sendMessage(SendMessage(chat.id, date_message.text))
            _ <- IO {
            }
          } yield ()
          action.unsafeRunSync()
        }
      }
      for (toRemove <- toRemoveArr) {
        removeReminder(chat, toRemove)
      }
    }
  }

  private def removeReminder(chat: Chat, toRemove: DateMessage)(implicit client: Client[IO]): Unit = {
    messages_map(chat) -= toRemove
    if (messages_map(chat).isEmpty) {
      messages_map.remove(chat)
    }
  }

  private def removeReminderCommand(chat: Chat, text: String)(implicit client: Client[IO]): IO[Unit] = {
    val text_arr = text.split(' ')
    if (text_arr.length < 2 || !text_arr.last.forall(_.isDigit)) {
      sendMessage(SendMessage(chat.id, "Мало аргументов для команды или не передано число"))
    } else {
      val removeId = text_arr.last.toInt
      if (!ids_map(chat).contains(removeId)) {
        sendMessage(SendMessage(chat.id, "Нет напоминания с таким ID:" + removeId.toString))
      } else {
        removeReminder(chat, ids_map(chat)(removeId))
        ids_map(chat) -= removeId
        if (ids_map(chat).isEmpty) {
          ids_map.remove(chat)
        }
        sendMessage(SendMessage(chat.id, "Напоминание удалено"))
      }
    }
  }

  def handleUpdate(update: Update)(implicit client: Client[IO]): IO[Unit] = {
    update.message.text match {
      case Some("/start") => sendMessage(SendMessage(update.message.chat.id, "Добро пожаловать!"))
      case Some("/help") => sendMessage(SendMessage(update.message.chat.id, "/start -- начать \n" +
        "/create reminder_name minutes -- создать напоминание через минуту \n" +
        "/show -- показать напоминания и их ID\n" +
        "/remove id -- удалить напоминание по ID\n"))
      case Some("/show") => showReminders(update.message.chat)
      case Some(text) =>
        text match {
          case command if command.startsWith("/create ") =>
            createReminder(update.message.chat, text)
          case command if command.startsWith("/remove ") =>
            removeReminderCommand(update.message.chat, text)
          case _ => sendMessage(SendMessage(update.message.chat.id, "Неизвестная команда"))
        }
      case _ => sendMessage(SendMessage(update.message.chat.id, "Неизвестная команда"))
    }
  }

  def getUpdates(lastUpdateId: Option[Long])(implicit client: Client[IO]): IO[List[Update]] = {
    sendOnTime()
    val offset = lastUpdateId.getOrElse(0L) + 1
    val requestUri = baseUrl / "getUpdates" +? ("offset", offset)

    val request = Request[IO](Method.GET, requestUri)

    client.expect[TelegramResponse](request).attempt.flatMap {
      case Right(telegramResponse) if telegramResponse.ok =>
        IO.pure(telegramResponse.result)

      case Right(telegramResponse) =>
        IO.raiseError(new Exception(s"Telegram API responded with ok=false: $telegramResponse"))

      case Left(NonFatal(e)) =>
        IO.raiseError(new Exception(s"Failed to fetch updates: ${e.getMessage}", e))
    }
  }
}

