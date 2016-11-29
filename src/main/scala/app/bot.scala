package app

import akka.actor.ActorSystem
import slack.models.{ChannelJoined, Message, MessageChanged}
import slack.rtm.SlackRtmClient
import utils.storage

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

/**
  * Created by nikos on 12/11/2016.
  */
object bot extends App {

  val testing = false

  val (token, admin, channel) = storage.getCredentials

  implicit val system = ActorSystem("qb_pong")
  implicit val ec = system.dispatcher

  val client = SlackRtmClient(token)
  val selfId = client.state.self.id
  def getSystem = system

  storage.read()

  client.onMessage { message => onMessage(message) }

  def sendMessageChannel(channelName: String, text: String): Unit = {
    val channelId: String =
      if (testing) idToChannel(nameToId(admin).get)
      else bot.client.state.channels.find(_.name == channelName).get.id

    client.sendMessage(channelId, if (testing) "*TO CHANNEL*\n" + text else text)
  }

  def sendMessage(user: String, text: String): Unit = {
    val id =
      if (testing) idToChannel(nameToId(admin).get)
      else {
        if (!imExists(user)) client.apiClient.openIm(user)
        else idToChannel(user)
      }

    client.sendMessage(id, if (testing) "*TO " + fromId(user).get + "*\n" + text else text)

    //    if (!imExists(user)) client.sendMessage(idToChannel(user), s"@${fromId(user).get} please open a private chat with @qb-pong so I can ask there for confirmations. Less spam for everyone!")
  }

  def sendMessage(user: String, text: String, delay: FiniteDuration): Unit = {
    system.scheduler.scheduleOnce(delay) {
      sendMessage(user, text)
    }
  }

  def onMessage(message: Message) = {
    if (message.user != selfId) {
      val command = Cmd(message)
      command.execute match {
        case Success(reply) =>
          if (reply != "") client.sendMessage(command.channel, reply)
        case Failure(ex) => ex match {
          case e: EmptyReply =>
          case _ => ex.printStackTrace()
        }
      }
    }
  }

  client.onEvent {
    case m: MessageChanged =>
      onMessage(Message(m.ts, m.channel, m.message.user, m.message.text, None))
    case ev: ChannelJoined =>
      client.sendMessage(ev.channel.id, "Hello, I'm the QB ping pong bot! :table_tennis_paddle_and_ball:")
      client.sendMessage(ev.channel.id, Cmd.usage)
    case _ =>
  }

  def nameToId(name: String): Option[String] = client.state.users.find(_.name.toLowerCase == name.toLowerCase).map(_.id)

  def fromName(name: String): Option[String] = {
    val user = client.state.users.find(_.name == name)
    if (user.isDefined) {
      val id = user.get.id
      return Some(client.state.ims.filter(_.user == id).head.id)
    }

    val group = client.state.groups.find(_.name == name)
    if (group.isDefined) return Some(group.get.id)

    val c = client.state.channels.find(_.name == name)
    if (c.isDefined) return Some(c.get.id)

    None
  }

  def idToChannel(id: String): String = {
    val name = client.state.ims.find(_.user == id).map(_.id)
    if (name.isDefined) name.get
    else client.state.channels.find(_.name == bot.getChannel).get.id
  }

  def imExists(id: String): Boolean = client.state.ims.exists(_.user == id)

  def fromId(id: String): Option[String] = client.state.users.find(_.id == id.replaceAll("[<>@]", "")).map(_.name)

	def getChannel: String = channel
  //  def fromId(id: String): Option[String] = {
  //    val i = client.state.ims.find(_.id == id)
  //    if (i.isDefined) {
  //      val user = i.get.user
  //      return Some(client.state.users.filter(_.id == user).head.name)
  //    }
  //
  //    val g = client.state.groups.find(_.id == id)
  //    if (g.isDefined) return Some(g.get.name)
  //
  //    val c = client.state.channels.find(_.id == id)
  //    if (c.isDefined) return Some(c.get.name)
  //
  //    None
  //  }
}