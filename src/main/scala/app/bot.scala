package app

import akka.actor.ActorSystem
import slack.api.BlockingSlackApiClient
import slack.models.{ChannelJoined, Message, MessageChanged}
import slack.rtm.SlackRtmClient
import tournament.tournamentWatcher
import utils.storage

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

/**
  * Created by nikos on 12/11/2016.
  */
object bot extends App {

  val testing = true

  val (token, admin, channel) = storage.getCredentials
  implicit val system = ActorSystem("qb_pong")
  implicit val ec = system.dispatcher

  val apiClient = BlockingSlackApiClient(token)
  val client = SlackRtmClient(token)
  val selfId = client.state.self.id
  def getSystem: ActorSystem = system

  storage.read()
	tournamentWatcher.startWatching()

  client.onMessage { message => onMessage(message) }

	client.onEvent {
		case m: MessageChanged =>
			onMessage(Message(m.ts, m.channel, m.message.user, m.message.text, None))
		case ev: ChannelJoined =>
			client.sendMessage(ev.channel.id, "Hello, I'm the QB ping pong bot! :table_tennis_paddle_and_ball:")
			client.sendMessage(ev.channel.id, Cmd.usage)
		case _ =>
	}

  def sendMessageChannel(channelName: Option[String] = None, text: String): Unit = {
    val channelId: String =
      if (testing) idToChannel(nameToId(admin).get)
      else if (channelName.isDefined) bot.client.state.channels.find(_.name == channelName.get).get.id
      else bot.client.state.channels.find(_.name == channel).get.id
    client.sendMessage(channelId, if (testing) "*TO CHANNEL*\n" + text else text)
  }

  def sendMessage(user: String, text: String): Unit = {
    val id =
      if (testing) idToChannel(nameToId(admin).get)
      else {
        if (!imExists(user)) apiClient.openIm(user)
        else idToChannel(user)
      }
    client.sendMessage(id, if (testing) "*TO " + fromId(user).get + "*\n" + text else text)
  }

  def sendMessage(user: String, text: String, delay: FiniteDuration): Unit = system.scheduler.scheduleOnce(delay) {
		sendMessage(user, text)
	}

  def onMessage(message: Message): Unit = {
    if (message.user != selfId) {
      val command = Cmd(message)
      command.execute match {
        case Success(reply) =>
          if (reply != "") client.sendMessage(command.channel, reply)
        case Failure(ex) => ex match {
          case _: EmptyReply =>
          case _ => ex.printStackTrace()
        }
      }
    }
  }

	def imExists(id: String): Boolean = client.state.ims.exists(_.user == id)
	def nameToId(name: String): Option[String] = client.state.users.find(_.name.toLowerCase == name.toLowerCase).map(_.id)
  def fromId(id: String): Option[String] = client.state.users.find(_.id == id.replaceAll("[<>@]", "")).map(_.name)
	def getChannel: String = channel
	def idToChannel(id: String): String = {
		val name = client.state.ims.find(_.user == id).map(_.id)
		if (name.isDefined) name.get
		else client.state.channels.find(_.name == bot.getChannel).get.id
	}
}