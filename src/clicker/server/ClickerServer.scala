package clicker.server

import clicker.game.GameActor
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import clicker._
import com.corundumstudio.socketio.listener.DataListener
import com.corundumstudio.socketio.{AckRequest, Configuration, SocketIOClient, SocketIOServer}

import scala.collection.mutable
import scala.io.Source


class ClickerServer(val configuration: String) extends Actor {
  var actorsList: List[ActorRef] = List()
  var socketToConfig = mutable.Map[SocketIOClient, ActorRef]()
  var configToSocket = mutable.Map[ActorRef, SocketIOClient]()

  val config: Configuration = new Configuration {
    setHostname("localhost")
    setPort(8080)
  }

  val server: SocketIOServer = new SocketIOServer(config)
  server.start()

  server.addEventListener("startGame", classOf[String], new createActor(this, configuration))
  server.addEventListener("click", classOf[Nothing], new clicked(this))
  server.addEventListener("buy", classOf[String], new bought(this))


  override def receive: Receive = {
    case UpdateGames =>
      actorsList.foreach(_ ! Update)

    case gs: GameState =>
      (configToSocket(sender())).sendEvent("gameState", gs.gameState)
  }


  override def postStop(): Unit = {
    println("stopping server")
    server.stop()
  }
}

class createActor(server: ClickerServer, configuration: String) extends DataListener[String] {
  override def onData(socket: SocketIOClient, data: String, ackRequest: AckRequest): Unit = {
    val system = ActorSystem("Clicks")
    val actor = system.actorOf(Props(classOf[GameActor], data, configuration))
    server.actorsList = server.actorsList :+ actor
    server.socketToConfig += (socket -> actor)
    server.configToSocket += (actor -> socket)
    socket.sendEvent("initialize", configuration)
  }
}

class clicked(server: ClickerServer) extends DataListener[Nothing] {
  override def onData(socket: SocketIOClient, data: Nothing, ackRequest: AckRequest): Unit = {
    val sendey: ActorRef = server.socketToConfig(socket)
    sendey ! Click
  }
}

class bought(server: ClickerServer) extends DataListener[String] {
  override def onData(socket: SocketIOClient, data: String, ackRequest: AckRequest): Unit = {
    val sendey: ActorRef = server.socketToConfig(socket)
    sendey ! BuyEquipment(data)
  }
}

object ClickerServer {

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem()
    import actorSystem.dispatcher

    import scala.concurrent.duration._

    val configuration: String = Source.fromFile("codeConfig.json").mkString

    val server = actorSystem.actorOf(Props(classOf[ClickerServer], configuration))

    actorSystem.scheduler.schedule(0.milliseconds, 100.milliseconds, server, UpdateGames)
  }
}
