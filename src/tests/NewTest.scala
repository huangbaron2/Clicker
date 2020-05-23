package tests

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import clicker._
import clicker.game.GameActor
import clicker.server.ClickerServer
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.duration._
import scala.io.Source


class NewTest extends TestKit(ActorSystem("Test2"))
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  val EPSILON: Double = 0.000001

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A Clicker actor" must {
    "react to clicks and equipment purchases" in {

      val configuration: String = Source.fromFile("codeConfig.json").mkString
      val gameActor = system.actorOf(Props(classOf[ClickerServer], configuration))

    }
  }
}