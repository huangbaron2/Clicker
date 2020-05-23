package clicker.game

import akka.actor.Actor
import clicker.{Update, Click, BuyEquipment}
import clicker.{GameState, UpdateGames}
import play.api.libs.json.{JsObject, JsValue, Json}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GameActor(username: String, configuration: String) extends Actor {
  //println("initialConfig", configuration)
  var userName: String = username
  var timeZero = BigDecimal(System.nanoTime())
  var timeDifference = BigDecimal(0)
  var currency: Double = 0.0


  //Information in maps
  var eqId: List[String] = List()
  var eqCost = new ListBuffer[Double]()
  var eqName = new ListBuffer[String]()
  var eqAmount = new ListBuffer[Int]()
  var eqMultipler = new ListBuffer[Double]()
  var eqIncomeClick = new ListBuffer[Int]()
  var eqIncomeSec = new ListBuffer[Int]()
  var eqClickAll = new ListBuffer[Int]()
  var eqSecAll = new ListBuffer[Int]()

  //Make the initial JsMap
  var parsed = Json.parse(configuration)

  def init(): Unit ={
    val LMS: List[Map[String, JsValue]] =  ((parsed \ "equipment")).as[List[Map[String, JsValue]]]
    for (i <- LMS){
      eqId = eqId :+ i("id").toString().replaceAll("\"", "")
    }
    for (id <- eqId) {
      eqCost += ((parsed \ "equipment") (eqId.indexOf(id)) \ "initialCost").as[Double]
      eqName += ((parsed \ "equipment") (eqId.indexOf(id)) \ "name").as[String]
      eqAmount += 0
      eqMultipler += ((parsed \ "equipment") (eqId.indexOf(id)) \ "priceExponent").as[Double]
      eqIncomeClick += ((parsed \ "equipment") (eqId.indexOf(id)) \ "incomePerClick").as[Int]
      eqIncomeSec += ((parsed \ "equipment") (eqId.indexOf(id)) \ "incomePerSecond").as[Int]
      //incomeclick = List[1, 5, 10]
    }
    for (i <- 0 to eqIncomeClick.length - 1) {
      eqClickAll += eqIncomeClick(i) * eqAmount(i)
    }
    for (i <- 0 to eqIncomeSec.length - 1) {
      eqSecAll += eqIncomeSec(i) * eqAmount(i)
    }
    //println("eId", eqId, eqId.indexOf("debugger"))
  }

  def makeJS(): mutable.Map[String, JsValue] = {
    var gameState = scala.collection.mutable.Map[String, JsValue]()
    var listEquips = ListBuffer[mutable.Map[String, JsValue]]()
    gameState("username") = Json.toJson(username)
    gameState("currency") = Json.toJson(0)
    for (i <- 0 to eqId.length - 1) {
      val equipinfo = scala.collection.mutable.Map[String, JsValue]()
      equipinfo("id") = Json.toJson(eqId(i))
      equipinfo("numberOwned") = Json.toJson(0)
      equipinfo("cost") = Json.toJson(eqCost(i))
      listEquips += equipinfo
    }
    gameState("equipment") = Json.toJson(listEquips)
    //println("FirstJS", gameState)
    gameState
  }

  def editState(eqCost: ListBuffer[Double],  eqAmount: ListBuffer[Int], eqMultiplier: ListBuffer[Double]): mutable.Map[String, JsValue] = {
    var gameState = scala.collection.mutable.Map[String, JsValue]()
    var listEquips = ListBuffer[mutable.Map[String, JsValue]]()
    gameState("username") = Json.toJson(username)
    gameState("currency") = Json.toJson(currency)
    for (i <- 0 to eqId.length - 1) {
      val equipinfo = scala.collection.mutable.Map[String, JsValue]()
      equipinfo("id") = Json.toJson(eqId(i))
      equipinfo("numberOwned") = Json.toJson(eqAmount(i))
      equipinfo("cost") = Json.toJson(eqCost(i))
      listEquips += equipinfo
    }
    gameState("equipment") = Json.toJson(listEquips)
    //println("EditedJS", gameState)
    gameState
  }

  init()
  makeJS()

  override def receive: Receive = {
    case Click =>
      for (i <- 0 to eqIncomeClick.length - 1) {
        eqClickAll(i) = eqIncomeClick(i) * eqAmount(i)
      }
      currency += 1
      currency += eqClickAll.sum

    case Update =>
      for (i <- 0 to eqIncomeSec.length - 1) {
        eqSecAll(i) = eqIncomeSec(i) * eqAmount(i)
      }
      val newState: JsValue = Json.toJson(editState(eqCost, eqAmount, eqMultipler))
      timeDifference = System.nanoTime() - timeZero
      timeZero = BigDecimal(System.nanoTime())
      val timeInSeconds = timeDifference / 1000000000
      currency += timeInSeconds.toDouble * eqSecAll.sum
      sender() ! GameState(Json.stringify(newState))

    case eqBuy: BuyEquipment =>
      val index = eqId.indexOf(eqBuy.equipmentId)
      if (currency >= eqCost(index)){
        eqAmount(index) = eqAmount(index) + 1
        currency -= eqCost(index)
        eqCost(index) = eqCost(index) * eqMultipler(index)
      }
  }


}

/*
      if (jsState("currency").as[Int] > eqCost(eqBought.equipmentId)){
        this.currency -= eqCost(eqBought.equipmentId)
        eqAmount(eqBought.equipmentId) = eqAmount(eqBought.equipmentId) + 1
        eqCost(eqBought.equipmentId) = eqCost(eqBought.equipmentId) * eqMultipler(eqBought.equipmentId)
        println(eqAmount)
 */