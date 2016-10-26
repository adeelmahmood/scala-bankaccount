package com.accenture.training

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._

object AsyncDemo extends App {
  val start = System.currentTimeMillis()
  def info(msg: String) = printf("%.2f: %s\n",
    (System.currentTimeMillis() - start) / 1000.0, msg)

  case class Dish(name: String) {
    def +(other: Dish) = Dish(s"$name with ${other.name}")
  }

  def cook(dish: String): Dish = {
    Thread.sleep(1000)
    info(s"$dish cooked")
    Dish(dish)
  }

  def serve(dish: Dish): Unit = {
    info(s"Here's your ${dish.name}, sir!")
  }

  var fsteak = Future { cook("steak") }
  var fpotatoes = Future { cook("potatoes") }

  val fs = for {
    s <- fsteak
    p <- fpotatoes
  } yield {
    serve(s + p)
  }

  Await.result(fs, 10.seconds)
}