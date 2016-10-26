package com.accenture.training.akka

import java.util.Date

import scala.concurrent.duration.DurationInt

import akka.actor.ActorSystem
import akka.actor.Props
import akka.dispatch.ExecutionContexts.global
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Await

object BankAccountApp extends App {

  implicit val ec = global
  implicit val timeout = Timeout(25 seconds)

  val system = ActorSystem("System")
  val actor = system.actorOf(Props(new TransactionProcessingActor()))

  val adeelAccount = Account(Person("Adeel", "Qureshi"))

  val future = actor ? DepositRequest(adeelAccount, Deposit(new Date(), 100))
  val adeelAccount2 = Await.result(future, timeout.duration).asInstanceOf[DepositResponse]

  system terminate

  println(adeelAccount2.account)
}