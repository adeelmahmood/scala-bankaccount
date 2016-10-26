package com.accenture.training.akka

import scala.collection.immutable.Stream.consWrapper

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.actorRef2Scala

case class DepositRequest(account: Account, deposit: Deposit)
case class DepositResponse(account: Account)

class TransactionProcessingActor extends Actor with ActorLogging {

  def receive = {
    case DepositRequest(account, deposit) => {
      log.info(s"received a message to deposit ${deposit.amount}  into ${account.owner.first} account")

      // append the deposit trans and return a new account
      sender ! DepositResponse(account.copy(transactions = deposit #:: account.transactions))
    }

    case _ => log.info("unknown message received")
  }
}