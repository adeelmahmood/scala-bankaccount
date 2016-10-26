package com.accenture.training.akka

import java.util.Date

import scala.Stream

abstract class Transaction {
  def date: Date
  def amount: Double

  override def toString() = s"${date} - ${getClass.getSimpleName} amount ${amount}"
}

case class Deposit(date: Date, amount: Double) extends Transaction
case class Withdraw(date: Date, amount: Double) extends Transaction

case class Person(first: String, last: String)

case class Account(opened: Date, owner: Person, transactions: Stream[Transaction]) {

  lazy val currentBalance = transactions.map(t => t match {
    case d: Deposit => d.amount
    case _ => t.amount * -1
  }).sum

  override def toString() = {
    val t = transactions.map(t => s"${t}") mkString ("\n")
    s"""
      |Account for ${owner.first} ${owner.last}
      |Current balance = ${currentBalance}
      |Transaction history:
      |${t}
      """.stripMargin
  }
}

object Account {
  def apply(owner: Person) = new Account(new Date(), owner, Stream[Transaction]())
}