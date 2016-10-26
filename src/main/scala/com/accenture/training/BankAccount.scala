package com.accenture.training

import java.util.Date

import scala.Stream
import scala.collection.immutable.Stream.consWrapper
import scala.util.Try

object BankAccountApp extends App {

  type Percentage = Double
  type Amount = Double

  abstract class Transaction {
    def date: Date
    def amount: Amount

    override def toString() = s"${date} - ${getClass.getSimpleName} amount ${amount}"
  }

  // different types of transactions
  case class Deposit(date: Date, amount: Amount) extends Transaction
  case class Withdraw(date: Date, amount: Amount) extends Transaction
  case class Transfer(date: Date, amount: Amount, to: Account) extends Transaction {
    override def toString = s"${super.toString} to ${to.owner.first} ${to.owner.last}"
  }
  case class Interest(date: Date, apr: Percentage) extends Transaction {
    lazy val amount: Amount = ???
  }

  // account holder
  case class Person(first: String, last: String, ssn: String)

  case class Account(opened: Date, owner: Person, transactions: Stream[Transaction]) {

    // sum of balance from transactions
    lazy val currentBalance = transactions.map(t => t match {
      case d: Deposit => d.amount
      case Withdraw(_, _) | Transfer(_, _, _) => t.amount * -1
      case _ => 0.0
    }).sum

    // handle the transaction
    private def tran(t: Transaction): Try[Account] = Try {
      if (t.amount < 0) throw new ArithmeticException("Negative amount for transaction is not allowed")

      t match {
        case d: Deposit => copy(transactions = d #:: transactions)
        case _ =>
          if (currentBalance < t.amount) throw new ArithmeticException(s"Insufficient balance")
          else copy(transactions = t #:: transactions)
      }
    }

    // named functions for transactions
    def deposit(amount: Amount): Try[Account] = tran(Deposit(new Date(), amount))
    def withdraw(amount: Amount): Try[Account] = tran(Withdraw(new Date(), amount))
    def transfer(amount: Amount, to: Account): Tuple2[Try[Account], Try[Account]] = {
      (tran(Transfer(new Date(), amount, to)),
        to.tran(Deposit(new Date(), amount)))
    }

    // calculate interest until the last interest payment
    def calcInterest(apr: Percentage): Amount = {
      def calc(trans: Stream[Transaction], amount: Amount): Amount = {
        if (trans.isEmpty || trans.head.getClass.getSimpleName == Interest.getClass.getSimpleName) {
          amount
        } else {
          calc(trans.tail, (trans.head.amount + amount) * apr / 100)
        }
      }

      calc(transactions, 0.0)
    }

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

  val adeel = Person("Adeel", "Qureshi", "1234")
  val adeelAccount = Account(adeel).deposit(1000)
  val adeelAccount2 = adeelAccount.get.withdraw(500)
  println(adeelAccount2.get)
  println("--")

  val another = Person("Another", "Person", "1111")
  var anotherAccount = Account(another).deposit(5000)
  val (anotherAccount2, adeelAccount3) = anotherAccount.get.transfer(300, adeelAccount2.get)

  println(adeelAccount3.get)
  println(anotherAccount2.get)

  println("Interest = " + adeelAccount3.get.calcInterest(5.0))
}