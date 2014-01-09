package models

import play.api.Play.current
import play.api.db.slick.Config.driver.simple._
import play.api.db.slick.DB
import java.sql.Date

case class Customer(login: String, pwdHash: Option[String], adminPwdHash: Option[String], validFrom: Date, validUntil: Date, numUsers: Int, staemme: Option[String])

object CustomerTable extends Table[Customer]("CUSTOMER") {
  
  def login = column[String]("LOGIN")
  def pwdHash = column[String]("PWD_HASH")
  def adminPwdHash = column[String]("ADMIN_PWD_HASH")
  def validFrom = column[Date]("VALID_FROM")
  def validUntil = column[Date]("VALID_UNTIL")
  def numUsers = column[Int]("NUM_USERS")
  def staemme = column[String]("STAEMME")
  
  def * = login ~ pwdHash.? ~ adminPwdHash.? ~ validFrom ~ validUntil ~ numUsers ~ staemme.? <> (Customer.apply _, Customer.unapply _)
  
}

object Customers {
  def list(): List[Customer] = DB.withSession { implicit session: Session =>
    Query(CustomerTable).list
  }
  
  def insert(customer: Customer): Unit = DB.withSession { implicit session: Session =>
    CustomerTable.insert(customer)
  }
  
  def update(customer: Customer): Unit = DB.withSession { implicit session: Session =>
    val projection = Query(CustomerTable).filter(_.login === customer.login).map(c => c.validFrom ~ c.validUntil ~ c.numUsers ~ c.staemme.?)
    projection.update((customer.validFrom, customer.validUntil, customer.numUsers, customer.staemme))
  }
  
  def findByLogin(login: String): Option[Customer] = DB.withSession { implicit session: Session =>
    Query(CustomerTable).filter(_.login === login).list.headOption
  } 
  
  def modifyPassword(login: String, setNotClear: Boolean, adminNotUser: Boolean): Option[String] = DB.withSession { implicit session: Session =>
    val value = if (setNotClear) Some("abc") else None
    val column = (c: CustomerTable.type) => if (adminNotUser) c.adminPwdHash.? else c.pwdHash.?
    val projection = Query(CustomerTable).filter(_.login === login).map(column)
    projection.update((value))
    value
  }
  
  def delete(login: String): Unit = DB.withSession { implicit session: Session =>
    Query(CustomerTable).filter(_.login === login).delete
  }
}
