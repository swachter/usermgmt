package controllers

import play.api._
import play.api.mvc._
import models.Customer
import models.Customers
import play.api.data.Form
import play.api.data.Forms._
import models.CustomerTable
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import views.html.helper.FieldConstructor
import views.html.helper.FieldElements

object Application extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.index())
  }
  
  def navBar = Action { implicit request => {
    val qs = request.queryString
    def searchText = qs.get("text").map(_.mkString).filter(!_.isEmpty())
    val response = if (qs.contains("show")) {
      searchText.map(s => Redirect(routes.Application.showCustomer(s))).getOrElse(Redirect(routes.Application.index).flashing("error" -> "fehlende Eingabe"))
    } else if (qs.contains("edit")) {
      searchText.map(s => Redirect(routes.Application.editCustomer(s))).getOrElse(Redirect(routes.Application.index).flashing("error" -> "fehlende Eingabe"))
    } else if (qs.contains("new")) {
      Redirect(routes.Application.newCustomer)
    } else {
      // search
      Redirect(routes.Application.list)
    }
    response.withSession(request.session + ("search" -> searchText.getOrElse("")))
  }}
  
  def list = Action { implicit request =>
    val customers = Customers.list
    Ok(views.html.list(customers))
  }
  
  def newCustomer = Action { implicit request => 
    val form = if (flash.get("error").isDefined) customerForm.bind(flash.data) else customerForm
    Ok(views.html.customerForm(form, true))  
  }
  
  def editCustomer(login: String) = Action { implicit request =>
    if (flash.get("error").isDefined) {
      val form = customerForm.bind(flash.data)
      Ok(views.html.customerForm(form, false))  
    } else {
      val customer = Customers.findByLogin(login)
      if (customer.isDefined) {
        val form = customerForm.fill(customer.get)
        Ok(views.html.customerForm(form, false))  
      } else {
        Redirect(routes.Application.index).flashing("error" -> ("unbekannter Login: " + login))
      }
    }
  }
  
  def insertCustomer = Action { implicit request =>
    val cf = customerForm.bindFromRequest
    cf.fold(
      form => 
        Redirect(routes.Application.newCustomer()).flashing(Flash(form.data) + ("error" -> "Login wurde nicht gespeichert")),
      customer => {
        Try {
          Customers.insert(customer)
          Redirect(routes.Application.showCustomer(customer.login)).flashing("success" -> "Login wurde gespeichert")
        } match {
          case Success(r) => r
          case Failure(t) => Redirect(routes.Application.newCustomer()).flashing(Flash(cf.data) + ("error" -> ("Login wurde nicht gespeichert: " + t.getMessage())))
        }
      }
    )
  }
  
  def updateCustomer = Action { implicit request =>
    val cf = customerForm.bindFromRequest
    cf.fold(
      form => 
        Redirect(routes.Application.editCustomer(form.data("login"))).flashing(Flash(form.data) + ("error" -> "Login wurde nicht aktualisiert")),
      customer => {
        Customers.update(customer)
        Redirect(routes.Application.showCustomer(customer.login))
      }
    )
  }
  
  def showCustomer(login: String) = Action { implicit request =>
    val customer = Customers.findByLogin(login)
    if (customer.isDefined) {
      Ok(views.html.showCustomer(customer.get))
    } else {
      Redirect(routes.Application.index).flashing("error" -> ("unbekannter Login: " + login))
    }
  }
  
  def modifyPassword(login: String, setNotClear: Boolean, adminNotUser: Boolean) = Action { implicit request =>
    val password = Customers.modifyPassword(login, setNotClear, adminNotUser)
    Redirect(routes.Application.showCustomer(login)).flashing("success" -> s"""${if (adminNotUser) "Administrator" else "Benutzer"}-Paßwort wurde ${if (setNotClear) "gesetzt; neuer Wert: " + password.get else "gelöscht"}""")
  }
  
  def deleteCustomer(login: String, confirmed: Boolean) = Action { implicit request =>
    if (confirmed) {
      Customers.delete(login)
      Redirect(routes.Application.index)
    } else {
      Redirect(routes.Application.showCustomer(login)).flashing("confirmeDelete" -> "true")
    }
  }
  
  val customerForm = Form(
    mapping(
      "login" -> nonEmptyText(1),
      "validFrom" -> sqlDate,
      "validUntil" -> sqlDate,
      "numUsers" -> number,
      "staemme" -> optional(text)
    )(
        (login, validFrom, validUntil, numUsers, staemme) => Customer(login, None, None, validFrom, validUntil, numUsers, staemme)
    )(
        c => Some((c.login, c.validFrom, c.validUntil, c.numUsers, c.staemme))
    )
  )
  
  implicit val bootstrap3FieldConstructor = new FieldConstructor {
    def apply(elements: FieldElements) = views.html.helper.bootstrap3.fieldConstructorTemplate(elements)
  } 
  
 
}