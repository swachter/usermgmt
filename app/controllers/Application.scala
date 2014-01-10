package controllers

import play.api._
import play.api.mvc._
import models.Customer
import models.Customers
import play.api.data.Form
import play.api.data.Forms._
import models.CustomerTable

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
        Customers.insert(customer)
        Redirect(routes.Application.showCustomer(customer.login))
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
    Redirect(routes.Application.showCustomer(login)).flashing("success" -> s"""${if (setNotClear) "set" else "cleared"} ${if (adminNotUser) "admin" else "user"} password${if (setNotClear) "to " + password.get else ""}""")
  }
  
  def deleteCustomer(login: String, confirm: Option[Boolean]) = Action { implicit request =>
    if (confirm.getOrElse(false)) {
      Customers.delete(login)
      Redirect(routes.Application.list)
    } else if (confirm.isDefined) {
      Redirect(routes.Application.showCustomer(login))
    } else {
	    val customer = Customers.findByLogin(login)
	    if (customer.isDefined) {
	      Ok(views.html.deleteCustomer(customer.get))
	    } else {
	      Redirect(routes.Application.list).flashing("error" -> ("unbekannter Login: " + login))
	    }
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
 
}