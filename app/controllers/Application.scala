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
    Ok(views.html.index("Your new application is ready."))
  }
  
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
        Redirect(routes.Application.list).flashing("error" -> ("unknown login: " + login))
      }
    }
  }
  
  def insertCustomer = Action { implicit request =>
    val cf = customerForm.bindFromRequest
    cf.fold(
      form => 
        Redirect(routes.Application.newCustomer()).flashing(Flash(form.data) + ("error" -> "Customer not saved")),
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
        Redirect(routes.Application.editCustomer(form.data("login"))).flashing(Flash(form.data) + ("error" -> "Customer not updated")),
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
      Redirect(routes.Application.list).flashing("error" -> ("unknown login: " + login))
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
	      Redirect(routes.Application.list).flashing("error" -> ("unknown login: " + login))
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