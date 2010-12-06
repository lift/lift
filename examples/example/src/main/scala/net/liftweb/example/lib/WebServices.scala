/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package example {
package lib {

import _root_.net.liftweb._
import http._
import http.rest._
import common._
import json._
import util._
import _root_.net.liftweb.example.model._

object WebServices extends RestHelper {
  // a JSON-able class that holds a User
  case class UserInfo(firstName: String, lastName: String,
                      email: String) {
    def toXml = <user firstname={firstName}
    lastName={lastName} email={email}/>

    def toJson = Extraction.decompose(this)
  }
  
  // a JSON-able class that holds all the users
  case class AllUsers(users: List[UserInfo]) {
    def toJson = Extraction.decompose(this)
    def toXml = <users>{users.map(_.toXml)}</users>
  }

  // define a REST handler for an XML request
  serve {
    case "webservices" :: "all_users" :: _ XmlGet _ =>
      AllUsers(User.findAll()).toXml
  }

  // define a REST handler for a JSON reqest
  serve {
    case "webservices" :: "all_users" :: _ JsonGet _ =>
      AllUsers(User.findAll()).toJson
  }


  /*
   * While many on the Web use GET requests in this way, a client shouldn't
   * be given the expectation of resource state change or creation
   * through a GET. GET should be idempotent and safe. This doesn't mean
   * that a service couldn't create or modify state as as result
   * (e.g. logging, counting the number of requests, creating business
   * objects). It's just that any such state-related operations should
   * not be visible through GET. In the above example, it is implied
   * that a client could send a GET request in order to create a user.
   *
   * AKA -- don't do it this way in the real world, this is an example
   * of using Scala's guards
   */

  serveJx {
    case Req("webservices" :: "add_user" :: _, _, rt) if rt.post_? || rt.get_? =>
      addUser()
  } { // How do we convert a UserInfo to either XML or JSON?
    case (JsonSelect, u, _) => u.toJson
    case (XmlSelect, u, _) => u.toXml
  }
  
  // a couple of helpful conversion rules
  implicit def userToInfo(u: User): UserInfo = 
    UserInfo(u.firstName, u.lastName, u.email)

  implicit def uLstToInfo(ul: List[User]): List[UserInfo] =
    ul.map(userToInfo)

  // extract the parameters, create a user
  // return the appropriate response
  def addUser(): Box[UserInfo] =
    for {
      firstname <- S.param("firstname") ?~ "firstname parameter missing" ~> 400
      lastname <- S.param("lastname") ?~ "lastname parameter missing"
      email <- S.param("email") ?~ "email parameter missing"
    } yield {
      val u = User.create.firstName(firstname).
      lastName(lastname).email(email)

      S.param("password") foreach u.password.set

      u.saveMe
    }
}

}
}
}
