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
package snippet {

import model._

import _root_.net.liftweb._
import http._
import mapper._
import S._
import SHtml._

import common._
import util._
import Helpers._

import _root_.scala.xml.{NodeSeq, Text, Group}
import _root_.java.util.Locale

class Misc {
  private object selectedUser extends RequestVar[Box[User]](Empty)

  /**
   * Get the XHTML containing a list of users
   */
  def users: NodeSeq = {
    User.find() match {
      case Empty => User.create.firstName("Archer").lastName("Dog").email("archer@dogfood.com").password("mypassword").save
      case _ =>
    }
    // the header
    <tr>{User.htmlHeaders}<th>Edit</th><th>Delete</th></tr> ::
    // get and display each of the users
    User.findAll(OrderBy(User.id, Ascending)).flatMap(u => <tr>{u.htmlLine}
        <td>{link("/simple/edit", () => selectedUser(Full(u)), Text("Edit"))}</td>
        <td>{link("/simple/delete", () => selectedUser(Full(u)), Text("Delete"))}</td>
                                                           </tr>)
  }

  /**
   * Confirm deleting a user
   */
  def confirmDelete(xhtml: Group): NodeSeq = {
    (for (user <- selectedUser.is) // find the user
     yield {
        def deleteUser() {
          notice("User "+(user.firstName+" "+user.lastName)+" deleted")
          user.delete_!
          redirectTo("/simple/index.html")
        }

        // bind the incoming XHTML to a "delete" button.
        // when the delete button is pressed, call the "deleteUser"
        // function (which is a closure and bound the "user" object
        // in the current content)
        bind("xmp", xhtml, "username" -> (user.firstName.is+" "+user.lastName.is),
             "delete" -> submit("Delete", deleteUser _))

        // if the was no ID or the user couldn't be found,
        // display an error and redirect
      }) openOr {error("User not found"); redirectTo("/simple/index.html")}
  }

  // called when the form is submitted
  private def saveUser(user: User) = user.validate match {
    // no validation errors, save the user, and go
    // back to the "list" page
    case Nil => user.save; redirectTo("/simple/index.html")

      // oops... validation errors
      // display the errors and make sure our selected user is still the same
    case x => error(x); selectedUser(Full(user))
  }

  /**
   * Add a user
   */
  def add(xhtml: Group): NodeSeq =
  selectedUser.is.openOr(new User).toForm(Empty, saveUser _) ++ <tr>
    <td><a href="/simple/index.html">Cancel</a></td>
    <td><input type="submit" value="Create"/></td>
                                                                </tr>

  /**
   * Edit a user
   */
  def edit(xhtml: Group): NodeSeq =
  selectedUser.map(_.
                   // get the form data for the user and when the form
                   // is submitted, call the passed function.
                   // That means, when the user submits the form,
                   // the fields that were typed into will be populated into
                   // "user" and "saveUser" will be called.  The
                   // form fields are bound to the model's fields by this
                   // call.
                   toForm(Empty, saveUser _) ++ <tr>
      <td><a href="/simple/index.html">Cancel</a></td>
      <td><input type="submit" value="Save"/></td>
                                                </tr>

                   // bail out if the ID is not supplied or the user's not found
  ) openOr {error("User not found"); redirectTo("/simple/index.html")}

  // the request-local variable that hold the file parameter
  private object theUpload extends RequestVar[Box[FileParamHolder]](Empty)

  /**
   * Bind the appropriate XHTML to the form
   */
  def upload(xhtml: Group): NodeSeq =
  if (S.get_?) bind("ul", chooseTemplate("choose", "get", xhtml),
                    "file_upload" -> fileUpload(ul => theUpload(Full(ul))))
  else bind("ul", chooseTemplate("choose", "post", xhtml),
            "file_name" -> theUpload.is.map(v => Text(v.fileName)),
            "mime_type" -> theUpload.is.map(v => Box.legacyNullTest(v.mimeType).map(Text).openOr(Text("No mime type supplied"))), // Text(v.mimeType)),
            "length" -> theUpload.is.map(v => Text(v.file.length.toString)),
            "md5" -> theUpload.is.map(v => Text(hexEncode(md5(v.file))))
  );


  def lang = {
    "#lang" #> locale.getDisplayLanguage(locale) &
    "#select" #> SHtml.selectObj(locales.map(lo => (lo, lo.getDisplayName)),
                                 definedLocale, setLocale)
  }

  private def locales =
  Locale.getAvailableLocales.toList.sort(_.getDisplayName < _.getDisplayName)

  private def setLocale(loc: Locale) = definedLocale(Full(loc))
}

object definedLocale extends SessionVar[Box[Locale]](Empty)


}
}
}
