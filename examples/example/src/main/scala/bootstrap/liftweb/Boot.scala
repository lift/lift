/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package bootstrap.liftweb

import _root_.net.liftweb._
import common.{Box, Full, Empty, Failure, Loggable}
import util.{Helpers, Log, NamedPF, Props}
import http._
import actor._
import provider._
import sitemap._
import Helpers._

import example._
import widgets.autocomplete._
import comet._
import model._
import lib._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}

import _root_.java.sql.{Connection, DriverManager}
import snippet._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    LiftRules.addToPackages("net.liftweb.example")

    /**
     * We're doing this as HTML5
     */
    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))

    LiftRules.localeCalculator = r => definedLocale.openOr(LiftRules.defaultLocaleCalculator(r))

    if (!Props.inGAE) {
      // No DB stuff in GAE
      Schemifier.schemify(true, Schemifier.infoF _, User, WikiEntry, Person)
    }

    LiftRules.dispatch.append(WebServices)

    StatelessJson.init()

    XmlServer.init()

    LiftRules.statelessDispatchTable.append {
      case r@Req("stateless" :: _, "", GetRequest) => StatelessHtml.render(r) _
    }

    LiftRules.dispatch.prepend(NamedPF("Login Validation") {
      case Req("login" :: page, "", _)
      if !LoginStuff.is && page.head != "validate" =>
        () => Full(RedirectResponse("/login/validate"))
    })

    LiftRules.snippetDispatch.append(NamedPF("Template")
                                     (Map("Template" -> Template,
                                          "AllJson" -> AllJson)))

    /*
    LiftRules.snippetDispatch.append {
      case "MyWizard" => MyWizard
      case "WizardChallenge" => WizardChallenge
      case "ScreenForm" => PersonScreen
    }
    */

    SessionMaster.sessionCheckFuncs = SessionMaster.sessionCheckFuncs :::
    List(SessionChecker)

    // Uncomment the lines below to see how
    // a Lift app looks when it's stateless
    /*
    LiftRules.statelessTest.prepend {
      case _ => true
    }*/

    LiftRules.snippetDispatch.append(Map("runtime_stats" -> RuntimeStats))

    /*
    * Show the spinny image when an Ajax call starts
    */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
    * Make the spinny image go away when it ends
    */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.cometCreation.append {
      case CometCreationInfo("Clock",
                             name,
                             defaultXml,
                             attributes,
			     session) =>
			       new ExampleClock(session, Full("Clock"),
						name, defaultXml, attributes)
      
    }

    LiftSession.onBeginServicing = RequestLogger.beginServicing _ ::
    LiftSession.onBeginServicing

    LiftSession.onEndServicing = RequestLogger.endServicing _ ::
    LiftSession.onEndServicing

    LiftRules.setSiteMapFunc(MenuInfo.sitemap)

    ThingBuilder.boot()

    AutoComplete.init()

    // Dump information about session every 10 minutes
    SessionMaster.sessionWatchers = SessionInfoDumper ::
    SessionMaster.sessionWatchers

    // Dump browser information each time a new connection is made
    LiftSession.onBeginServicing = BrowserLogger.haveSeenYou _ :: LiftSession.onBeginServicing

  }

  private def makeUtf8(req: HTTPRequest): Unit = {req.setCharacterEncoding("UTF-8")}
}

object RequestLogger extends Loggable {
  object startTime extends RequestVar(0L)

  def beginServicing(session: LiftSession, req: Req) {
    startTime(millis)
  }

  def endServicing(session: LiftSession, req: Req,
                   response: Box[LiftResponse]) {
    val delta = millis - startTime.is
    logger.info("At " + (timeNow) + " Serviced " + req.uri + " in " + (delta) + "ms " + (
      response.map(r => " Headers: " + r.toResponse.headers) openOr ""
    ))
  }
}

object MenuInfo {
  import Loc._

  lazy val noGAE = Unless(() => Props.inGAE, "Disabled for GAE")

  def sitemap() = SiteMap(
    Menu("Home") / "index",
    Menu("Interactive Stuff") / "interactive" submenus(
      Menu("Comet Chat") / "chat" >> noGAE,
      Menu("Ajax Samples") / "ajax",
      Menu("Ajax Form") / "ajax-form",
      Menu("Modal Dialog") / "rhodeisland",
      Menu("JSON Messaging") / "json",
      Menu("Stateless JSON Messaging") / "stateless_json",
      Menu("More JSON") / "json_more",
      Menu("Ajax and Forms") / "form_ajax") ,
    Menu("Persistence") / "persistence" >> noGAE submenus (
      Menu("XML Fun") / "xml_fun" >> noGAE,
      Menu("Database") / "database" >> noGAE,
      Menu(Loc("simple", Link(List("simple"), true, "/simple/index"), "Simple Forms", noGAE)),
      Menu("Templates") / "template" >> noGAE),
    Menu("Templating") / "templating" / "index" submenus(
      Menu("Surround") / "templating" / "surround",
      Menu("Embed") / "templating" / "embed",
      Menu("Evalutation Order") / "templating" / "eval_order",
      Menu("Select <div>s") / "templating" / "selectomatic",
      Menu("Simple Wizard") / "simple_wizard",
      Menu("Lazy Loading") / "lazy",
      Menu("Parallel Snippets") / "parallel",
      Menu("<head/> tag") / "templating"/ "head"),
    Menu("Web Services") / "ws" >> noGAE,
    Menu("Localization") / "lang",
    Menu("Menus") / "menu" / "index" submenus(
      Menu("First Submenu") / "menu" / "one",
      Menu("Second Submenu (has more)") / "menu" / "two" submenus(
        Menu("First (2) Submenu") / "menu" / "two_one",
        Menu("Second (2) Submenu") / "menu" / "two_two"),
      Menu("Third Submenu") / "menu" / "three",
      Menu("Forth Submenu") / "menu" / "four"),
    Menu(WikiStuff),
    Menu("Misc code") / "misc" submenus(
      Menu("Long Time") / "longtime",
      Menu("Number Guessing") / "guess",
      Menu("Wizard") / "wiz",
      Menu("Wizard Challenge") / "wiz2",
      Menu("Simple Screen") / "simple_screen",
      Menu("Variable Screen") / "variable_screen",
      Menu("Arc Challenge #1") / "arc",
      Menu("Simple Wiring") / "simple_wiring",
      Menu("Wiring Invoice") / "invoice_wiring",
      Menu("File Upload") / "file_upload",
      Menu(Loc("login", Link(List("login"), true, "/login/index"),
               <xml:group>Requiring Login<strike>SiteMap</strike> </xml:group>)),
      Menu("Counting") / "count"),
    Menu(Loc("lift", ExtLink("http://liftweb.net"),
             <xml:group> <i>Lift</i> project home</xml:group>)),
    Menu(Loc("src", ExtLink("https://github.com/lift/lift/tree/master/examples/example"),
             "Source code for this site")))
}

/**
* Database connection calculation
*/
object DBVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4

  private lazy val chooseDriver = Props.mode match {
    case Props.RunModes.Production => "org.apache.derby.jdbc.EmbeddedDriver"
    case _ => "org.h2.Driver"
  }


  private lazy val chooseURL = Props.mode match {
    case Props.RunModes.Production => "jdbc:derby:lift_example;create=true"
    case _ => "jdbc:h2:mem:lift;DB_CLOSE_DELAY=-1"
  }


  private def createOne: Box[Connection] = try {
    val driverName: String = Props.get("db.driver") openOr chooseDriver


    val dbUrl: String = Props.get("db.url") openOr chooseURL


    Class.forName(driverName)

    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>
        DriverManager.getConnection(dbUrl, user, pwd)

      case _ => DriverManager.getConnection(dbUrl)
    }

    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
        case Nil if poolSize < maxPoolSize =>
          val ret = createOne
        poolSize = poolSize + 1
        ret.foreach(c => pool = c :: pool)
        ret

        case Nil => wait(1000L); newConnection(name)
        case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }

  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
}

object BrowserLogger extends Loggable {
  object HaveSeenYou extends SessionVar(false)

  def haveSeenYou(session: LiftSession, request: Req) {
    if (!HaveSeenYou.is) {
      logger.info("Created session " + session.uniqueId + " IP: {" + request.request.remoteAddress + "} UserAgent: {{" + request.userAgent.openOr("N/A") + "}}")
      HaveSeenYou(true)
    }
  }
}

object SessionInfoDumper extends LiftActor with Loggable {
  private var lastTime = millis

  private def cyclePeriod = 1 minute

  import net.liftweb.example.lib.SessionChecker

  protected def messageHandler =
    {
      case SessionWatcherInfo(sessions) =>
        if ((millis - cyclePeriod) > lastTime) {
          lastTime = millis
          val rt = Runtime.getRuntime
          rt.gc

          RuntimeStats.lastUpdate = timeNow
          RuntimeStats.totalMem = rt.totalMemory
          RuntimeStats.freeMem = rt.freeMemory
          RuntimeStats.sessions = sessions.size

          val percent = (RuntimeStats.freeMem * 100L) / RuntimeStats.totalMem

          // get more aggressive about purging if we're
          // at less than 35% free memory
          if (percent < 35L) {
            SessionChecker.killWhen /= 2L
	    if (SessionChecker.killWhen < 5000L) 
	      SessionChecker.killWhen = 5000L
            SessionChecker.killCnt *= 2
          } else {
            SessionChecker.killWhen *= 2L
	    if (SessionChecker.killWhen >
                SessionChecker.defaultKillWhen)
	     SessionChecker.killWhen = SessionChecker.defaultKillWhen
            val newKillCnt = SessionChecker.killCnt / 2
	    if (newKillCnt > 0) SessionChecker.killCnt = newKillCnt
          }

          val dateStr: String = timeNow.toString
          logger.info("[MEMDEBUG] At " + dateStr + " Number of open sessions: " + sessions.size)
          logger.info("[MEMDEBUG] Free Memory: " + pretty(RuntimeStats.freeMem))
          logger.info("[MEMDEBUG] Total Memory: " + pretty(RuntimeStats.totalMem))
          logger.info("[MEMDEBUG] Kill Interval: " + (SessionChecker.killWhen / 1000L))
          logger.info("[MEMDEBUG] Kill Count: " + (SessionChecker.killCnt))
        }
    }


  private def pretty(in: Long): String =
    if (in > 1000L) pretty(in / 1000L) + "," + (in % 1000L)
    else in.toString
}
