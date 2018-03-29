/*
 * Copyright 2010 Michael Fortin <mike@brzy.org>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");  you may not use this
 * file except in compliance with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied. See the License for the specific
 * language governing permissions and limitations under the License.
 */
package org.brzy.webapp.action.response


/**
 * A Data class is one of the two possible return types of an action.
 *
 * @author Michael Fortin
 */
sealed trait Data  extends Response

/**
 * Add a name value pair to the servlet request attributes.
 *
 * @param attrs The attributes to add the HttpServletRequest scope.
 */
case class Model(attrs: (String, AnyRef)*) extends Data

/**
 * Add a cookie to the response.
 *
 * @param name The name of the cookie.
 * @param value The value of the cookie.
 * @param path The optional path to set the cookie too.
 * @param maxAge The optional age, defaults to the client session.
 * @param domain the optional domain name.
 */
case class Cookie(name: String,
        value: String,
        path: Option[String] = None,
        maxAge: Int = -1,
        domain: Option[String] = None) extends Data

/**
 * This provides a way to manipulate the HttpSession by returning it from an action.  The companion
 * object provides some helpers to constructing a Session action return object.
 *
 * @param add Attributes to add to the Session
 * @param remove A List of attribute names to remove from the session.
 * @param invalidate Should the session be invalidated.
 */
case class Session(add: List[(String, AnyRef)], remove: Array[String], invalidate: Boolean) extends Data

/**
 * This ass some helper constructors to the Session case class.
 */
object Session {
  /**
   * Adds attributes to the HttpSession
   * @param attrs A list of tuple attributes to add to the session.
   * @return A Session
   */
  def apply(attrs: (String, AnyRef)*) = new Session(List(attrs:_*), Array.empty[String], false)

  /**
   * Remove an attribute from the session.
   * @param attrs the names of the attributes to remove.
   * @return A Session
   */
  def remove(attrs: String*) = new Session(List.empty[(String, AnyRef)], Array(attrs:_*), false)

  /**
   * Invalidates a session.
   * @return A Session
   */
  def invalidate = new Session(List.empty[(String, AnyRef)], Array.empty[String], true)
}

/**
 * Add an attribute to the http session that is only available for a single
 * request by the client.
 *
 * @param message The message to display.
 * @param i18nCode For internationalization, this will pull the code from the message bundle
 *        for the application.  If it's an empty string, the code is ignored.
 */
case class Flash(message: String, i18nCode: String = "") extends Data

/**
 * Add response headers to the HttpServletResponse.
 *
 * @param headers A list of tuples to add to the response.  For example:
 * {{{
 * Headers("Content-Type"->"text/json","Cache-Control"->"no-cache")
 * }}}
 }*/
case class Headers(headers: (String, String)*) extends Data