/*
 * S-BPM Groupware v1.2
 *
 * http://www.tk.informatik.tu-darmstadt.de/
 *
 * Copyright 2013 Telecooperation Group @ TU Darmstadt
 * Contact: Stephan.Borgert@cs.tu-darmstadt.de
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package de.tkip.sbpm.persistence.schema

import de.tkip.sbpm.persistence.mapping._

/**
 * Defines the database schema of Processes.
 * If you want to query Processes database table mix this trait
 * into the actor performing the queries.
 */
object InterfaceSchema extends Schema {
  // import current slick driver dynamically

  import driver.simple._

  // represents schema if the "processes" table in the database
  // using slick's lifted embedding API
  class Interfaces(tag: Tag) extends SchemaTable[Interface](tag, "interfaces") {
    def id = autoIncIdCol[Int]
    def interfaceType = column[String]("interfaceType")
    def addressId = column[Int]("address_id")
    def processId = column[Int]("process_id")
    def graphId = column[Int]("graph_id")
    def name = nameCol
    def * = (interfaceType, id.?, addressId, processId, graphId, name) <> (Interface.tupled, Interface.unapply)
    // def autoInc = * returning id
    def uniqueName = unique(name)
  }

  val interfaces = TableQuery[Interfaces]
}
