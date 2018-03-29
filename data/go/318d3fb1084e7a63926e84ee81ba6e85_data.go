/*
 * Copyright 2011 <gtalent2@gmail.com>
 * This file is released under the BSD license, as defined here:
 * 	http://www.opensource.org/licenses/bsd-license.php
 */
package users

import "libadv/util"

var Design_users util.DesignDoc = util.DesignDoc{ID:   "_design/users", Lang: "javascript",
	Views: util.View("all", "function(doc) {if (doc.Type == 'User') emit(doc.Username, doc)}")}

type User struct {
	ID             string "_id"
	Rev            string "_rev"
	Type           string
	Username, Email string
	Password util.Password
}

//Returns a new User object by value.
func NewUser() User {
	var data User
	data.Type = "User"
	return data
}
