/*
 * Copyright 2011 <gtalent2@gmail.com>
 * This file is released under the BSD license, as defined here:
 * 	http://www.opensource.org/licenses/bsd-license.php
 */
package char

import "libadv/util"

var Design_characters util.DesignDoc = util.DesignDoc{ID:   "_design/characters", Lang: "javascript",
	Views: util.View("all", "function(doc) { if (doc.Type == 'Character')  emit(doc.Name, doc) }")}

type Character struct {
	ID                                        string "_id"
	Rev                                       string "_rev"
	Type                                      string
	Game, Name, World, Alligiance, Bio, Owner string
}

func NewCharacter() Character {
	var data Character
	data.Type = "Character"
	return data
}


