/*
 * Copyright 2011 <gtalent2@gmail.com>
 * This file is released under the BSD license, as defined here:
 * 	http://www.opensource.org/licenses/bsd-license.php
 */
package posts

import "libadv/util"

//Database views

var Design_posts util.DesignDoc = util.DesignDoc{ID:   "_design/posts",Lang: "javascript",
	Views: util.View("all", "function(doc) { if (doc.Type == 'Post')  emit(doc.Title, doc) }")}

type Post struct {
	ID             string "_id"
	Rev            string "_rev"
	Type           string
	Title, Author, Owner, Date, Content string
}

//Returns a new Post object by value.
func NewPost() Post {
	var data Post
	data.Type = "Post"
	return data
}

