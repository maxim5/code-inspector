// Copyright 2013 Justin Wilson. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package data

import (
	"code.minty.io/wombat"
	"code.minty.io/wombat/config"
	"code.minty.io/wombat/users"
)

type Data struct {
	cookieKey        string
	IsProd           bool
	MediaURL, Domain string
	User             users.User
}

func New(ctx wombat.Context) Data {
	t := new(Data)
	t.IsProd = config.IsProd
	t.MediaURL = config.MediaURL
	t.Domain = config.ServerDomain
	t.User = ctx.User

	return *t
}
