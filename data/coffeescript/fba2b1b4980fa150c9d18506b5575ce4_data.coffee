(data) ->
  admin   = require "vendor/lpue/lib/client/admin"
  userCtx = $$("#account").userCtx
  if data.rows.length > 0
    row = data.rows[0]
    # the result is:
    row.doc.time = (new Date row.doc.time*1000).toYMDString()
    data    : row.doc,
    is_guest: admin.isGuest userCtx
  else
    is_guest: true
