(data) ->
  if data.rows.length
    object = data.rows[0].doc
    $("#basic-config-editor").data "device", object 
    object
  else
    {error: true}