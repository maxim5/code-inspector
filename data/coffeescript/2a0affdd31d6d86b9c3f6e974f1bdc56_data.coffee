(data) ->
  if data.rows.length
    data.rows[0].doc
  else
    {error: true}