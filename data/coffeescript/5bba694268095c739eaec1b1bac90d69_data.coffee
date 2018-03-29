(data) ->
  priv = ($ @).data "data"

  result = 
    container_tag : priv.container_tag ? "ul"
    item_tag      : priv.item_tag ? "li"
    items         : []
  
  items = {}
  
  for r in data.rows    
    [key, value] = if priv.query.include_docs
                     [r.doc._id, r.doc]
                   else
                     [r.key, r.value]
    result.items.push {id: key, value: value}
    items[r.key] = value
  ($ @).data "items", items ## save the query result
  result
  
