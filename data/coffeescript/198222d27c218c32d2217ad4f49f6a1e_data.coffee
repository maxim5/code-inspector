(data) ->
  if data.rows.length > 0
    items = []
    for row in data.rows
      for own key, value of row.value
        items.push {
          code: key
          i18n: for own lang, txt of value
                  {
                    lang: lang
                    txt: txt
                  }
       }

    #{{{ the result
    data: items
    name: $(@).data "input-name"
    view: $(@).data "search-view"
    target: $(@).data "target"
    #}}}
