(data) ->
  result = {}
  editor = $ @
  result = if data.rows.length
             schedule      = data.rows[0].doc
             pattern       = /(sunrise|sunset)((\+|\-)(\d){1,2})/
             starts_match  = pattern.exec(schedule.time.starts)
             stops_match   = pattern.exec(schedule.time.stops)
             editor.data "schedule", schedule

             action : schedule.action
             freq   : schedule.freq
             time   :
               'start-date' : schedule.time.from
               'stop-date'  : schedule.time.to
               'from-time'  : if !starts_match then schedule.time.starts else "00:00"
               'to-time'    : if !stops_match then schedule.time.stops else "00:00"
               'from-time-spec' : if !starts_match then "exact" else starts_match[1]
               'to-time-spec'   : if !stops_match then "exact" else stops_match[1]
               'from-suntime-adjustment-sign' : if !starts_match then '+' else starts_match[3]
               'to-suntime-adjustment-sign'   : if !stops_match then '+' else stops_match[3]
               'from-minutes-adjustment' : if starts_match then starts_match[4] else "0"
               'to-minutes-adjustment' : if stops_match then stops_match[4] else "0"

           else # default values for new schedule
             today = ((new Date).toISOString().split 'T')[0]

             action : "record"
             freq   : "once"
             time   :
               'start-date'     : today
               'stop-date'      : today
               'from-time'      : "00:00"
               'to-time'        : "00:00"
               'from-time-spec' : "exact"
               'to-time-spec'   : "exact"
               'from-suntime-adjustment-sign' : '+'
               'to-suntime-adjustment-sign'   : '+'
               'from-minutes-adjustment'      : "0"
               'to-minutes-adjustment'        : "0"

  $("#scheduling-editor").data "schedule-data", result
  # read device document
  $$(this).app.db.view "views/devices-all",
    include_docs: true
    startkey : [editor.data("selected-device"), 0]
    limit    : 1
    success  : (data) -> editor.data "device", data.rows[0].doc
  result