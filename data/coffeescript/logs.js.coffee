table = null
format_time = null
offset = null
cameraOffset = null
mouseOverCtrl = undefined
evercam_logs = undefined

sendAJAXRequest = (settings) ->
  token = $('meta[name="csrf-token"]')
  if token.size() > 0
    headers =
      "X-CSRF-Token": token.attr("content")
    settings.headers = headers
  xhrRequestChangeMonth = $.ajax(settings)

updateLogTypesFilter = () ->
  NProgress.start()
  exid = $('#exid').val()
  page = $('#current-page').val()
  types = []
  $.each($("input[name='type']:checked"), ->
    types.push($(this).val())
  )
  from_date = moment($('#datetimepicker').val(), "DD-MM-YYYY H:mm")
  to_date = moment($('#datetimepicker2').val(), "DD-MM-YYYY H:mm")
  from = from_date._d.getTime()/ 1000
  to = to_date._d.getTime()/ 1000
  if from && to
    showStatusBar(from, to)
  fromto_seg = ''
  fromto_seg += '&from=' + from unless isNaN(from)
  fromto_seg += '&to=' + to unless isNaN(to)
  newurl = $('#base-url').val()+ "&page=" + page + "&types=" + types.join() + fromto_seg
  table.ajax.url(newurl).load() if table?
  $('#ajax-url').val(newurl) if not table?
  true

toggleAllTypeFilters = ->
  $('#type-all').change ->
    status = this.checked
    $('.logs-checkbox').each ->
      this.checked = status
      if status == true
        $(".type-label span").addClass("checked")
      else
        $(".type-label span").removeClass("checked")

  $(".logs-checkbox").change ->
    if this.checked == false
      $('#type-all')[0].checked = false
      $("label[for='type-all'] span").removeClass("checked")
    if $('.logs-checkbox:checked').length == $('.logs-checkbox').length
      $('#type-all')[0].checked = true
      $("label[for='type-all'] span").addClass("checked")

toggleCheckboxes = ->
  if !$('#type-online-offline').is(':checked')
    $("input[id='type-online-offline']").prop("checked", true)
    $("label[for='type-online-offline'] span").addClass("checked")

initializeDataTable = ->
  table = $('#logs-table').DataTable({
    ajax: {
      url: $('#ajax-url').val(),
      dataSrc: (d) ->
        return format_online_log(d.logs)
      error: (xhr, error, thrown) ->
        if xhr.responseJSON
          Notification.show(xhr.responseJSON.message)
        else
          Notification.show("Something went wrong, Please try again.")
        NProgress.done()
    },
    columns: [
      {
        orderable: false,
        data: null,
        defaultContent: '' },
      {data: ( row, type, set, meta ) ->
        getImage(row)
        time = row.done_at*1000
        return "
          <div class='#{row.done_at} thumb-div'>
          </div>\
          <span>#{moment(time).format('MMMM Do YYYY, H:mm:ss')}</span>"
      , sType: 'uk_datetime' },
      {data: ( row, type, set, meta ) ->
        ip = ""
        if row.extra and row.extra.ip
          ip = ", ip: #{row.extra.ip}"
        if row.action is 'shared' or row.action is 'stopped sharing'
          if row.extra && row.extra.with
            return ("#{row.action} with #{row.extra.with}") + ip
          else
            return row.action
        if row.action is 'edited' or
          row.action is 'created' or
          row.action is 'cloud recordings updated' or
          row.action is 'cloud recordings created' or
          row.action is 'archive created' or
          row.action is 'archive deleted'
            return row.action + ip
        else if row.action is 'online'
          if row.extra
            return "<div class='onlines'>#{row.extra.message}</div>"
          else
            return '<div class="onlines">Camera came online</div>'
        else if row.extra and row.action is 'offline'
          getOfflineCause(row)
        else if row.action is 'offline'
          return '<div class="offlines">Camera went offline</div>'
        else if row.action is 'accessed'
          return 'Camera was viewed'
        else
          return row.action
      , className: 'log-action'},
      {data: ( row, type, set, meta ) ->
        if row.action is 'online' or row.action is 'offline'
          return 'System'
        return row.who
      }
    ],
    autoWidth: false,
    info: false,
    bPaginate: true,
    pageLength: 50
    "language": {
      "emptyTable": "No data available"
    },
    order: [[ 1, "desc" ]],
    drawCallback: ->
      api = @api()
      $.each api.rows(page: 'current').data(), (i, data) ->
        if data.action is 'cloud recordings updated' or
           data.action is 'cloud recordings created' or
           data.action is 'edited'
          $("table#logs-table > tbody > tr:eq(#{i}) td:eq(0)")
            .addClass("details-control")
            .html("<i class='fa fa-plus font-12 expand-icon' aria-hidden='true'></i>")
      NProgress.done()
  })

format = (row) ->
  if row.action is 'cloud recordings updated' or row.action is 'cloud recordings created'
    if row.extra.cr_settings
      return "
        <table cellpadding='5' cellspacing='0' border='0' style='padding-left:50px;width:100%'>
          #{getTableValues(row.extra.cr_settings)}
        </table>
      "
    else
      return "No data available."
  else if row.action is 'edited'
    if row.extra.cam_settings && row.extra.cam_settings != false
      return "
        <table cellpadding='5' cellspacing='0' border='0' style='padding-left:50px;width:100%'>
          #{getCameraValues(row.extra.cam_settings)}
        </table>
      "
    else
      return "No data available."

getCameraValues = (data) ->
  if data
    return "
      <tbody style='float: left; margin-left: 24px;'>
        <tr>
          <th style='background-color: #f1f1f1; font-size: 12px;'>Settings</th>
          <th style='background-color: #f1f1f1; font-size: 12px;'>Old</th>
          <th style='background-color: #f1f1f1; font-size: 12px;'>New</th>
        </tr>
        <tr>
          <td>IP</td>
          #{loadTheChange(data.old.external_host, data.new.external_host)}
        </tr>
        <tr>
          <td>HTTP Port</td>
          #{loadTheChange(data.old.external_http_port, data.new.external_http_port)}
        </tr>
        <tr>
          <td>RTSP Port</td>
          #{loadTheChange(data.old.external_rtsp_port, data.new.external_rtsp_port)}
        </tr>
        <tr>
          <td>Snapshot URL</td>
          #{loadTheChange(data.old.snapshot_url, data.new.snapshot_url)}
        </tr>
        <tr>
          <td>Username</td>
          #{loadTheChange(data.old.auth.username, data.new.auth.username)}
        </tr>
        <tr>
          <td>Password</td>
          #{loadTheChange(data.old.auth.password, data.new.auth.password)}
        </tr>
        <tr>
          <td>Model</td>
          #{loadTheChange(data.old.vendor_model_name, data.new.vendor_model_name)}
        </tr>
        <tr>
          <td>Vendor</td>
          #{loadTheChange(data.old.vendor_name, data.new.vendor_name)}
        </tr>
      </tbody>
    "
  else
    ""

getTableValues = (data) ->
  if data
    return "
      <tbody style='float: left; margin-left: 24px;'>
        <tr>
          <th style='background-color: #f1f1f1; font-size: 12px;'>Settings</th>
          <th style='background-color: #f1f1f1; font-size: 12px;'>Old</th>
          <th style='background-color: #f1f1f1; font-size: 12px;'>New</th>
        </tr>
        <tr>
          <td>Status</td>
          #{loadTheChange(data.old.status, data.new.status)}
        </tr>
        <tr>
          <td>Storage Duration</td>
          #{loadTheChange(data.old.storage_duration, data.new.storage_duration)}
        </tr>
        <tr>
          <td>Frequency</td>
          #{loadTheChange(data.old.frequency, data.new.frequency)}
        </tr>
      </tbody>
    "
  else
    ""

loadTheChange = (old_val, new_val) ->
  if old_val == new_val
    return "
      <td>#{old_val}</td>
      <td>#{new_val}</td>
    "
  else
    return "
      <td style='background-color:yellow;'>#{old_val}</td>
      <td style='background-color:yellow;'>#{new_val}</td>
    "

showDetails = ->
  $('#logs-table tbody').on 'click', 'td.details-control', ->
    tr = $(this).closest('tr')
    row = table.row(tr)
    if row.child.isShown()
      row.child.hide()
      tr.removeClass 'shown'
      tr.find('td.details-control').html("<i class='fa fa-plus font-12 expand-icon' aria-hidden='true'></i>")
    else
      row.child(format(row.data())).show()
      tr.addClass 'shown'
      tr.find('td.details-control').html("<i class='fa fa-minus font-12 expand-icon' aria-hidden='true'></i>")
    return

format_online_log = (logs) ->
  online = null
  offline = null
  $.each logs, (index, log) ->
    if log.action is 'online'
      online = moment(log.done_at*1000)
      tail = logs.slice((index + 1), logs.length)
      $.each tail, (i, head) ->
        if head.action is 'offline'
          offline = moment(head.done_at*1000)
          timeGet = "
            <span class='message'>after #{getTime2(online, offline)}</span>"
          logs[index].extra = {message: "Camera came online #{timeGet}"}
          return false
  return logs

getTime2 = (online, offline) ->
  s = ""
  days = online.diff(offline, "days")
  total_hours = online.diff(offline, "hours")
  hours = total_hours - (days*24)
  total_minutes = online.diff(offline, "minutes")
  minutes = total_minutes - ((days*24*60) + (hours*60))
  total_seconds = online.diff(offline, "seconds")
  seconds = total_seconds - ((days*24*60*60) + (hours*60*60) + (minutes*60))

  if days > 0
    s += days + " days, "
  if hours > 0
    s += hours + " hours, "
  if minutes > 0
    s += minutes + " mins, "
  s += seconds + " seconds"
  return s

getOfflineCause = (row) ->
  switch row.extra.reason
    when "case_clause"
      message = "Bad request."
    when "bad_request"
      message = "Bad request"
    when "closed"
      message = "Connection closed."
    when "nxdomain"
      message = "Non-existant domain."
    when "ehostunreach"
      message = "No route to host."
    when "enetunreach"
      message = "Network unreachable."
    when "req_timedout"
      message = "Request to the camera timed out."
    when "timeout"
      message = "Camera response timed out."
    when "connect_timeout"
      message = "Connection to the camera timed out."
    when "econnrefused"
      message = "Connection refused."
    when "not_found"
      message = "Camera url is not found."
    when "forbidden"
      message = "Camera responded with a Forbidden message."
    when "unauthorized"
      message = "Please check the username and password."
    when "device_error"
      message = "Camera responded with a Device Error message."
    when "device_busy"
      message = "Camera responded with a Device Busy message."
    when "moved"
      message = "Camera url has changed, please update it."
    when "not_a_jpeg"
      message = "Camera didn't respond with an image."
    when "unhandled"
      message = "Sorry, we dropped the ball."
  error = "<span class='message'>( Cause: #{message} )</span>"
  return "<div class='offlines'>Camera went offline #{error}</div>"

getImage = (row) ->
  timestamp = row.done_at
  data = {}
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key

  onSuccess = (response) ->
    if response.snapshots and response.snapshots.length > 0
      img_src = response.snapshots[0].data
    else
      img_src = "/assets/offline.png"
    img = $('<i>',{class: "thumbs fa fa-picture-o"})
    img.attr "aria-hidden", true
    img.attr "src",img_src
    $("#logs .#{timestamp}").empty()
    $("#logs .#{timestamp}").append(img)

  onError = (jqXHR, status, error) ->
    icon = $('<i>',{class: "thumbs fa fa-picture-o"})
    icon.attr "aria-hidden", true
    icon.attr "src", "/assets/offline.png"
    $("#logs .#{timestamp}").empty()
    $("#logs .#{timestamp}").append(icon)

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots/#{timestamp}"
  #sendAJAXRequest(settings)

callDate = ->
  DateFromCalendar = new Date(moment.utc().format('MM/DD/YYYY'))
  DateFromCalendar.setDate(DateFromCalendar.getDate() - 30)
  CalendarDateformated =  format_time.formatDate(DateFromCalendar, 'd/m/y')
  $('#datetimepicker').val(getDate('from'))
  $('#datetimepicker2').val(getDate('to'))
  $('#datetimepicker').datetimepicker({value: CalendarDateformated})

getDate = (type) ->
  DateFromTime = new Date(moment.utc().format('MM/DD/YYYY, HH:mm:ss'))
  DateFromTime.setHours(DateFromTime.getHours() + (cameraOffset))
  if type is "from"
    DateFromTime.setDate(DateFromTime.getDate() - 30)
    DateFromTime.setHours(0)
    DateFromTime.setMinutes(0)
  if type is "to"
    DateFromTime.setHours(DateFromTime.getHours() + 2)
  Dateformated =  format_time.formatDate(DateFromTime, 'd/m/y H:i')
  return Dateformated

onImageHover = ->
  $("#logs-table").on "mouseover", ".thumbs", ->
    data_src = $(this).attr "src"
    content_height = Metronic.getViewPort().height
    mouseOverCtrl = this
    $(".full-image").attr("src", data_src)
    $(".div-elms").show()
    thumbnail_height = $('.div-elms').height()
    thumbnail_center = (content_height - thumbnail_height) / 2
    $('.div-elms').css({"top": "#{thumbnail_center}px"})

  $("#logs-table").on "mouseout", mouseOverCtrl, ->
    $(".div-elms").hide()

showStatusBar = (from, to) ->
  data = {}
  data.to = to
  data.from = from
  data.camera_id = Evercam.Camera.id
  data.camera_name = Evercam.Camera.name
  data.camera_status = Evercam.Camera.is_online
  data.created_at = Evercam.Camera.created_at
  data.timezone = Evercam.Camera.timezone

  onSuccess = (response) ->
    initReport(response)

  onError = (jqXHR, status, error) ->
    Notification.show("Something went wrong, Please try again.")

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "/single_camera_status_bar"

  $.ajax(settings)

initReport = (logs) ->
  evercam_logs = logs
  chart = singleStatusBar()
  chart.width $('.portlet-body').width() - 100
  chart.dataHeight = 10
  $('#status_bar').text ''
  d3.select('#status_bar').datum(evercam_logs).call chart
  return

doResize = ->
  $(window).resize ->
    initReport(evercam_logs)

window.initializeLogsTab = ->
  moment.locale('en')
  doResize()
  offset = $('#camera_time_offset').val()
  cameraOffset = parseInt(offset)/3600
  format_time = new DateFormatter()
  callDate()
  $('#apply-types').click(updateLogTypesFilter)
  $('.datetimepicker').datetimepicker(format: 'd/m/Y H:m')
  toggleAllTypeFilters()
  jQuery.fn.DataTable.ext.type.order['string-date-pre'] = (x) ->
    return moment(x, 'MMMM Do YYYY, H:mm:ss').format('X')
  toggleCheckboxes()
  updateLogTypesFilter()
  initializeDataTable()
  onImageHover()
  showDetails()
