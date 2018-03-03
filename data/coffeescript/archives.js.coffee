upload = null;
uploadIsRunning = false;
archives_table = null
server_url = "http://timelapse.evercam.io/timelapses"
format_time = null
archives_data = {}
xhrRequestCheckSnapshot = null
has_snapshots = false
archive_js_player = null
imagesCompare = undefined
is_reload = true

sendAJAXRequest = (settings) ->
  token = $('meta[name="csrf-token"]')
  if token.size() > 0
    headers =
      "X-CSRF-Token": token.attr("content")
    settings.headers = headers
  xhrRequestChangeMonth = $.ajax(settings)

isUnauthorized = (response) ->
  if response.responseText.indexOf("/v1/users/signin") isnt -1
    Notification.show("Your session has expired.")
    location = window.location
    location.assign(location.protocol + "//" + location.host)
  else
    Notification.show(jqXHR.responseJSON.message)

initDatePicker = ->
  $('.clip-datepicker').datetimepicker
    step: 1
    closeOnDateSelect: 0
    format: 'd/m/Y'
    timepicker: false
    onSelectDate: (ct, $i) ->
      if $("#txtCreateArchiveType").val() is ""
        GetSnapshotInfo()

  $("#archive-time").on "keyup", ->
    if $("#txtCreateArchiveType").val() is ""
      GetSnapshotInfo()

  $("#to-date").on "keyup", ->
    if $("#txtCreateArchiveType").val() is ""
      GetSnapshotInfo()

initializeArchivesDataTable = ->
  archives_table = $('#archives-table').DataTable({
    ajax: {
      url: $("#archive-api-url").val(),
      dataSrc: 'archives',
      error: (xhr, error, thrown) ->
    },
    columns: [
      {data: getTitle, sClass: 'title'},
      {data: gravatarName, sClass: 'fullname'},
      {data: renderIsPublic, orderDataType: 'string', type: 'string', sClass: 'public', visible: false},
      {data: renderStatus, sClass: 'center', visible: false},
      {data: "type", sClass: 'text-center', visible: false},
      {data: renderbuttons, sClass: 'options'}
    ],
    iDisplayLength: 50,
    order: [[ 2, "desc" ]],
    bSort: false,
    bFilter: true,
    autoWidth: false,
    drawCallback: ->
      initializePopup() if is_reload
      is_reload = true
      if archives_table
        archives_data = archives_table.data()
        refreshDataTable()
    initComplete: (settings, json) ->
      getArchiveIdFromUrl()
      $("#archives-table_length").hide()
      $("#archives-table_filter").hide()
      if json.archives.length is 0
        $('#archives-table_paginate, #archives-table_info').hide()
        $('#archives-table').hide()
        span = $("<span>")
        span.append($(document.createTextNode("There are no clips.")))
        span.attr("id", "no-archive")
        $('#archives-table_wrapper .col-sm-12').append(span)
      else if json.archives.length < 50
        $("#archives-table_info").hide()
        $('#archives-table_paginate').hide()
      else if json.archives.length >= 50
        $("#archives-table_info").show()
        $('#archives-table_paginate').hide()
      true
  })

renderbuttons = (row, type, set, meta) ->
  div = $('<div>', {class: "form-group"})
  if Evercam.Camera.has_edit_right || row.requested_by is Evercam.User.username
    divPopup =$('<div>', {class: "popbox2"})
    remove_icon = '<span href="#" data-toggle="tooltip" title="Delete" ' +
      'class="archive-actions delete-archive" val-archive-id="'+row.id+
      '" val-camera-id="'+row.camera_id+'">' +
      '<i class="fas fa-trash-alt"></i></span>'
    span = $('<span>', {class: "open-archive"})
    span.append(remove_icon)
    divPopup.append(span)
    divCollapsePopup = $('<div>', {class: "collapse-popup"})
    divBox2 = $('<div>', {class: "box2"})
    divBox2.append($('<div>', {class: "arrow"}))
    divBox2.append($('<div>', {class: "arrow-border"}))
    divMessage = $('<div>', {class: "margin-bottom-10"})
    divMessage.append($(document.createTextNode("Are you sure?")))
    divBox2.append(divMessage)
    divButtons = $('<div>', {class: "margin-bottom-10"})
    inputDelete = $('<input type="button" value="Yes, Remove">')
    inputDelete.addClass("btn btn-primary delete-btn delete-archive2")
    inputDelete.attr("camera_id", Evercam.Camera.id)
    inputDelete.attr("archive_id", row.id)
    inputDelete.attr("archive_type", row.type)
    inputDelete.click(deleteClip)
    divButtons.append(inputDelete)
    divButtons.append('<div class="btn delete-btn closepopup grey">' +
      '<div class="text-center" fit>CANCEL</div></div>')
    divBox2.append(divButtons)
    divCollapsePopup.append(divBox2)
    divPopup.append(divCollapsePopup)
    div.append(divPopup)
  if row.status is "Completed"
    if row.type is "Compare"
      getCompareButtons(div, row)
    else if row.type is "File"
      getFileButtons(row, div)
    else if row.type is "URL"
      return "<a target='_blank' class='archive-actions' href='#{row.media_url}'><i class='fa fa-external-link-alt'></i></a>#{div.html()}"
    else
      DateTime = new Date(moment.utc(row.created_at*1000).format('MM/DD/YYYY, HH:mm:ss'))
      day = DateTime.getDate()
      month = DateTime.getMonth()
      year = DateTime.getFullYear()
      archive_date = new Date(year, month, day)
      if archive_date < new Date(2017, 10, 1)
        mp4_url = "#{Evercam.SEAWEEDFS_URL}#{row.camera_id}/clips/#{row.id}.mp4"
      else
        mp4_url = "https://seaweedfs2.evercam.io/#{row.camera_id}/clips/#{row.id}.mp4"

      view_url = "clip/#{row.id}/play?date=#{year}-#{(parseInt(month) + 1)}-#{day}"
      copy_url = ""
      if row.public is true
        copy_url = '<a href="#" data-toggle="tooltip" title="share" class="archive-actions share-archive" play-url="' + view_url + '" val-archive-id="'+row.id+'" val-camera-id="'+row.camera_id+'"><i class="fa fa-share-alt"></i></a>'

      return '<a class="archive-actions play-clip" href="#" data-width="640" data-height="480" data-toggle="tooltip" title="Play" play-url="' + view_url + '"><i class="fa fa-play-circle"></i></a>' +
        '<a id="archive-download-url' + row.id + '" class="archive-actions" data-toggle="tooltip" title="Download" href="' + mp4_url + '" download="' + mp4_url + '"><i class="fa fa-download"></i></a>' +
          copy_url + div.html()
  else
    return div.html()

getCompareButtons = (div, row) ->
  animation_url = "#{Evercam.API_URL}cameras/#{row.camera_id}/compares/#{row.id}"
  view_url = ""
  copy_url = ""
  return '<div class="dropdown"><a class="archive-actions dropdown-toggle" href="#" data-toggle="dropdown" title="Play"><i class="fa fa-play-circle"></i></a>' +
    '<ul class="dropdown-menu"><li><a class="play-clip" href="#" title="Play GIF" data-width="1280" data-height="720" play-url="' + animation_url + '.gif"><i class="fa fa-play-circle"></i> GIF</a></li>'+
      '<li><a class="play-clip" href="#" title="Play MP4" data-width="1280" data-height="720" play-url="' + animation_url + '.mp4"><i class="fa fa-play-circle"></i> MP4</a></li></ul>' +
    '</div>' +
    '<div class="dropdown float-left"><a class="archive-actions dropdown-toggle" href="#" data-toggle="dropdown" title="Download"><i class="fa fa-download"></i></a>' +
    '<ul class="dropdown-menu"><li><a class="download-archive" href="' + animation_url + '.gif" title="Download GIF" download="' + animation_url + '.gif"><i class="fa fa-download"></i> GIF</a></li>'+
      '<li><a class="download-archive" href="' + animation_url + '.mp4" title="Download MP4" download="' + animation_url + '.mp4"><i class="fa fa-download"></i> MP4</a></li></ul>' +
    '</div>' +
    copy_url + div.html()

getFileButtons = (row, div) ->
  file_url = "#{Evercam.SEAWEEDFS_URL2}#{row.camera_id}/clips/#{row.file_name}"
  return "<a target='_blank' class='archive-actions' href='#{file_url}'><i class='fa fa-external-link-alt'></i></a>#{div.html()}"

getTitle = (row, type, set, meta) ->
  start_index = row.embed_code.indexOf("#{Evercam.Camera.id}")
  if row.embed_code.indexOf("autoplay") > 0
    end_index = row.embed_code.indexOf("autoplay")
  else
    end_index = row.embed_code.indexOf("'></script>")
  query_string = row.embed_code.substring(start_index, end_index) if row.embed_code

  if row.type is "URL"
    return "<div class='gravatar-placeholder'><img class='gravatar-logo' src='https://favicon.yandex.net/favicon/#{getHostName(row.media_url)}'></div>
      <div class='media-url-title'><i class='fa fa-link type-icon type-icon-url'></i>
      <a target='_blank' class='archive-title' href='#{row.media_url}'>#{row.title}</a></div>"
  else if row.type is "File"
    file_url = "#{Evercam.SEAWEEDFS_URL2}#{row.camera_id}/clips/#{row.file_name}"
    return "<div class='gravatar-placeholder'><img class='gravatar-logo' src='#{row.thumbnail_url}'></div>
      <div class='media-url-title'><i class='fa fa-upload type-icon type-icon-url'></i>
      <a target='_blank' class='archive-title' href='#{file_url}'>#{row.title}</a></div>"
  else
    fa_class = "<i class='fas fa-video type-icon'></i>"
    if row.type is "Compare"
      fa_class = "<svg fill='#a0a0a0' height='15' viewBox='0 0 24 24' width='15'>
        <path d='M0 0h24v24H0z' fill='none'/>
        <path d='M10 3H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h5v2h2V1h-2v2zm0 15H5l5-6v6zm9-15h-5v2h5v13l-5-6v9h5c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2z'/>
        </svg>"
    return "<div class='gravatar-placeholder'><img class='gravatar' src='#{row.thumbnail_url}'></div>
      <div class='username-id'><div class='float-left type-icon-alignment'>#{fa_class}</div><div class='float-left'>
      <a id='archive_link_#{row.id}' class='archive-title' href='#' data-id='#{row.id}' data-type='#{row.type}' data-toggle='modal' data-target='#modal-archive-info'>#{row.title}</a>
      <br /><small class='blue'>#{renderFromDate(row, type, set, meta)} - #{renderToDate(row, type, set, meta)}</small></div></div>
      <input id='txtArchiveTitle#{row.id}' type='hidden' value='#{row.title}'>
      <input id='txtArchiveThumb#{row.id}' type='hidden' value='#{row.thumbnail_url}'>
      <input id='txt_frames#{row.id}' type='hidden' value='#{row.frames}'>
      <input id='txt_duration#{row.id}' type='hidden' value='#{renderDuration(row, type, set, meta)}'>
      <input id='archive_embed_code#{row.id}' type='hidden' value='#{query_string}'/>"

gravatarName = (row, type, set, meta) ->
  main_div = $('<div>', {class: "main_div"})
  div = $('<div>', {class: "gravatar-placeholder hide"})
  img = $('<img>', {class: "gravatar #{row.id}"})
  div.append(img)
  div_user = $('<div>', {class: "requester"})
  if row.requester_email
    small = $("<small>")
    small.append(row.requester_name)
    div_user.append("<label>By:</label>")
    div_user.append(small)
    if row.status is 'Processing'
      div_user.append(" ( Processing )")
    else if row.status is 'Failed'
      div_user.append(" ( <span class='offlines'>Failed</span> )")
  else
    div_user.append("Deleted User")
  div_user.append('<br>')
  small = $("<small>")
  small.append(renderDate(row, type, set, meta))
  div_user.append("<label>On:</label>")
  div_user.append(small)
  main_div.append(div)
  main_div.append(div_user)
  return main_div.html()

getHostName = (url) ->
  match = url.match(/:\/\/(www[0-9]?\.)?(.[^/:]+)/i)
  if match isnt null && match.length > 2 && typeof match[2] is 'string' && match[2].length > 0
    return match[2]
  else
    return null

changeImageSource = (email, id) ->
  favicon_url = "https://favicon.yandex.net/favicon/"
  if email
    signature = hex_md5(email)
    index = email.indexOf("@")
    domain = email.substr((index+1))
    favicon_url = favicon_url + domain
    img_src = "https://gravatar.com/avatar/#{signature}?d=#{favicon_url}"
    if domain is "hotmail.com"
      img_src = "https://gravatar.com/avatar/#{signature}"
  else
    img_src = "https://gravatar.com/avatar"

  data = {}

  onSuccess = (data, success, jqXHR) ->
    length = jqXHR.responseText.length
    if length < 100
      img_src = "https://gravatar.com/avatar/#{signature}"
    $("#archives-table .#{id}").attr "src", img_src

  onError = (jqXHR, status, error) ->
    $("#archives-table .#{id}").attr "src", img_src

  settings =
    cache: false
    data: data
    dataType: 'html'
    error: onError
    success: onSuccess
    type: 'GET'
    url: "#{favicon_url}"
  jQuery.ajax(settings)

renderDate = (row, type, set, meta) ->
  getDate(row.created_at*1000)

renderFromDate = (row, type, set, meta) ->
  getDates(row.from_date*1000)

renderToDate = (row, type, set, meta) ->
  getDates(row.to_date*1000)

renderDuration = (row, type, set, meta) ->
  if row.type is "Compare"
    return "9 secs"
  else
    dateTimeFrom = new Date(
      moment.utc(row.from_date*1000).
      format('MM/DD/YYYY,HH:mm:ss')
    )
    dateTimeTo = new Date(
      moment.utc(row.to_date*1000).
      format('MM/DD/YYYY, HH:mm:ss')
    )
    diff = dateTimeTo - dateTimeFrom
    diffSeconds = diff / 1000
    HH = Math.floor(diffSeconds / 3600)
    MM = Math.floor(diffSeconds % 3600) / 60
    MM = Math.round(MM)
    HH = (HH + 1) if MM is 60
    hours = HH + ' ' + if HH is 1 then 'hr' else 'hrs'
    hours = '' if HH is 0
    minutes = MM + ' ' + if MM is 1 then 'min' else 'mins'
    minutes = '' if MM is 0
    minutes = '' if MM is 60
    formatted = hours + ' ' + minutes
    return formatted

renderIsPublic = (row, type, set, meta) ->
  if row.public
    return 'Yes'
  else
    return 'No'

renderStatus = (row, type, set, meta) ->
  if row.status is 'Processing'
    return "<img alt='Loading' style='margin-left: 20px;' src='/assets/loader3.gif'>"
  else
    return row.status

getDates = (times) ->
  offset =  $('#camera_time_offset').val()
  cameraOffset = parseInt(offset)/3600
  DateTime = new Date(moment.utc(times).format('MM/DD/YYYY, HH:mm:ss'))
  DateTime.setHours(DateTime.getHours() + (cameraOffset))
  Dateformateed =  format_time.formatDate(DateTime, 'd/m/y H:i')
  return Dateformateed

getDate = (timestamp) ->
  offset =  $('#camera_time_offset').val()
  cameraOffset = parseInt(offset)/3600
  DateTime = new Date(moment.utc(timestamp).format('MM/DD/YYYY, HH:mm:ss'))
  DateTime.setHours(DateTime.getHours() + (cameraOffset))
  Dateformateed = format_time.formatDate(DateTime, 'd M Y, H:i:s')
  return Dateformateed

shareURL = ->
  $("#archives-table").on "click",".share-archive", ->
    url = $(this).attr("play-url")
    share_url ="https://dash.evercam.io/v1/cameras/#{$(this).attr("val-camera-id")}/#{url}"
    copyToClipboard share_url

copyToClipboard = (text) ->
  window.prompt 'Copy to URL from here', text
  return

tooltip = ->
  $('[data-toggle="tooltip"]').tooltip()
  return

FormatNumTo2 = (n) ->
  num = parseInt(n)
  if num < 10
    "0#{num}"
  else
    num

createClip = ->
  $("#create_clip_button").on "click", ->
    if $("#txtCreateArchiveType").val() is "" && !has_snapshots
      $("#td-has-snapshot").removeClass("alert-info").addClass("alert-danger")
      return false

    duration = parseInt($("#to-date").val())
    date = $("#from-date").val().split('/')
    time = $('.timepicker-default').val().split(":")
    from = moment.tz("#{date[2]}-#{FormatNumTo2(date[1])}-#{FormatNumTo2(date[0])} #{FormatNumTo2(time[0])}:#{FormatNumTo2(time[1])}:00", "UTC")
    to = from.clone().minutes(from.minutes() + duration)

    if $("#clip-name").val() is ""
      Notification.show("Clip title cannot be empty.")
      $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
      return false
    if duration > 60
      Notification.show("Duration exceeds maximum limit of 60 min.")
      $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
      return false
    $(".bb-alert").removeClass("alert-danger").addClass("alert-info")
    NProgress.start()
    $("#create_clip_button").attr 'disabled', 'disabled'

    data =
      title: $("#clip-name").val()
      from_date: from / 1000
      to_date: to / 1000
      is_nvr_archive: $("#txtCreateArchiveType").val()
      requested_by: Evercam.User.username

    onError = (jqXHR, status, error) ->
      if jqXHR.status is 500
        Notification.show("Internal Server Error. Please contact to admin.")
      else
        Notification.show(jqXHR.responseJSON.message)
      $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
      NProgress.done()
      $("#create_clip_button").removeAttr 'disabled'

    onSuccess = (data, status, jqXHR) ->
      if $("#txtCreateArchiveType").val() isnt ""
        window.vjs_player_local.pause()
        $("#clip-create-message").show()
      archives_table.ajax.reload (json) ->
        $('#archives-table').show()
        $("#no-archive").hide()
        NProgress.done()
        formReset()
        setDate()
        $("#create_clip_button").removeAttr 'disabled'

    settings =
      cache: false
      data: data
      dataType: 'json'
      error: onError
      success: onSuccess
      type: 'POST'
      url: "#{Evercam.API_URL}cameras/#{Evercam.Camera.id}/archives?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"
    $.ajax(settings)

GetSnapshotInfo = ->
  xhrRequestCheckSnapshot.abort() if xhrRequestCheckSnapshot
  duration = parseInt($("#to-date").val())
  date = $("#from-date").val().split('/')
  time = $('.timepicker-default').val().split(":")
  from = moment.tz("#{date[2]}-#{FormatNumTo2(date[1])}-#{FormatNumTo2(date[0])} #{FormatNumTo2(time[0])}:#{FormatNumTo2(time[1])}:00", "UTC")
  to = from.clone().minutes(from.minutes() + duration)

  data = {}
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key
  data.from = from / 1000
  data.to = to / 1000
  data.limit = 3600
  data.page = 1

  onError = (jqXHR, status, error) ->
    false

  onSuccess = (response) ->
    $("#row-archive-has-snapshots").slideDown()
    if response == null || response.snapshots.length == 0
      has_snapshots = false
      $("#td-has-snapshot").removeClass("alert-info").addClass("alert-danger")
      $("#td-has-snapshot").html("There are no images for this time period.")
    else
      has_snapshots = true
      $("#td-has-snapshot").addClass("alert-info").removeClass("alert-danger")
      total_snapshots = parseInt(response.snapshots.length)
      total_seconds = Math.round(total_snapshots / 6)
      $("#td-has-snapshot").html("#{total_snapshots} snapshots (<b>#{total_seconds} seconds</b> @ <b>6 FPS</b>).")

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots"

  xhrRequestCheckSnapshot = $.ajax(settings)

setToDate = (date, duration) ->
  dates = date.split(" ")
  date = dates[0]
  date_arr = date.split('/')
  time = dates[1]
  time_arr = time.split(":")
  new_date = new Date(
    date_arr[2],date_arr[1] - 1,date_arr[0],
    time_arr[0],time_arr[1]
  )
  min = $('#archive-time').data('timepicker').minute
  new_date.setMinutes(parseInt(min) + parseInt(duration))
  set_date = format_time.formatDate(new_date, 'd/m/Y H:i:s')
  return set_date

setDate = ->
  offset =  $('#camera_time_offset').val()
  cameraOffset = parseInt(offset)/3600
  DateTime = new Date(moment.utc().format('MM/DD/YYYY'))
  Datefrom = format_time.formatDate(DateTime, 'd/m/Y')
  $('#from-date').val Datefrom,true

formReset = ->
  $("#clip-name").val("")
  $("#txtCreateArchiveType").val("")
  $('#archive-modal').modal('hide')

playClip = ->
  $("#archives-table").on "click", ".play-clip", ->
    width = parseInt($(this).attr("data-width"))
    height = parseInt($(this).attr("data-height"))
    view_url = $(this).attr("play-url")
    window.open view_url, '_blank', "width=#{width}, Height=#{height}, scrollbars=0, resizable=0"

  $("#archives-table").on "click", ".download-archive", ->
    NProgress.start()
    setTimeout( ->
      NProgress.done()
    , 4000)

cancelForm = ->
  $('#archive-modal').on 'hidden.bs.modal', ->
    $("#clip-name").val("")
    $("#txtCreateArchiveType").val("")
    $("#row-archive-has-snapshots").slideUp()
    has_snapshots = false
    setDate()

deleteClip = ->
  $('#archives').on 'click','.delete-archive2', ->
    NProgress.start()
    control = $(this)
    data =
      camera_id: control.attr("camera_id")
      archive_id: control.attr("archive_id")

    onError = (jqXHR, status, error) ->
      Notification.show(jqXHR.responseJSON.message)
      $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
      NProgress.done()

    onSuccess = (data, status, jqXHR) ->
      if control.attr("archive_type") is "Compare"
        refresh_archive_table()
        Notification.show("Compare deleted successfully.")
      else
        if data.message
          $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
          Notification.show(data.message)
          NProgress.done()
        else
          refresh_archive_table()
          Notification.show("Archive deleted successfully.")


    api_url = "#{Evercam.API_URL}cameras/#{Evercam.Camera.id}/archives/#{control.attr("archive_id")}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"
    if control.attr("archive_type") is "Compare"
      api_url = "#{Evercam.API_URL}cameras/#{control.attr("camera_id")}/compares/#{control.attr("archive_id")}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"

    settings =
      cache: false
      data: data
      dataType: 'json'
      error: onError
      success: onSuccess
      type: 'DELETE'
      url: api_url
    $.ajax(settings)

refresh_archive_table = ->
  archives_table.ajax.reload (json) ->
    if json.archives.length is 0
      $('#archives-table_paginate, #archives-table_info').hide()
      $('#archives-table').hide()
      $("#no-archive").show()
    NProgress.done()

initializePopup = ->
  $(".popbox2").popbox
    open: ".open-archive"
    box: ".box2"
    arrow: ".arrow"
    arrow_border: ".arrow-border"
    close: ".closepopup"

refreshDataTable = ->
  status =  jQuery.map(archives_data, (arr) ->
    arr.status
  )
  if ($.inArray('Pending', status)) != -1
    setTimeout archives_table.ajax.reload, 60000
  else if ($.inArray('Processing', status)) != -1
    setTimeout archives_table.ajax.reload, 30000

window.on_export_compare = ->
  archives_table.ajax.reload()
  $('#archives-table').show()
  $("#no-archive").hide()
  refreshDataTable()

getArchiveIdFromUrl = ->
  archive_id = window.Evercam.request.subpath.
    replace(RegExp("archives", "g"), "").
    replace(RegExp("/", "g"), "")
  if archive_id
    $("#archive_link_#{archive_id}").trigger("click")

modal_events = ->
  $("#archives"). on "click", ".archive-title", ->
    id = $(this).attr("data-id")
    type = $(this).attr("data-type")
    root_url = "#{Evercam.request.rootpath}/archives/#{id}"
    if history.replaceState
      window.history.replaceState({}, '', root_url)

    query_string = $("#archive_embed_code#{id}").val()
    # $('#archive-thumbnail').attr("src", $("#txtArchiveThumb#{id}").val())
    url = "#{Evercam.API_URL}cameras/#{Evercam.Camera.id}/compares/#{id}"
    $("#archive_gif_url").val("#{url}.gif")
    $("#archive_mp4_url").val("#{url}.mp4")
    code = "<div id='evercam-compare'></div><script src='#{window.location.origin}/assets/evercam_compare.js' class='#{query_string} autoplay'></script>"
    $("#archive_embed_code").val(code)
    $("#div_frames").text($("#txt_frames#{id}").val())
    $("#div_duration").text($("#txt_duration#{id}").val())
    $("#div_title").text($("#txtArchiveTitle#{id}").val())
    if type isnt "Compare"
      $("#row-compare").hide()
      $(".div-thumbnail").show()
      $("#row-embed-code").hide()
      $("#row-frames").show()
      $("#row-duration").show()
      $("#row-gif").hide()
      $("#archive_mp4_url").val("#{Evercam.request.url}/archives/#{id}")
      archive_js_player.poster($("#txtArchiveThumb#{id}").val())
      archive_js_player.src([
        { type: "video/mp4", src: $("#archive-download-url#{id}").attr("href") }
      ])
      archive_js_player.play()
    else
      $("#row-compare").html(window.compare_html)
      params = query_string.split(" ")
      bucket_url = "https://s3-eu-west-1.amazonaws.com/evercam-camera-assets/"
      before_image = "#{bucket_url}#{Evercam.Camera.id}/snapshots/#{params[1]}.jpg?#{Math.random()}"
      after_image = "#{bucket_url}#{Evercam.Camera.id}/snapshots/#{params[2]}.jpg?#{Math.random()}"
      $("#archive_compare_before").attr("src", before_image)
      $("#archive_compare_after").attr("src", after_image)
      $("#row-frames").hide()
      $("#row-duration").hide()
      $("#row-embed-code").show()
      $("#row-gif").show()
      $("#row-compare").show()
      $(".div-thumbnail").hide()
      initCompare()

  $('#modal-archive-info').on 'hide.bs.modal', ->
    archive_js_player.pause()
    archive_js_player.reset()
    $("#row-compare").html("")
    imagesCompare = undefined
    url = "#{Evercam.request.rootpath}/archives"
    if history.replaceState
      window.history.replaceState({}, '', url)

  $('#social-media-url-modal').on 'hide.bs.modal', ->
    reset_media_url_form()

  $('#social-media-url-modal').on 'show.bs.modal', ->
    reset_media_url_form()

initCompare = ->
  imagesCompareElement = $('.archive-img-compare').imagesCompare()
  imagesCompare = imagesCompareElement.data('imagesCompare')
  events = imagesCompare.events()

  imagesCompare.on events.changed, (event) ->
    true

open_window = ->
  $(".type-link").on "click", ->
    type = $(this).attr("data-type")
    $("#row-archive-has-snapshots").slideUp()
    switch type
      when "local"
        $("#archive_create_caption").text("Create Local Recording Clip")
        $("#txtCreateArchiveType").val("true")
        $('#archive-time').val(getPastOneHour())
      when "cloud"
        $("#archive_create_caption").text("Create Cloud Recording Clip")
        $('#archive-time').val(getPastOneHour())
        GetSnapshotInfo()
      when "compare"
        $(".nav-tab-compare").tab('show')

init_fileupload = ->
  $("#file-upload").on "change", (e) ->
    lbl_old_val = $("#spn-upload-file-name").html()
    fileName = ''
    if $(this).files && $(this).files.length > 1
      fileName = ( $(this).getAttribute( 'data-multiple-caption' ) || '' ).replace( '{count}', $(this).files.length )
    else
      fileName = e.target.value.split( '\\' ).pop()

    if fileName
      $("#spn-upload-file-name").html(fileName)
    else
      $("#spn-upload-file-name").html(lbl_old_val)

  $("#upload-file-modal").on "hide.bs.modal", ->
    $("#file-upload-progress .bar").css("width", "0%")
    $("#file-upload-progress .bar").text("0%")
    $("#file-upload").val("")
    $("#spn-upload-file-name").html("Choose a file or drag it here.")
    $("#upload_file_title").val("")

  $("#start-file-upload").on "click", ->
    input = document.querySelector("#file-upload")
    if upload
      if uploadIsRunning
        upload.abort()
        $("#start-file-upload").val("Resume upload")
        uploadIsRunning = false
      else
        upload.start()
        $("#start-file-upload").val("Pause upload")
        uploadIsRunning = true
    else
      if $("#upload_file_title").val() is ""
        Notification.show("Title cannot be empty.")
        $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
        return false
      if input.files.length is 0
        Notification.show("Choose file to upload.")
        $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
        return false
      startUpload()

startUpload = ->
  file = document.querySelector("#file-upload").files[0]
  if !file
    return

  $("#start-file-upload").val("Pause upload")

  options =
    endpoint: Evercam.TUS_URL
    resume: true
    chunkSize: Infinity
    retryDelays: [0, 1000, 3000, 5000]
    onError: (error) ->
      $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
      Notification.show("Failed because: #{error}")
      reset()
    onProgress: (bytesUploaded, bytesTotal) ->
      percentage = (bytesUploaded / bytesTotal * 100).toFixed(2)
      $("#file-upload-progress .bar").css("width", "#{percentage}%")
      $("#file-upload-progress .bar").text("#{parseInt(percentage)}%")
    onSuccess: ->
      save_upload_file(upload.url, upload.file.name)

  upload = new tus.Upload(file, options)
  upload.start()
  uploadIsRunning = true

reset = ->
  $("#start-file-upload").val("Start upload")
  $("#file-upload-progress .bar").css("width", "0%")
  $("#file-upload-progress .bar").text("0%")
  upload = null
  uploadIsRunning = false

save_upload_file = (file_url, filename) ->
  timespan = moment().utc() /1000

  $(".bb-alert").removeClass("alert-danger").addClass("alert-info")

  data =
    title: $("#upload_file_title").val()
    from_date: timespan
    to_date: timespan
    requested_by: Evercam.User.username
    type: "file"
    file_url: file_url
    file_extension: filename.slice (filename.lastIndexOf('.') - 1 >>> 0) + 2

  onError = (jqXHR, status, error) ->
    if jqXHR.status is 500
      Notification.show("Internal Server Error. Please contact to admin.")
    else
      Notification.show(jqXHR.responseJSON.message)
    $(".bb-alert").removeClass("alert-info").addClass("alert-danger")

  onSuccess = (data, status, jqXHR) ->
    $("#clip-create-message").show()
    archives_table.ajax.reload (json) ->
      $('#archives-table').show()
      $("#no-archive").hide()
      $("#upload-file-modal").modal("hide")
      reset()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'POST'
    url: "#{Evercam.API_URL}cameras/#{Evercam.Camera.id}/archives?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"
  $.ajax(settings)

detect_validate_url = ->
  $("#social_media_url").on "keyup paste change", ->
    url = $("#social_media_url").val()
    domain = getHostName(url)
    setTimeout(->
      if url is "" or domain is null
        $("#icon-media-type").addClass("fa-link")
        $("#media_url_type").hide()
      else
        $("#media_url_type").attr("src", "https://favicon.yandex.net/favicon/#{getHostName(url)}")
        $("#media_url_type").show()
        $("#icon-media-type").removeClass("fa-link")
    , 200)

reset_media_url_form = ->
  $("#media_title_title").val("")
  $("#social_media_url").val("")
  $("#icon-media-type").addClass("fa-link")
  $("#media_url_type").hide()

detect_url = (url, regex) ->
  patt = new RegExp(regex)
  patt.test(url)

handle_submenu = ->
  $("#archive-add").on "click", ->
    top = $(this).position().top
    archive_height = $("#archives").height()
    view_height = Metronic.getViewPort().height
    if view_height - archive_height > 245
      $(".m-menu__submenu").css("top", top - 10)
      $(".triangle-right-border").css("top", "25px")
    else
      $(".triangle-right-border").css("top", "180px")
      $(".m-menu__submenu").css("top", top - 170)
    $(".m-menu__submenu").toggle( "slow")

  $(document).on 'mouseup', (evt) ->
    $(".m-menu__submenu").hide()

save_media_url = ->
  $("#save_social_media_url").on "click", ->
    timespan = moment().utc() /1000
    $(".bb-alert").removeClass("alert-danger").addClass("alert-info")
    NProgress.start()

    data =
      title: $("#media_title_title").val()
      url: $("#social_media_url").val()
      from_date: timespan
      to_date: timespan
      requested_by: Evercam.User.username
      type: "url"

    onError = (jqXHR, status, error) ->
      if jqXHR.status is 500
        Notification.show("Internal Server Error. Please contact to admin.")
      else
        Notification.show(jqXHR.responseJSON.message)
      $(".bb-alert").removeClass("alert-info").addClass("alert-danger")

    onSuccess = (data, status, jqXHR) ->
      archives_table.ajax.reload (json) ->
        $('#archives-table').show()
        $("#no-archive").hide()
        NProgress.done()
        $("#social-media-url-modal").modal("hide")
        reset_media_url_form()

    settings =
      cache: false
      data: data
      dataType: 'json'
      error: onError
      success: onSuccess
      type: 'POST'
      url: "#{Evercam.API_URL}cameras/#{Evercam.Camera.id}/archives?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"
    $.ajax(settings)

getPastOneHour = (d) ->
  d = moment().tz(Evercam.Camera.timezone)
  d.hours(d.hours() - 1)
  return "#{FormatNumTo2(d.hours())}:#{FormatNumTo2(d.minutes())}"

file_drag_drop = ->
  droppedFiles = false
  $('.file-drag-drop').on "drag dragstart dragend dragover dragenter dragleave drop", (e) ->
    e.preventDefault()
    e.stopPropagation()
  .on "dragover dragenter", ->
    $(this).addClass 'is-dragover'
  .on "dragleave dragend drop", ->
    $(this).removeClass 'is-dragover'
  .on 'drop', (e) ->
    droppedFiles = e.originalEvent.dataTransfer.files
    showFiles(droppedFiles)

showFiles = (files) ->
  $("#spn-upload-file-name").text if files.length > 1 then (input.getAttribute('data-multiple-caption') or '').replace('{count}', files.length) else files[0].name

filter_archives = ->
  $(".archive-tab-item").on "click", ->
    is_reload = false
    $(".archive-tab-item i").removeClass("fas").addClass("far")
    $(this).find("i").removeClass("far").addClass("fas")
    archives_table.column(4).search($(this).attr("data-val")).draw()

window.initializeArchivesTab = ->
  window.compare_html = $("#row-compare").html()
  if Evercam.User.username
    archive_js_player = videojs("archive_player")
  format_time = new DateFormatter()
  jQuery.fn.DataTable.ext.type.order['string-date-pre'] = (x) ->
    return moment(x, 'MMMM Do YYYY, H:mm:ss').format('X')
  initDatePicker()
  initializeArchivesDataTable()
  tooltip()
  createClip()
  playClip()
  shareURL()
  setDate()
  deleteClip()
  cancelForm()
  modal_events()
  open_window()
  init_fileupload()
  detect_validate_url()
  handle_submenu()
  save_media_url()
  getArchiveIdFromUrl()
  filter_archives()
