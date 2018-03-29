imagesCompare = undefined
clearTimeOut = null
xhrChangeMonth = null

window.sendRequest = (settings) ->
  token = $('meta[name="csrf-token"]')
  if token.size() > 0
    headers =
      "X-CSRF-Token": token.attr("content")
    settings.headers = headers
  xhrChangeMonth = $.ajax(settings)

initCompare = ->
  imagesCompareElement = $('.js-img-compare').imagesCompare()
  imagesCompare = imagesCompareElement.data('imagesCompare')
  events = imagesCompare.events()

  imagesCompare.on events.changed, (event) ->
    true

getFirstLastImages = (image_id, query_string, reload, setDate) ->
  data =
    api_id: Evercam.User.api_id
    api_key: Evercam.User.api_key

  onError = (jqXHR, status, error) ->
    false

  onSuccess = (response, status, jqXHR) ->
    snapshot = response
    if query_string.indexOf("nearest") > 0 && response.snapshots.length > 0
      snapshot = response.snapshots[0]
    if snapshot.data isnt undefined
      $("##{image_id}").attr("src", snapshot.data)
      $("##{image_id}").attr("timestamp", snapshot.created_at)
      if setDate is true && query_string.indexOf("nearest") < 0
        d = new Date(snapshot.created_at*1000)
        before_month = d.getUTCMonth()+1
        before_year = d.getUTCFullYear()
        camera_created_date = new Date(Evercam.Camera.created_at*1000)
        camera_created_month = camera_created_date.getUTCMonth()+1
        camera_created_year = camera_created_date.getUTCFullYear()
        string_date = "#{before_month}/#{d.getUTCDate()}/#{before_year}"
        camera_created_at = "#{camera_created_year}/#{camera_created_month}/#{camera_created_date.getUTCDate()}"
        $('#calendar-before').datetimepicker({value: string_date, minDate: camera_created_at, yearStart: camera_created_year})
        $('#calendar-after').datetimepicker({minDate: camera_created_at, yearStart: camera_created_year})
      if setDate is false && query_string.indexOf("nearest") < 0
        date_after = new Date(snapshot.created_at*1000)
        after_month = date_after.getUTCMonth()+1
        after_year = date_after.getUTCFullYear()
        string_after_date = "#{after_year}/#{after_month}/#{date_after.getUTCDate()}"
        $('#calendar-before').datetimepicker({maxDate: string_after_date, yearEnd: after_year})
        $('#calendar-after').datetimepicker({maxDate: string_after_date, yearEnd: after_year})
      initCompare() if reload
    else
      Notification.show("No image found")

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'GET'
    url: "#{Evercam.API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots#{query_string}"
  sendRequest(settings)

handleTabOpen = ->
  $('.nav-tab-compare').on 'shown.bs.tab', ->
    initCompare()
    updateURL()

updateURL = ->
  url = "#{Evercam.request.rootpath}/compare"
  query_string = ""
  if $("#txtbefore").val() isnt ""
    query_string = "?before=#{moment.utc($("#txtbefore").val()).toISOString()}"
  if $("#txtafter").val() isnt ""
    if query_string is ""
      query_string = "?after=#{moment.utc($("#txtafter").val()).toISOString()}"
    else
      query_string = "#{query_string}&after=#{moment.utc($("#txtafter").val()).toISOString()}"

  url = "#{url}#{query_string}"
  if history.replaceState
    window.history.replaceState({}, '', url)

getQueryStringByName = (name) ->
  name = name.replace(/[\[]/, '\\[').replace(/[\]]/, '\\]')
  regex = new RegExp('[\\?&]' + name + '=([^&#]*)')
  results = regex.exec(location.search)
  if results == null
    null
  else
    decodeURIComponent(results[1].replace(/\+/g, ' '))

clickToCopy = ->
  clipboard = new Clipboard('.copy-url-icon')
  clipboard.on 'success', (e) ->
    $('.bb-alert').width '100px'
    Notification.show 'Copied!'

copyToClipboard = (elem) ->
  targetId = '_hiddenCopyText_'
  isInput = elem.tagName == 'INPUT' or elem.tagName == 'TEXTAREA'
  origSelectionStart = undefined
  origSelectionEnd = undefined
  if isInput
    target = elem
    origSelectionStart = elem.selectionStart
    origSelectionEnd = elem.selectionEnd
  else
    target = document.getElementById(targetId)
    if !target
      target = document.createElement('textarea')
      target.style.position = 'absolute'
      target.style.left = '-9999px'
      target.style.top = '0'
      target.id = targetId
      document.body.appendChild target
    target.textContent = elem.textContent
  currentFocus = document.activeElement
  target.focus()
  target.setSelectionRange 0, target.value.length
  succeed = undefined
  try
    succeed = document.execCommand('copy')
  catch e
    succeed = false
  if currentFocus and typeof currentFocus.focus == 'function'
    currentFocus.focus()
  if isInput
    elem.setSelectionRange origSelectionStart, origSelectionEnd
  else
    target.textContent = ''
  succeed

HighlightDaysInMonth = (query_string, year, month) ->
  data = {}
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key

  onError = (response, status, error) ->
    false

  onSuccess = (response, status, jqXHR) ->
    removeCurrentDateHighlight(query_string)
    removeCurrentHourHighlight(query_string)
    hideBeforeAfterLoadingAnimation(query_string)
    for day in response.days
      HighlightBeforeAfterDay(query_string, year, month, day)

  settings =
    cache: true
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots/#{year}/#{month}/days"

  sendRequest(settings)

HighlightBeforeAfterDay = (query_string, before_year, before_month, before_day) ->
  beforeDays = $("##{query_string} .xdsoft_datepicker table td[class*='xdsoft_date'] div")
  beforeDays.each ->
    beforeDay = $(this)
    if !beforeDay.parent().hasClass('xdsoft_other_month')
      iDay = parseInt(beforeDay.text())
      if before_day == iDay
        beforeDay.parent().addClass 'xdsoft_current'

HighlightSnapshotHour = (query_string, year, month, date) ->
  data = {}
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key

  onError = (jqXHR, status, error) ->
    false

  onSuccess = (response, status, jqXHR) ->
    removeCurrentHourHighlight(query_string)
    hideBeforeAfterLoadingAnimation(query_string)
    for hour in response.hours
      HighlightBeforeAfterHour(query_string, year, month, date, hour)

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    timeout: 15000
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots/#{year}/#{(month)}/#{date}/hours"

  sendRequest(settings)

HighlightBeforeAfterHour = (query_string, before_year, before_month, before_day, before_hour) ->
  beforeHours = $("##{query_string} .xdsoft_timepicker [class*='xdsoft_time']")
  beforeHours.each ->
    beforeHour = $(this)
    iHour = parseInt(beforeHour.text())
    if before_hour == iHour
      beforeHour.addClass 'xdsoft_current'

removeCurrentHourHighlight = (query_string) ->
  beforeHours = $("##{query_string} .xdsoft_timepicker [class*='xdsoft_time']")
  beforeHours.removeClass 'xdsoft_current'

removeCurrentDateHighlight = (query_string) ->
  beforeDays = $("##{query_string} .xdsoft_datepicker table td[class*='xdsoft_date']")
  beforeDays.removeClass 'xdsoft_current'

showBeforeAfterLoadingAnimation = (query_string) ->
  $("##{query_string} .xdsoft_datepicker").addClass 'opacitypoint5'
  $("##{query_string} .xdsoft_timepicker").addClass 'opacitypoint5'

hideBeforeAfterLoadingAnimation = (query_string) ->
  $("##{query_string} .xdsoft_datepicker").removeClass 'opacitypoint5'
  $("##{query_string} .xdsoft_timepicker").removeClass 'opacitypoint5'

setCompareEmbedCodeTitle = ->
  $("#div-embed-code").on "click", (e)->
    $(".export-buttons #cancel_export").html 'Close'
    after_image_time = $("#compare_after").attr("timestamp")
    before_image_time = $("#compare_before").attr("timestamp")
    if after_image_time && before_image_time isnt undefined
      day_before = moment.utc(before_image_time*1000).format("Do")
      day_after = moment.utc(after_image_time*1000).format("Do")
      month_before = moment.utc(before_image_time*1000).format("MMM")
      month_after = moment.utc(after_image_time*1000).format("MMM")
      $("#export-compare-title").val("#{day_before} #{month_before} to #{day_after} #{month_after}")
      e.stopPropagation()
      $('#export-compare-modal').modal 'show'
    else
      e.stopPropagation()
      $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
      $(".bb-alert").css "width", "410px"
      Notification.show("Unable to export compare, before/after image is not available.")

export_compare = ->
  $("#export_compare_button").on "click", ->
    $("#spn-success-export").removeClass("alert-info").addClass("alert-danger")
    name = $("#export-compare-title").val()
    if name is ""
      $("#spn-success-export").text("Please enter export name.").removeClass("hide")
      return false

    button = $(this)
    button.prop("disabled", true)
    $("#row-animation").removeClass("hide")
    after = " #{convert_timestamp_to_path($("#compare_after").attr("timestamp"))}"
    before = " #{convert_timestamp_to_path($("#compare_before").attr("timestamp"))}"
    embed_code = "<div id='evercam-compare'></div><script src='#{window.location.origin}/assets/evercam_compare.js' class='#{Evercam.Camera.id}#{before}#{after} autoplay'></script>"
    $("#txtEmbedCode").val(embed_code)

    data =
      api_id: Evercam.User.api_id
      api_key: Evercam.User.api_key
      name: name
      before: $("#compare_before").attr("timestamp")
      before_image: $("#compare_before").attr("src")
      after: $("#compare_after").attr("timestamp")
      after_image: $("#compare_after").attr("src")
      embed: embed_code
      create_animation: true

    onError = (jqXHR, status, error) ->
      $("#spn-success-export").text("Failed to export compare.").removeClass("hide")
      $("#row-animation").addClass("hide")
      button.prop("disabled", false)

    onSuccess = (response, status, jqXHR) ->
      button.hide()
      $(".export-buttons #cancel_export").html 'Ok'
      $("#row-animation").addClass("hide")
      $("#row-textarea").removeClass("hide")
      $("#row-message").removeClass("hide")
      $("#spn-success-export").addClass("alert-info").removeClass("alert-danger").addClass("hide")
      $("#gif_url").val(response.compares[0].gif_url.replace("media.evercam.io", "api.evercam.io"))
      $("#mp4_url").val(response.compares[0].mp4_url.replace("media.evercam.io", "api.evercam.io"))
      window.on_export_compare()
      clearTimeOut = setTimeout( ->
        auto_check_compare_status(response.compares[0].id, 0)
      , 10000)

    settings =
      cache: false
      data: data
      dataType: 'json'
      error: onError
      success: onSuccess
      type: 'POST'
      url: "#{Evercam.API_URL}cameras/#{Evercam.Camera.id}/compares"
    sendRequest(settings)

convert_timestamp_to_path = (timestamp) ->
  timestamp_to_int = parseInt(timestamp)
  moment.utc(timestamp_to_int*1000).format('YYYY/MM/DD/HH_mm_ss')

cancelForm = ->
  $('#export-compare-modal').on 'hide.bs.modal', ->
    clean_form()

  $('#export-compare-modal').on 'show.bs.modal', ->
    clean_form()

clean_form = ->
  $("#txtEmbedCode").val("")
  $("#row-textarea").addClass("hide")
  $("#spn-success-export").addClass("hide")
  $("#export_compare_button").prop("disabled", false)
  $("#export_compare_button").show()
  $("#row-gif-url").addClass("hide")
  $("#row-mp4-url").addClass("hide")
  $("#cancel_export").show()
  $("#row-message").addClass("hide")
  clearTimeout(clearTimeOut)

download_animation = ->
  $(".download-animation").on "click", ->
    src_id = $(this).attr("data-download-target")
    NProgress.start()
    download($("#{src_id}").val())
    setTimeout( ->
      NProgress.done()
    , 4000)

switch_to_archive_tab = ->
  $("#switch_archive").on "click", ->
    $(".nav-tab-archives").tab('show')

auto_check_compare_status = (compare_id, tries) ->
  onError = (jqXHR, status, error) ->
    false

  onSuccess = (response, status, jqXHR) ->
    if response.compares[0].status is "Completed"
      $("#row-gif-url").removeClass("hide")
      $("#row-mp4-url").removeClass("hide")
      $("#row-message").addClass("hide")
    else if response.compares[0].status is "Processing" && tries < 10
      clearTimeOut = setTimeout( ->
        auto_check_compare_status(response.compares[0].id, tries++)
      , 10000)

  settings =
      cache: false
      data: {}
      dataType: 'json'
      error: onError
      success: onSuccess
      type: 'GET'
      url: "#{Evercam.API_URL}cameras/#{Evercam.Camera.id}/compares/#{compare_id}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"
    sendRequest(settings)

window.initializeCompareTab = ->
  getFirstLastImages("compare_before", "/oldest", false, true)
  getFirstLastImages("compare_after", "/latest", false, false)
  handleTabOpen()
  removeCurrentDateHighlight()
  export_compare()
  cancelForm()
  clickToCopy()
  download_animation()
  switch_to_archive_tab()
  setCompareEmbedCodeTitle()

  $('#calendar-before').datetimepicker
    format: 'm/d/Y H:m'
    id: 'before-calendar'
    onSelectTime: (dp, $input) ->
      $("#txtbefore").val($input.val())
      val = getQueryStringByName("after")
      url = "#{Evercam.request.rootpath}/compare?before=#{moment.utc($input.val()).toISOString()}"
      if val isnt null
        url = "#{url}&after=#{val}"
      if history.replaceState
        window.history.replaceState({}, '', url)
      getFirstLastImages("compare_before", "/#{(new Date($input.val())) / 1000}/nearest", true, false)
    onChangeMonth: (dp, $input) ->
      xhrChangeMonth.abort()
      month = dp.getMonth() + 1
      year = dp.getFullYear()
      HighlightDaysInMonth("before-calendar", year, month)
      removeCurrentHourHighlight("before-calendar")
      showBeforeAfterLoadingAnimation("before-calendar")
    onSelectDate: (ct, $i) ->
      month = ct.getMonth() + 1
      year = ct.getFullYear()
      date = ct.getDate()
      HighlightSnapshotHour("before-calendar", year, month, date)
      HighlightDaysInMonth("before-calendar", year, month)
    onShow: (current_time, $input) ->
      month = current_time.getMonth() + 1
      year = current_time.getFullYear()
      removeCurrentHourHighlight("before-calendar")
      HighlightDaysInMonth("before-calendar", year, month)
      showBeforeAfterLoadingAnimation("before-calendar")

  $('#calendar-after').datetimepicker
    format: 'm/d/Y H:m'
    id: 'after-calendar'
    onSelectTime: (dp, $input) ->
      $("#txtafter").val($input.val())
      val = getQueryStringByName("before")
      url = "#{Evercam.request.rootpath}/compare"
      if val isnt null
        url = "#{url}?before=#{val}&after=#{moment.utc($input.val()).toISOString()}"
      else
        url = "#{url}?after=#{moment.utc($input.val()).toISOString()}"
      if history.replaceState
        window.history.replaceState({}, '', url)
      getFirstLastImages("compare_after", "/#{(new Date($input.val())) / 1000}/nearest", true, false)
    onChangeMonth: (dp, $input) ->
      xhrChangeMonth.abort()
      month = dp.getMonth() + 1
      year = dp.getFullYear()
      removeCurrentHourHighlight("after-calendar")
      HighlightDaysInMonth("after-calendar", year, month)
      showBeforeAfterLoadingAnimation("after-calendar")
    onSelectDate: (ct, $i) ->
      month = ct.getMonth() + 1
      year = ct.getFullYear()
      date = ct.getDate()
      HighlightSnapshotHour("after-calendar", year, month, date)
      HighlightDaysInMonth("after-calendar", year, month)
    onShow: (current_time, $input) ->
      month = current_time.getMonth() + 1
      year = current_time.getFullYear()
      removeCurrentHourHighlight("after-calendar")
      HighlightDaysInMonth("after-calendar", year, month)
      showBeforeAfterLoadingAnimation("after-calendar")
