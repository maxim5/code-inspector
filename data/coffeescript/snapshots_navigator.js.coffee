#= require cameras/single/cloud_recording_schedule.js.coffee
snapshotInfos = null
totalFrames = 0
snapshotInfoIdx = 0
currentFrameNumber = 0
cameraCurrentHour = 0
PreviousImageHour = "tdI8"
BoldDays = []
ClearCalendarTimeOut = null
isPlaying = false
PauseAfterPlay = false
playInterval = 250
ChunkSize = 3600
normalSpeed = 1000
totalSnaps = 0
changedPlayFrom = ""
limit = ChunkSize
sliderpercentage = 679
playDirection = 1
playStep = 1
CameraOffset = 0
xhrRequestChangeMonth = null
playFromDateTime = null
playFromTimeStamp = null
CameraOffsetHours = null
CameraOffsetMinutes = null
is_logged_intercom = false
query_value = undefined
fist_image_date = null

showFeedback = (message) ->
  Notification.show(message)

sendAJAXRequest = (settings) ->
  token = $('meta[name="csrf-token"]')
  if token.size() > 0
    headers =
      "X-CSRF-Token": token.attr("content")
    settings.headers = headers
  xhrRequestChangeMonth = $.ajax(settings)

initDatePicker = ->
  $("#ui_date_picker_inline").datepicker().on("changeDate", datePickerSelect).on "changeMonth", datePickerChange
  $("#ui_date_picker_inline table th[class*='prev']").on "click", ->
    changeMonthFromArrow('p')

  $("#ui_date_picker_inline table th[class*='next']").on "click", ->
    changeMonthFromArrow('n')

  $("#hourCalendar td[class*='day']").on "click", ->
    SetImageHour $(this).html(), "tdI#{$(this).html()}"

changeMonthFromArrow = (value) ->
  clearHourCalendar()
  xhrRequestChangeMonth.abort()
  $("#ui_date_picker_inline").datepicker('fill')
  d = $("#ui_date_picker_inline").datepicker('getDate')
  month = d.getMonth()
  year = d.getFullYear()

  if value is 'n'
    month = month + 2
  if month is 13
    month = 1
    year++
  if month is 0
    month = 12
    year--

  walkDaysInMonth(year, month)

  if value =='n'
    d.setMonth(d.getMonth()+1)
  else if value =='p'
    d.setMonth(d.getMonth()-1)
  $("#ui_date_picker_inline").datepicker('setDate',d)
  snapshotInfos = null
  snapshotInfoIdx = 1
  currentFrameNumber = 0
  BoldSnapshotHour(false)

walkDaysInMonth = (year, month) ->
  showDaysLoadingAnimation()
  showHourLoadingAnimation()
  showLoader()

  BoldDays = []
  data = {}
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key

  onError = (response, status, error) ->
    false

  onSuccess = (response, status, jqXHR) ->
    for day in response.days
      HighlightDay(year, month, day, true)

  settings =
    cache: true
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots/#{year}/#{month}/days"

  sendAJAXRequest(settings)

datePickerSelect = (value)->
  dt = value.date
  $("#divPointer").width(0)
  $("#divSlider").width(0)
  $("#ddlRecMinutes").val(0)
  $("#ddlRecSeconds").val(0)
  $("#divDisableButtons").removeClass("hide").addClass("show")
  $("#divFrameMode").removeClass("show").addClass("hide")
  $("#divPlayMode").removeClass("show").addClass("hide")
  hasDayRecord = false
  for j in BoldDays
    if j == dt.getDate()
      hasDayRecord = true
      break

  clearHourCalendar()
  if hasDayRecord
    BoldSnapshotHour(false)
  else
    NoRecordingDayOrHour()

  ClearCalendarTimeOut = setTimeout(ResetDays, 100)

datePickerChange=(value)->
  d = value.date
  year = d.getFullYear()
  month = d.getMonth() + 1
  $("#ui_date_picker_inline").datepicker('setDate', d)
  walkDaysInMonth(year, month)
  snapshotInfos = null
  snapshotInfoIdx = 1
  currentFrameNumber = 0

clearHourCalendar = ->
  $("#hourCalendar td[class*='day']").removeClass("active")
  calDays = $("#hourCalendar td[class*='day']")
  calDays.each ->
    calDay = $(this)
    calDay.removeClass('has-snapshot')

ResetDays = ->
  clearTimeout ClearCalendarTimeOut
  return unless BoldDays.length > 0
  calDays = $("#ui_date_picker_inline table td[class*='day']")
  calDays.each (idx, el) ->
    calDay = $(this)
    if !calDay.hasClass('old') && !calDay.hasClass('new')
      iDay = parseInt(calDay.text())
      for day in BoldDays
        if day is iDay
          calDay.addClass('has-snapshot')
        else
          calDay.addClass('no-snapshot')

selectCurrentDay = ->
  $(".datepicker-days table td[class*='day']").removeClass('active')
  dt = $("#ui_date_picker_inline").datepicker('getDate')
  calDays = $(".datepicker-days table td[class*='day']")
  calDays.each (idx, el) ->
    calDay = $(this)
    if !calDay.hasClass('old') && !calDay.hasClass('new')
      iDay = parseInt(calDay.text())
      if dt.getDate() is iDay
        calDay.addClass('active')

handleSlider = ->
  onSliderMouseMove = (ev) ->
    if snapshotInfos == null || snapshotInfos.length == 0 then return

    sliderStartX = $("#divSlider").offset().left
    sliderEndX = sliderStartX + $("#divSlider").width()

    pos = (ev.pageX - sliderStartX) / (sliderEndX - sliderStartX)
    if pos < 0
      pos = 0

    idx = Math.round(pos * totalFrames)
    if (idx > totalFrames - 1)
      idx = totalFrames - 1

    x = ev.pageX - 80
    if x > sliderEndX - 80
      x = sliderEndX - 80
    frameNo = idx + 1
    $("#divPopup").html("Frame #{frameNo}, #{shortDate(new Date(snapshotInfos[idx].created_at*1000))}")
    $("#divPopup").show()
    $("#divPopup").offset({ top: ev.pageY + 20, left: x })

    $("#divSlider").css('background-position', "#{(ev.pageX - sliderStartX)}px 0px")
    $("#divPointer").css('background-position', "#{(ev.pageX - sliderStartX)}px 0px")
  $("#divSlider").mousemove(onSliderMouseMove)

  onSliderMouseOut = ->
    $("#divPopup").hide()
    $("#divSlider").css('background-position', '-3px 0px')
    $("#divPointer").css('background-position', '-3px 0px')

  $("#divSlider").mouseout(onSliderMouseOut)

  onSliderClick = (ev) ->
    sliderStartX = $("#divSlider").offset().left
    sliderEndX = sliderStartX + $("#divSlider").width()
    x = ev.pageX - sliderStartX
    percent = x / (sliderEndX - sliderStartX)
    nextFrameNum = parseInt(totalFrames * percent)

    if nextFrameNum < 0
      nextFrameNum = 0
    if nextFrameNum > totalFrames
      nextFrameNum = totalFrames
    if nextFrameNum is totalFrames || nextFrameNum is snapshotInfoIdx
      return
    showLoader()
    snapshotInfoIdx = nextFrameNum
    currentFrameNumber = snapshotInfoIdx + 1
    UpdateSnapshotRec(snapshotInfos[nextFrameNum])

  $("#divSlider").click(onSliderClick)

showLoader = ->
  if $('#recording-tab').width() is 0
    $("#imgLoaderRec").hide()
  else
    if $("#imgPlayback").attr("src").indexOf('nosnapshots') != -1
      $("#imgPlayback").attr("src","/assets/plain.png")
    $("#imgLoaderRec").width($('.left-column').width())
    $("#imgLoaderRec").height($('#imgPlayback').height())
    $("#imgLoaderRec").css("top", $('#imgPlayback').css('top'))
    $("#imgLoaderRec").css("left", $('#imgPlayback').css('left'))
    $("#imgLoaderRec").show()

SetInfoMessage = (currFrame, date_time) ->
  $("#divInfo").fadeIn()
  $("#snapshot-notes-text").show()
  $("#divInfo").html("<span class='snapshot-frame'>#{currFrame} of #{totalSnaps}</span> <span class='snapshot-date'>#{shortDate(date_time)}</span>")
  if snapshotInfoIdx
    snapshot = snapshotInfos[snapshotInfoIdx]
    $('#snapshot-notes-text').text snapshotInfos[snapshotInfoIdx].notes

  totalWidth = $("#divSlider").width()
  $("#divPointer").width(totalWidth * currFrame / totalFrames)
  url = "#{Evercam.request.rootpath}/recordings/snapshots/#{moment.utc(date_time).toISOString()}"

  if $(".nav-tabs li.active a").html() is "Recordings" && history.replaceState
    window.history.replaceState({}, '', url)

UpdateSnapshotRec = (snapInfo) ->
  showLoader()
  $("#snapshot-notes-text").text(snapInfo.notes)
  SetInfoMessage currentFrameNumber, new Date(moment(snapInfo.created_at*1000)
  .format('MM/DD/YYYY HH:mm:ss'))
  loadImage(snapInfo.created_at, snapInfo.notes)

getQueryStringByName = (name) ->
  name = name.replace(/[\[]/, '\\[').replace(/[\]]/, '\\]')
  regex = new RegExp('[\\?&]' + name + '=([^&#]*)')
  results = regex.exec(location.search)
  if results == null
    null
  else
    decodeURIComponent(results[1].replace(/\+/g, ' '))

getTimestampFromUrl = ->
  if getQueryStringByName("date_time")
    timestamp = getQueryStringByName("date_time")
  else
    timestamp = window.Evercam.request.subpath.
      replace(RegExp("recordings", "g"), "").
      replace(RegExp("snapshots", "g"), "").
      replace(RegExp("/", "g"), "")
  if isValidDateTime(timestamp)
    timestamp
  else
    ""

isValidDateTime = (timestamp) ->
  moment(timestamp, "YYYY-MM-DDTHH:mm:ss.SSSZ", true).isValid()

handleBodyLoadContent = ->
  offset = $('#camera_time_offset').val()
  CameraOffset = parseInt(offset)/3600
  CameraOffsetHours = Math.floor(CameraOffset)
  CameraOffsetMinutes = 60 *(CameraOffset - CameraOffsetHours)
  currentDate = new Date($("#camera_selected_time").val())
  cameraCurrentHour = currentDate.getHours()
  $("#hourCalendar td[class*='day']").removeClass("active")

  timestamp = getTimestampFromUrl()
  if timestamp isnt ""
    playFromTimeStamp = moment.utc(timestamp)/1000
    playFromDateTime = new Date(moment.utc(timestamp)
    .format('MM/DD/YYYY HH:mm:ss'))
    playFromDateTime.setHours(playFromDateTime.getHours() + CameraOffsetHours)
    playFromDateTime
    .setMinutes(playFromDateTime.getMinutes() + CameraOffsetMinutes)
    currentDate = playFromDateTime
    cameraCurrentHour = currentDate.getHours()
    $("#ui_date_picker_inline").datepicker('update', currentDate)

  $("#tdI#{cameraCurrentHour}").addClass("active")
  PreviousImageHour = "tdI#{cameraCurrentHour}"
  $("#ui_date_picker_inline").datepicker('setDate', currentDate)
  selectCurrentDay()
  $(".btn-group").tooltip()

  showLoader()
  HighlightCurrentMonth()
  BoldSnapshotHour(false)

fullscreenImage = ->
  $("#imgPlayback").dblclick ->
    screenfull.toggle $(this)[0]

  if screenfull.enabled
    document.addEventListener screenfull.raw.fullscreenchange, ->
      if screenfull.isFullscreen
        $("#imgPlayback").css('width','auto')
      else
        $("#imgPlayback").css('width','100%')

HighlightCurrentMonth = ->
  d = $("#ui_date_picker_inline").datepicker('getDate')
  year = d.getFullYear()
  month = d.getMonth() + 1
  walkDaysInMonth(year, month)

HighlightDay = (year, month, day, exists) ->
  d = $("#ui_date_picker_inline").datepicker('getDate')
  calendar_year = d.getFullYear()
  calendar_month = d.getMonth() + 1
  if year == calendar_year and month == calendar_month
    calDays = $("#ui_date_picker_inline table td[class*='day']")
    calDays.each ->
      calDay = $(this)
      if !calDay.hasClass('old') && !calDay.hasClass('new')
        iDay = parseInt(calDay.text())
        if day == iDay
          if playFromDateTime isnt null && playFromDateTime.getDate() == iDay
            calDay.addClass('active')
          if exists
            calDay.addClass('has-snapshot')
            BoldDays.push(day)
          else
            calDay.addClass('no-snapshot')
    hideDaysLoadingAnimation()
    hideHourLoadingAnimation()

BoldSnapshotHour = (callFromDt) ->
  showHourLoadingAnimation()
  $("#divDisableButtons").removeClass("hide").addClass("show")
  $("#divFrameMode").removeClass("show").addClass("hide")
  $("#divPlayMode").removeClass("show").addClass("hide")
  d = $("#ui_date_picker_inline").datepicker('getDate')

  data = {}
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key
  onError = (jqXHR, status, error) ->
    $('#snapshot-tab-save').hide()
    $("#imgPlayback").attr("src", "/assets/nosnapshots.svg")

  settings =
    cache: false
    data: data
    dataType: 'json'
    timeout: 30000
    error: onError
    success: BoldSnapshotHourSuccess
    context: { isCall: callFromDt }
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots/#{d.getFullYear()}/#{(d.getMonth() + 1)}/#{d.getDate()}/hours"

  sendAJAXRequest(settings)

BoldSnapshotHourSuccess = (result, context) ->
  hasRecords = false
  currentDate = new Date($("#camera_selected_time").val())
  AssignedDate = $("#ui_date_picker_inline").datepicker('getDate')
  selected_hour = parseInt(AssignedDate.getHours())
  for hour in result.hours
    $("#tdI#{hour}").addClass('has-snapshot')
    if currentDate.getDate() isnt AssignedDate.getDate() ||
    currentDate.getMonth() isnt AssignedDate.getMonth()
      hasRecords = true
    else
      hasRecords = true
      if selected_hour is 0
        cameraCurrentHour = hour

  if query_value == "latest" && result.hours.length == 0
    hideDaysLoadingAnimation()
    hideHourLoadingAnimation()
    HideLoader()
  else
    if hasRecords
      if this.isCall
        GetCameraInfo true
      else
        SetImageHour(cameraCurrentHour, "tdI#{cameraCurrentHour}")
    else
      NoRecordingDayOrHour()

GetCameraInfo = (isShowLoader) ->
  NProgress.start()
  $("#divDisableButtons").removeClass("hide").addClass("show")
  $("#divFrameMode").removeClass("show").addClass("hide")
  $("#divPlayMode").removeClass("show").addClass("hide")
  if isShowLoader
    showLoader()
  date = $("#ui_date_picker_inline").datepicker('getDate')
  year = date.getFullYear()
  month = date.getMonth() + 1
  day = date.getDate()
  hour = parseInt(cameraCurrentHour)

  data = {}
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key
  onError = (jqXHR, status, error) ->
    NProgress.done()

  onSuccess = (response) ->
    snapshotInfoIdx = 0
    snapshotInfos = response.snapshots
    totalFrames = response.snapshots.length
    totalSnaps = response.snapshots.length
    deviceAgent = navigator.userAgent.toLowerCase()
    if totalSnaps is 1
      $("#divPointer").hide()
    else
      $("#divPointer").show()
    if response == null || response.snapshots.length == 0
      NoRecordingDayOrHour()
    else
      $("#divDisableButtons").removeClass("show").addClass("hide")
      $("#divFrameMode").removeClass("hide").addClass("show")
      iterations = Math.ceil(totalSnaps / ChunkSize)
      sliderpercentage = Math.ceil(100 / iterations)

      if sliderpercentage > 100
        sliderpercentage = 100
      $("#divSlider").width("#{sliderpercentage}%")
      currentFrameNumber=1
      frameDateTime = new Date(moment(snapshotInfos[snapshotInfoIdx]
      .created_at*1000).format('MM/DD/YYYY HH:mm:ss'))
      snapshotTimeStamp = snapshotInfos[snapshotInfoIdx].created_at
      snapshotNotes = snapshotInfos[snapshotInfoIdx].notes

      if playFromDateTime isnt null
        snapshotTimeStamp = SetPlayFromImage playFromTimeStamp
        frameDateTime = new Date(moment(snapshotTimeStamp*1000)
        .format('MM/DD/YYYY HH:mm:ss'))
        if currentFrameNumber isnt 1
          playFromDateTime = null
          playFromTimeStamp = null

      currentDate = $("#camera_selected_time").val()
      currentHour = parseInt($("#camera_current_time").val())
      ImageHour = parseInt(cameraCurrentHour)
      dt = $("#ui_date_picker_inline").datepicker('getDate')
      current_camera_date = moment(dt).format('MM/DD/YYYY')
      if currentDate == current_camera_date && currentHour == ImageHour
        setLatestImage()
      else
        $("#snapshot-notes-text").text(snapshotInfos[snapshotInfoIdx].notes)
        SetInfoMessage(currentFrameNumber, frameDateTime)
        loadImage(snapshotTimeStamp, snapshotNotes)
    NProgress.done()
    hideHourLoadingAnimation()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots/#{year}/#{month}/#{day}/#{hour}"

  sendAJAXRequest(settings)

loadImage = (timestamp, notes) ->
  data = {}
  data.range = 2
  data.notes = notes
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key

  onError = (jqXHR, status, error) ->
    checkCalendarDisplay()

  onSuccess = (response) ->
    if response.snapshots.length > 0
      $("#snapshot-tab-save").show()
      $("#imgPlayback").attr("src", response.snapshots[0].data)
      image_data = response.snapshots[0].data
      if $("#snapshot-magnifier").hasClass 'enabled'
        initElevateZoom()
    HideLoader()
    window.estimateImageSize(image_data)
    checkCalendarDisplay()
    showImageSaveOption()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots/#{timestamp}"

  sendAJAXRequest(settings)

window.estimateImageSize = (image_source) ->
  oneDay = 24 * 60 * 60 * 1000
  created_date = (Evercam.Camera.created_at * 1000)
  if created_date
    camera_created_date = moment.tz(created_date,"#{Evercam.Camera.timezone}")
  current_date = new Date()
  difference_days = Math.round(((current_date - camera_created_date) / oneDay))
  recording_status = Evercam.Camera.cloud_recording.status
  storage_duration = Evercam.Camera.cloud_recording.storage_duration
  storage_frequency = Evercam.Camera.cloud_recording.frequency
  if recording_status is 'paused' || recording_status is 'off'
    $("#show-image-info").addClass 'hide'
  else
    $("#show-image-info").removeClass 'hide'
  head = 'data:image/png;base64,'
  imgFileSize = Math.round((image_source.length - (head.length)) * 3 / 4)
  $("#image-file-size").text(convertFromBytes(imgFileSize))
  totalImageFileSize(imgFileSize, storage_duration, storage_frequency, difference_days)
  monthlyImageFileSize(imgFileSize, storage_duration, storage_frequency)
  setImageDimension()

totalImageFileSize = (image_file_size, image_storage_duration, image_storage_frequency, days_difference) ->
  if image_storage_duration is -1
    image_storage_duration = days_difference
  image_storage_value = (image_storage_duration * 24 * 60)
  total_image_estimates = (image_storage_value * image_storage_frequency * image_file_size)
  $("#totalGb-file-size").text(convertFromBytes(total_image_estimates))

monthlyImageFileSize = (image_file_size, image_storage_duration, image_storage_frequency) ->
  if image_storage_duration is -1 || image_storage_duration > 30
    image_storage_duration = 30
  image_storage_value = (image_storage_duration * 24 * 60)
  monthly_image_estimates = (image_storage_value * image_storage_frequency * image_file_size)
  $("#monthlyGB-file-size").text(convertFromBytes(monthly_image_estimates))

convertFromBytes = (bytes) ->
  if bytes == 0
    return '0 Bytes'
  k = 1024
  sizes = [
    'Bytes'
    'KB'
    'MB'
    'GB'
    'TB'
    'PB'
    'EB'
    'ZB'
    'YB'
  ]
  i = Math.floor(Math.log(bytes) / Math.log(k))
  parseFloat((bytes / k ** i).toFixed(2)) + ' ' + sizes[i]

setImageDimension = ->
  recording_img = $('#imgPlayback')
  $('<img>').attr('src', $(recording_img).attr('src')).load ->
    realWidth = @width
    realHeight = @height
    $("#recording_image_dimension").text(realWidth + 'x' + realHeight)

SetPlayFromImage = (timestamp) ->
  i = 0
  for snapshot in snapshotInfos
    snapshot_timestamp = GetUTCDate(new Date(getSnapshotDate(new Date(snapshot.created_at*1000)).format('MM/DD/YYYY HH:mm:ss')))/1000
    if snapshot.created_at >= timestamp
      currentFrameNumber = i + 1
      snapshotInfoIdx = i
      return snapshot.created_at
    i++
  currentFrameNumber = snapshotInfos.length
  snapshotInfoIdx = snapshotInfos.length - 1
  snapshotInfos[snapshotInfoIdx].created_at

GetUTCDate = (date) ->
  Date.UTC(date.getFullYear(), date.getMonth(), date.getDate(), date.getHours(), date.getMinutes(), date.getSeconds())

shortDate = (date) ->
  dt = $("#ui_date_picker_inline").datepicker('getDate')
  getDate = dt.getDate()
  getMonth = dt.getMonth()
  hour = parseInt(cameraCurrentHour)
  sec = date.getSeconds()
  minutes = date.getMinutes()
  if minutes >= CameraOffsetMinutes
    minutes = minutes - CameraOffsetMinutes
  else
    minutes = minutes + CameraOffsetMinutes
  "#{FormatNumTo2(getDate)}/#{FormatNumTo2(getMonth+1)}/#{date.getFullYear()}
  #{FormatNumTo2(hour)}:#{FormatNumTo2(minutes)}:#{FormatNumTo2(sec)}"

getSnapshotDate = (date) ->
  dt = $("#ui_date_picker_inline").datepicker('getDate')
  hour = parseInt(cameraCurrentHour)
  moment.utc([dt.getFullYear(), dt.getMonth(), dt.getDate(), hour, date.getMinutes(), date.getSeconds(), date.getMilliseconds()])

GetFromDT = ->
  d = $("#ui_date_picker_inline").datepicker('getDate')
  hour = parseInt(cameraCurrentHour)
  Date.UTC(d.getFullYear(), d.getMonth(), d.getDate(), hour, 0, 0)

GetToDT = ->
  d = $("#ui_date_picker_inline").datepicker('getDate')
  hour = parseInt(cameraCurrentHour) + 1
  if hour == 24
    Date.UTC(d.getFullYear(), d.getMonth(), d.getDate(), 23, 59, 59)
  else
    Date.UTC(d.getFullYear(), d.getMonth(), d.getDate(), hour, 0, 0)

StripLeadingZeros = (input) ->
  if input.length > 1 && input.substr(0,1) == "0"
    input.substr(1)
  else
    input

FormatNumTo2 = (n) ->
  if n < 10
    "0#{n}"
  else
    n

NoRecordingDayOrHour = ->
  showLoader()
  $("#imgPlayback").attr("src", "/assets/nosnapshots.svg")
  $("#divRecent").show()
  $("#divInfo").fadeOut()
  $("#divPointer").width(0)
  $("#divSliderBackground").width(0)
  hideDaysLoadingAnimation()
  hideHourLoadingAnimation()
  $("#snapshot-tab-save").hide()
  totalFrames = 0
  HideLoader()

SetImageHour = (hr, id) ->
  value = $("##{id}").html()
  $("#ddlRecMinutes").val(0)
  $("#ddlRecSeconds").val(0)
  cameraCurrentHour = hr
  $("##{PreviousImageHour}").removeClass("active")
  $("##{id}").addClass("active")
  PreviousImageHour = id
  snapshotInfos = null
  Pause()
  currentFrameNumber = 0
  $("#divPointer").width(0)
  $("#divSlider").width("0%")
  $("#divDisableButtons").removeClass("hide").addClass("show")
  $("#divFrameMode").removeClass("show").addClass("hide")
  $("#divPlayMode").removeClass("show").addClass("hide")
  setCreateClipDate(hr)

  if $("##{id}").hasClass('has-snapshot')
    $("#divSliderBackground").width("100%")
    $("#btnCreateHourMovie").removeAttr('disabled')
    GetCameraInfo true
  else
    xhrRequestChangeMonth.abort()

    $("#divRecent").show()
    $("#divInfo").fadeOut()
    $("#snapshot-notes-text").hide()
    $("#divSliderBackground").width("0%")
    $("#txtCurrentUrl").val("")
    $("#btnCreateHourMovie").attr('disabled', true)
    totalFrames = 0
    $("#imgPlayback").attr("src", "/assets/nosnapshots.svg")
    $("#snapshot-tab-save").hide()
    HideLoader()

  hideHourLoadingAnimation()

Pause = ->
  isPlaying = false
  $("#divFrameMode").removeClass("hide").addClass("show")
  $("#divPlayMode").removeClass("show").addClass("hide")
  PauseAfterPlay = true

HideLoader = ->
  $("#imgLoaderRec").hide()

handleWindowResize = ->
  $(window).on "resize", ->
    totalWidth = $("#divSlider").width()
    $("#divPointer").width(totalWidth * currentFrameNumber / totalFrames)

handlePlay = ->
  $("#btnPlayRec").on "click", ->
    return if totalFrames is 0

    playDirection = 1
    playStep = 1
    $("#divFrameMode").removeClass("show").addClass("hide")
    $("#divPlayMode").removeClass("hide").addClass("show")
    isPlaying = true
    if snapshotInfos.length is snapshotInfoIdx + 1
      snapshotInfoIdx = 0
      currentFrameNumber = 1
    DoNextImg()

  $("#btnPauseRec").on "click", ->
    Pause()

  $("#btnFRwd").on "click", ->
    SetPlaySpeed 10, -1

  $("#btnRwd").on "click", ->
    SetPlaySpeed 5, -1

  $("#btnFFwd").on "click", ->
    SetPlaySpeed 10, 1

  $("#btnFwd").on "click", ->
    SetPlaySpeed 5, 1

  $(".skipframe").on "click", ->
    switch $(this).html()
      when "+1" then SetSkipFrames 1, "n"
      when "+5" then SetSkipFrames 5, "n"
      when "+10" then SetSkipFrames 10, "n"
      when "+100" then SetSkipFrames 100, "n"
      when "-1" then SetSkipFrames 1, "p"
      when "-5" then SetSkipFrames 5, "p"
      when "-10" then SetSkipFrames 10, "p"
      when "-100" then SetSkipFrames 100, "p"

SetSkipFrames = (num, direction) ->
  if direction is "p"
    return if snapshotInfoIdx is 0
    if snapshotInfoIdx - num < 0
      currentFrameNumber = 1
      snapshotInfoIdx = 0
    else
      currentFrameNumber = currentFrameNumber - num
      snapshotInfoIdx = snapshotInfoIdx - num
  else if direction is "n"
    return if snapshotInfos.length is snapshotInfoIdx + 1
    if snapshotInfoIdx + num > snapshotInfos.length - 1
      snapshotInfoIdx = snapshotInfos.length - 1
      currentFrameNumber = snapshotInfos.length
    else
      currentFrameNumber = currentFrameNumber + num
      snapshotInfoIdx = snapshotInfoIdx + num
  PauseAfterPlay = false
  playDirection = 1

  UpdateSnapshotRec snapshotInfos[snapshotInfoIdx]

SetPlaySpeed = (step, direction) ->
  playDirection = direction
  playStep = step

DoNextImg = ->
  return if totalFrames is 0
  if snapshotInfos.length is snapshotInfoIdx
    Pause()
    currentFrameNumber = snapshotInfos.length
    snapshotInfoIdx = snapshotInfos.length - 1
  snapshot = snapshotInfos[snapshotInfoIdx]

  data = {}
  data.range = 2
  data.notes = snapshot.notes
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key

  onError = (jqXHR, status, error) ->
    if playDirection is 1 and playStep is 1
      currentFrameNumber++
      snapshotInfoIdx++
    else if playDirection is 1 and playStep > 1
      currentFrameNumber = currentFrameNumber + playStep
      currentFrameNumber = snapshotInfos.length if currentFrameNumber >= snapshotInfos.length
      snapshotInfoIdx = snapshotInfoIdx + playStep
      snapshotInfoIdx = snapshotInfos.length - 1 if snapshotInfoIdx > snapshotInfos.length - 1
    else if playDirection is -1 and playStep > 1
      currentFrameNumber = currentFrameNumber - playStep
      currentFrameNumber = 1 if currentFrameNumber <= 1
      snapshotInfoIdx = snapshotInfoIdx - playStep
      snapshotInfoIdx = 0 if snapshotInfoIdx < 0
      Pause() if snapshotInfoIdx is 0
    window.setTimeout DoNextImg, playInterval if isPlaying
    false

  onSuccess = (response) ->
    if response.snapshots.length > 0
      SetInfoMessage currentFrameNumber,
        new Date(moment(snapshot.created_at*1000).format('MM/DD/YYYY HH:mm:ss'))
    $("#imgPlayback").attr("src", response.snapshots[0].data)

    if playDirection is 1 and playStep is 1
      currentFrameNumber++
      snapshotInfoIdx++
    else if playDirection is 1 and playStep > 1
      currentFrameNumber = currentFrameNumber + playStep
      currentFrameNumber = snapshotInfos.length if currentFrameNumber >= snapshotInfos.length
      snapshotInfoIdx = snapshotInfoIdx + playStep
      snapshotInfoIdx = snapshotInfos.length - 1 if snapshotInfoIdx > snapshotInfos.length - 1
    else if playDirection is -1 and playStep > 1
      currentFrameNumber = currentFrameNumber - playStep
      currentFrameNumber = 1 if currentFrameNumber <= 1
      snapshotInfoIdx = snapshotInfoIdx - playStep
      snapshotInfoIdx = 0 if snapshotInfoIdx < 0
      Pause() if snapshotInfoIdx is 0

    window.setTimeout DoNextImg, playInterval if isPlaying

  settings =
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots/#{snapshot.created_at}"

  sendAJAXRequest(settings)

SelectImagesByMinSec = ->
  min = FormatNumTo2($("#ddlRecMinutes").val())
  sec = FormatNumTo2($("#ddlRecSeconds").val())
  estimatedIndex = Math.round((snapshotInfos.length / 60) * parseInt(min))
  if estimatedIndex < snapshotInfos.length - 1
    i = 0

    while i < snapshotInfos.length
      si = snapshotInfos[i]

      sDt = si.date.substring(si.date.indexOf(" ") + 1).split(":")
      if sDt[1] is min
        if sDt[2] is sec
          currentFrameNumber = i + 1
          snapshotInfoIdx = i
          UpdateSnapshotRec si
        else if sDt[2] > sec
          currentFrameNumber = i
          snapshotInfoIdx = i - 1
          si = snapshotInfos[snapshotInfoIdx]
          sDt = si.date.substring(si.date.indexOf(" ") + 1).split(":")
          $("#ddlRecSeconds").val sDt[2]
          UpdateSnapshotRec si
      else if sDt[1] > min
        currentFrameNumber = i
        snapshotInfoIdx = i - 1
        si = snapshotInfos[snapshotInfoIdx]
        sDt = si.date.substring(si.date.indexOf(" ") + 1).split(":")
        $("#ddlRecSeconds").val sDt[2]
        UpdateSnapshotRec si
      i++
  else
    currentFrameNumber = snapshotInfos.length + 1
    snapshotInfoIdx = snapshotInfos.length
    UpdateSnapshotRec snapshotInfos[snapshotInfoIdx]

handleMinSecDropDown = ->
  hour = 1

  while hour <= 59
    option = $("<option>").val(FormatNumTo2(hour)).append(FormatNumTo2(hour))
    $("#ddlRecMinutes").append option
    option = $("<option>").val(FormatNumTo2(hour)).append(FormatNumTo2(hour))
    $("#ddlRecSeconds").append option
    hour++
  $("#ddlRecMinutes").on "change", ->
    SelectImagesByMinSec()

  $("#ddlRecSeconds").on "change", ->
    SelectImagesByMinSec()

  $('#show-info').click ->
    $('#snapshot-notes-text').toggle()

handleTabOpen = ->
  $('.nav-tab-recordings').on 'shown.bs.tab', ->
    logCameraViewed() unless is_logged_intercom
    window.initScheduleCalendar()
    window.initCloudRecordingSettings()
    if snapshotInfos isnt null
      date_time = new Date(snapshotInfos[snapshotInfoIdx].created_at*1000)
      url = "#{Evercam.request.rootpath}/recordings/snapshots/#{moment.utc(date_time).toISOString()}"
      if history.replaceState
        window.history.replaceState({}, '', url)

saveImage = ->
  $('#save-recording-image').on 'click', ->
    date_time = new Date(snapshotInfos[snapshotInfoIdx].created_at*1000)
    download($("#imgPlayback").attr('src'), "#{Evercam.Camera.id}-#{getSnapshotDate(date_time).toISOString()}.jpg", "image/jpg")
    $('.play-options').css('display','none')
    setTimeout opBack , 1500

saveOldestLatestImage = ->
  $('#save-oldestlatest-image').on 'click', ->
    SaveImage.save($("#imgPlayback").attr('src'), "#{Evercam.Camera.id}-#{query_value}.jpg")
    $('.play-options').css('display','none')
    setTimeout opBack , 1500

opBack = ->
  $('.play-options').css('display','inline')

calculateWidth = ->
  deviceAgent = navigator.userAgent.toLowerCase()
  agentID = deviceAgent.match(/(iPad|iPhone|iPod)/i)
  is_widget = $('#snapshot-diff').val()
  tab_width = $("#recording-tab").width()
  right_column_width = $("#recording-tab .right-column").width()
  if tab_width is 0
    width_add = if !is_widget then 10 else 30
    tab_width = $(".tab-content").width() + width_add
  width_remove = if !is_widget then 40 else 20
  left_col_width = tab_width - right_column_width - width_remove
  if left_col_width > 360
    $("#snapshot-notes-text").show()
    $("#navbar-section .playback-info-bar").css("text-align", "center")
  else
    $("#snapshot-notes-text").hide()
    $("#navbar-section .playback-info-bar").css("text-align", "left")
  if (agentID)
    if tab_width > 480
      $("#recording-tab .left-column").css("width", "#{left_col_width}px")
      $("#recording-tab .right-column").css("width", "220px")
    else
      $("#recording-tab .left-column").css("width", "100%")
      $("#recording-tab .right-column").css("width", "220px")
      $("#snapshot-notes-text").show()
      $("#navbar-section .playback-info-bar").css("text-align", "center")
  else
    if $(window).width() >= 490
      $("#recording-tab .left-column").animate { width:
        "#{left_col_width}px" }, ->
          if $("#recording-tab .left-column").width() is 0
            setTimeout(calculateWidth, 500)
          recodringSnapshotDivHeight()
      $("#recording-tab .right-column").css("width", "220px")
    else
      $("#recording-tab .left-column").css("width", "100%")
      $("#recording-tab .right-column").css("width", "220px")
      $("#snapshot-notes-text").show()
      $("#navbar-section .playback-info-bar").css("text-align", "center")

recodringSnapshotDivHeight = ->
  deviceAgent = navigator.userAgent.toLowerCase()
  agentID = deviceAgent.match(/(iPad|iPhone|iPod)/i)
  if !(agentID)
    if $(window).width() >= 490
      tab_width = $("#recording-tab").width()
      calcuHeight = $(window).innerHeight() - $('.center-tabs').height() - $('div#navbar-section').height()
      if $('#fullscreen > img').height() < calcuHeight
        $('div#navbar-section').removeClass 'navbar-section'
        $('div#navbar-section').css 'width', $('.left-column').width()
      else
        $('div#navbar-section').addClass 'navbar-section'
        $('div#navbar-section').css 'width', $('.left-column').width()
    else
      $('div#navbar-section').removeClass 'navbar-section'
      $('div#navbar-section').css("width", "100%")
  if tab_width is 0
    setTimeout (-> recodringSnapshotDivHeight()), 500

checkCalendarDisplay = ->
  if $('#recording-tab .col-recording-right').css('display') == 'none'
    $('#recording-tab .left-column').animate { width: "99.4%" }, ->
      recodringSnapshotDivHeight()
  else
    calculateWidth()
    recodringSnapshotDivHeight()

calendarShow = ->
  $('#recording-tab .ui-datepicker-trigger').on 'click', ->
    $('#recording-tab .col-recording-right').toggle 'slow', ->
      $('#calendar .fas').css 'color', 'white'
      checkCalendarDisplay()
    $('#calendar .fas').css 'color', '#68a2d5'
    turnOffZoomEffect()

showDaysLoadingAnimation = ->
  $('#img-days-loader').removeClass 'hide'
  $('#ui_date_picker_inline').addClass 'opacity-transparent'

hideDaysLoadingAnimation = ->
  $('#img-days-loader').addClass 'hide'
  $('#ui_date_picker_inline').removeClass 'opacity-transparent'

showHourLoadingAnimation = ->
  $('#img-hour-Loader').removeClass 'hide'
  $('#hourCalendar').addClass 'opacity-transparent'

hideHourLoadingAnimation = ->
  $('#img-hour-Loader').addClass 'hide'
  $('#hourCalendar').removeClass 'opacity-transparent'

handleResize = ->
  calculateWidth()
  recodringSnapshotDivHeight()
  $(window).resize ->
    checkCalendarDisplay()
    turnOffZoomEffect()

logCameraViewed = ->
  is_logged_intercom = true
  data = {}
  data.recordings = true

  onError = (jqXHR, status, error) ->
    false

  onSuccess = (data, status, jqXHR) ->
    true

  settings =
    data: data
    dataType: 'json'
    success: onSuccess
    error: onError
    type: 'POST'
    contentType: 'application/x-www-form-urlencoded'
    url: "/log_intercom"
  sendAJAXRequest(settings)

loadOldestLatestImage = (enter_query) ->
  data =
    api_id: Evercam.User.api_id
    api_key: Evercam.User.api_key

  onError = (jqXHR, status, error) ->
    false

  onSuccess = (response, status, jqXHR) ->
    $("#imgPlayback").attr("src", response.data)
    HideLoader()
    HideImageSaveOption()
    hideDaysLoadingAnimation()
    hideHourLoadingAnimation()
    image_date = response.created_at
    updateImageCalendar(image_date)

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'GET'
    url: "#{Evercam.API_URL}cameras/#{Evercam.Camera.id}/recordings/snapshots/#{enter_query}"
  sendAJAXRequest(settings)

onClickOldestLatestImage = ->
  $('.get-image').on "click", ->
    Pause()
    query_value = $(this).data('query')
    showDaysLoadingAnimation()
    showHourLoadingAnimation()
    clearHourCalendar()
    showLoader()
    loadOldestLatestImage(query_value)
    $('#divDisableButtons').addClass('show').removeClass('hide')
    $('#divFrameMode').addClass('hide').removeClass('show')

showImageSaveOption = ->
  $('#oldestlatest-image').addClass('hide')
  $('#snapshot-tab-save').removeClass('hide')

HideImageSaveOption = ->
  $('#oldestlatest-image').removeClass('hide')
  $('#snapshot-tab-save').addClass('hide')

setLatestImage = ->
  snapshotInfoIdx = snapshotInfos.length - 1
  currentFrameNumber = snapshotInfos.length
  $("#divPointer").width("100%")
  UpdateSnapshotRec snapshotInfos[snapshotInfoIdx]

updateImageCalendar = (oldest_latest_image_date) ->
  currentFrameNumber = 1
  $("#hourCalendar td[class*='day']").removeClass("active")
  image_date = new Date(moment.unix(oldest_latest_image_date).tz("#{Evercam.Camera.timezone}").format("YYYY/MM/DD HH:mm:ss"))
  $("#ui_date_picker_inline").datepicker('update', image_date)
  $("#ui_date_picker_inline").datepicker('setDate', image_date)
  oldest_latest_image_year = image_date.getFullYear()
  oldest_latest_image_month = image_date.getMonth() + 1
  oldest_latest_image_day = image_date.getDate()
  walkDaysInMonth(oldest_latest_image_year, oldest_latest_image_month)
  HighlightFirstDay(oldest_latest_image_year, oldest_latest_image_month, oldest_latest_image_day)
  cameraCurrentHour = image_date.getHours()
  $("#tdI#{cameraCurrentHour}").addClass("active has-snapshot")
  SetInfoMessage(currentFrameNumber, image_date)
  BoldSnapshotHour(false)

HighlightFirstDay = (year, month, day) ->
  d = $("#ui_date_picker_inline").datepicker('getDate')
  calendar_year = d.getFullYear()
  calendar_month = d.getMonth() + 1
  calendar_day = d.getDate()
  if year == calendar_year and month == calendar_month
    calDays = $("#ui_date_picker_inline table td[class*='day']")
    calDays.each ->
      calDay = $(this)
      if !calDay.hasClass('old') && !calDay.hasClass('new')
        iDay = parseInt(calDay.text())
        if day == iDay
          calDay.addClass('has-snapshot active')
  hideDaysLoadingAnimation()
  hideHourLoadingAnimation()

onClickSnapshotMagnifier = ->
  $('#snapshot-magnifier').on 'click', ->
    $('.zoomContainer').remove()
    if $('.enabled').length == 0
      initElevateZoom()
      $(this).toggleClass 'enabled'
    else
      $(this).toggleClass 'enabled'
      $('.zoomContainer').hide()

turnOffZoomEffect = ->
  $('#snapshot-magnifier').removeClass 'enabled'
  $('.zoomContainer').hide()

initElevateZoom = ->
  $('#imgPlayback').elevateZoom
    zoomType: 'lens',
    scrollZoom: true,
    lensShape: 'round',
    lensSize: 230

centerTabClick = ->
  $(document).click (e) ->
    if $(e.target).is('#ul-nav-tab li a,#ul-nav-tab li a span')
      turnOffZoomEffect()

setImageSource = ->
  data_image_src = $("#imgPlayback").attr('src')
  window.estimateImageSize(data_image_src)

removeMagnifierOnEsc = ->
  $(document).on 'keyup', (evt) ->
    if evt.keyCode = 27
      turnOffZoomEffect()

setCreateClipDate = (hour_selected) ->
  $("#recording-tab #recording-archive-button").on "click", ->
    d = $("#recording-tab #ui_date_picker_inline").datepicker('getDate')
    current_calendar_date = moment(d).format('DD/MM/YYYY')
    if hour_selected
      $('#archive-time').val "#{hour_selected}:00"
    $('#from-date').val current_calendar_date

window.initializeRecordingsTab = ->
  initDatePicker()
  handleSlider()
  handleWindowResize()
  handleBodyLoadContent()
  handleMinSecDropDown()
  handlePlay()
  handleTabOpen()
  fullscreenImage()
  saveImage()
  handleResize()
  window.initScheduleCalendar()
  window.initCloudRecordingSettings()
  calendarShow()
  onClickSnapshotMagnifier()
  onClickOldestLatestImage()
  saveOldestLatestImage()
  centerTabClick()
  setImageSource()
  removeMagnifierOnEsc()
  setCreateClipDate()
