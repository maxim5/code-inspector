snapshotInfos = null
totalFrames = 0
snapshotInfoIdx = 0
currentFrameNumber = 0
BoldDays = []
playFromDateTime = null
ClearCalendarTimeOut = null
isPlaying = false
PauseAfterPlay = false
playInterval = 250
ChunkSize = 3600
normalSpeed = 1000
totalSnaps = 0
CameraOffsetMinutes = null
playDirection = 1
playStep = 1
old_frequency = null
old_storage_duration = null
old_status = null
old_schedule = null
xhrRequestChangeMonth = null
is_saving_recordings = false

initDatePicker = ->
  $("#timelapse_rec_calendar").datepicker().on("changeDate", datePickerSelect).on "changeMonth", datePickerChange
  $("#timelapse_rec_calendar table th[class*='prev']").on "click", ->
    changeMonthFromArrow('p')

  $("#timelapse_rec_calendar table th[class*='next']").on "click", ->
    changeMonthFromArrow('n')

datePickerSelect = (value)->
  dt = value.date
  $("#tr_divPointer").width(0)
  $("#tr_divSlider").width(0)
  $("#ddlRecMinutes").val(0)
  $("#ddlRecSeconds").val(0)
  $("#tr_divDisableButtons").removeClass("hide").addClass("show")
  $("#tr_divFrameMode").removeClass("show").addClass("hide")
  $("#tr_divPlayMode").removeClass("show").addClass("hide")
  hasDayRecord = false
  for j in BoldDays
    if j == dt.getDate()
      hasDayRecord = true
      break

  if hasDayRecord
    GetCameraInfo(true)
  else
    NoRecordingDayOrHour()

  ClearCalendarTimeOut = setTimeout(ResetDays, 100)

ResetDays = ->
  clearTimeout ClearCalendarTimeOut
  return unless BoldDays.length > 0
  calDays = $("#timelapse_rec_calendar table td[class*='day']")
  calDays.each (idx, el) ->
    calDay = $(this)
    if !calDay.hasClass('old') && !calDay.hasClass('new')
      iDay = parseInt(calDay.text())
      for day in BoldDays
        if day is iDay
          calDay.addClass('has-snapshot')
        else
          calDay.addClass('no-snapshot')

datePickerChange=(value)->
  d = value.date
  year = d.getFullYear()
  month = d.getMonth() + 1
  $("#timelapse_rec_calendar").datepicker('setDate', d)
  walkDaysInMonth(year, month)
  snapshotInfos = null
  snapshotInfoIdx = 1
  currentFrameNumber = 0

changeMonthFromArrow = (value) ->
  xhrRequestChangeMonth.abort()
  $("#timelapse_rec_calendar").datepicker('fill')
  d = $("#timelapse_rec_calendar").datepicker('getDate')
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
  $("#timelapse_rec_calendar").datepicker('setDate',d)
  snapshotInfos = null
  snapshotInfoIdx = 1
  currentFrameNumber = 0

walkDaysInMonth = (year, month) ->
  showDaysLoadingAnimation()
  showLoader()

  BoldDays = []
  data = {}
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key

  onError = (response, status, error) ->
    false

  onSuccess = (response, status, jqXHR) ->
    if response.days.length > 0
      for day in response.days
        HighlightDay(year, month, day, true)
    else
      NoRecordingDayOrHour()

  settings =
    cache: true
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/timelapse/recordings/snapshots/#{year}/#{month}/days"

  xhrRequestChangeMonth = $.ajax(settings)

HighlightDay = (year, month, day, exists) ->
  d = $("#timelapse_rec_calendar").datepicker('getDate')
  calendar_year = d.getFullYear()
  calendar_month = d.getMonth() + 1
  if year == calendar_year and month == calendar_month
    calDays = $("#timelapse_rec_calendar table td[class*='day']")
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
  GetCameraInfo()

hideDaysLoadingAnimation = ->
  $('#timelapse-calendar-loader').addClass 'hide'
  $('#timelapse_rec_calendar').removeClass 'opacity-transparent'

showDaysLoadingAnimation = ->
  $('#timelapse-calendar-loader').removeClass 'hide'
  $('#timelapse_rec_calendar').addClass 'opacity-transparent'

HighlightCurrentMonth = ->
  d = $("#timelapse_rec_calendar").datepicker('getDate')
  year = d.getFullYear()
  month = d.getMonth() + 1
  walkDaysInMonth(year, month)

SetInfoMessage = (currFrame, date_time) ->
  $("#tr_divInfo").fadeIn()
  $("#tr_divInfo").html("<span class='snapshot-frame'>#{currFrame} of #{totalSnaps}</span> <span class='snapshot-date'>#{date_time}</span>")

  totalWidth = $("#tr_divSlider").width()
  $("#tr_divPointer").width(totalWidth * currFrame / totalFrames)
  url = "#{Evercam.request.rootpath}/timelapse/recordings/snapshots/#{moment.utc(date_time).toISOString()}"

  if $(".nav-tabs li.active a").html() is "Timelapse Recordings" && history.replaceState
    window.history.replaceState({}, '', url)

shortDate = (date) ->
  moment.tz(date*1000, Evercam.Camera.timezone).format('/DDMM/YYYY HH:mm:ss')

FormatNumTo2 = (n) ->
  if n < 10
    "0#{n}"
  else
    n

GetCameraInfo = (isShowLoader) ->
  NProgress.start()
  $("#tr_divDisableButtons").removeClass("hide").addClass("show")
  $("#tr_divFrameMode").removeClass("show").addClass("hide")
  $("#tr_divPlayMode").removeClass("show").addClass("hide")
  if isShowLoader
    showLoader()
  date = $("#timelapse_rec_calendar").datepicker('getDate')
  year = date.getFullYear()
  month = date.getMonth() + 1
  day = date.getDate()

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
      $("#tr_divPointer").hide()
    else
      $("#tr_divPointer").show()
    if response == null || response.snapshots.length == 0
      NoRecordingDayOrHour()
    else
      $("#tr_divDisableButtons").removeClass("show").addClass("hide")
      $("#tr_divFrameMode").removeClass("hide").addClass("show")
      iterations = Math.ceil(totalSnaps / ChunkSize)
      sliderpercentage = Math.ceil(100 / iterations)

      if sliderpercentage > 100
        sliderpercentage = 100
      $("#tr_divSlider").width("#{sliderpercentage}%")
      $("#tr_divSliderBackground").width("100%")
      currentFrameNumber = 1
      frameDateTime = moment.tz(snapshotInfos[snapshotInfoIdx].created_at*1000, Evercam.Camera.timezone).format('DD/MM/YYYY HH:mm:ss')
      snapshotTimeStamp = snapshotInfos[snapshotInfoIdx].created_at

      if playFromDateTime isnt null
        snapshotTimeStamp = SetPlayFromImage playFromTimeStamp
        frameDateTime = new Date(moment(snapshotTimeStamp*1000)
        .format('MM/DD/YYYY HH:mm:ss'))
        if currentFrameNumber isnt 1
          playFromDateTime = null
          playFromTimeStamp = null

      currentDate = $("#camera_selected_time").val()
      currentHour = parseInt($("#camera_current_time").val())
      dt = $("#timelapse_rec_calendar").datepicker('getDate')
      current_camera_date = moment(dt).format('MM/DD/YYYY')
      SetInfoMessage(currentFrameNumber, frameDateTime)
      loadImage(snapshotTimeStamp)
    NProgress.done()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json charset=utf-8"
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/timelapse/recordings/snapshots/#{year}/#{month}/#{day}"

  $.ajax(settings)

loadImage = (timestamp) ->
  data = {}
  data.api_id = Evercam.User.api_id
  data.api_key = Evercam.User.api_key

  onError = (jqXHR, status, error) ->
    checkCalendarDisplay()

  onSuccess = (response) ->
    if response.snapshots.length > 0
      $("#tr_snapshot_tab_save").show()
      $("#tr_imgPlayback").attr("src", response.snapshots[0].data)
      if $("#tr-snapshot-magnifier").hasClass 'enabled'
        initElevateZoom()
    HideLoader()
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
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/timelapse/recordings/snapshots/#{timestamp}"

  $.ajax(settings)

showImageSaveOption = ->
  $('#tr_snapshot_tab_save').removeClass('hide')

HideImageSaveOption = ->
  $('#tr_snapshot_tab_save').addClass('hide')

opBack = ->
  $('#tr_snapshot_tab_save').css('display','inline')

checkCalendarDisplay = ->
  if $('#timelapse-recording-tab .col-recording-right').css('display') == 'none'
    $('#timelapse-recording-tab .left-column').animate { width: "99.4%" }, ->
      recodringSnapshotDivHeight()
  else
    calculateWidth()
    recodringSnapshotDivHeight()

calculateWidth = ->
  deviceAgent = navigator.userAgent.toLowerCase()
  agentID = deviceAgent.match(/(iPad|iPhone|iPod)/i)
  is_widget = $('#snapshot-diff').val()
  tab_width = $("#timelapse-recording-tab").width()
  right_column_width = $("#timelapse-recording-tab .right-column").width()
  if tab_width is 0
    width_add = if !is_widget then 10 else 30
    tab_width = $(".tab-content").width() + width_add
  width_remove = if !is_widget then 40 else 20
  left_col_width = tab_width - right_column_width - width_remove
  if left_col_width > 360
    $("#tr-navbar-section .playback-info-bar").css("text-align", "center")
  else
    $("#tr-navbar-section .playback-info-bar").css("text-align", "left")
  if (agentID)
    if tab_width > 480
      $("#timelapse-recording-tab .left-column").css("width", "#{left_col_width}px")
      $("#timelapse-recording-tab .right-column").css("width", "220px")
    else
      $("#timelapse-recording-tab .left-column").css("width", "100%")
      $("#timelapse-recording-tab .right-column").css("width", "220px")
      $("#tr-navbar-section .playback-info-bar").css("text-align", "center")
  else
    if $(window).width() >= 490
      $("#timelapse-recording-tab .left-column").animate { width:
        "#{left_col_width}px" }, ->
          if $("#timelapse-recording-tab .left-column").width() is 0
            setTimeout(calculateWidth, 500)
          recodringSnapshotDivHeight()
      $("#timelapse-recording-tab .right-column").css("width", "220px")
    else
      $("#timelapse-recording-tab .left-column").css("width", "100%")
      $("#timelapse-recording-tab .right-column").css("width", "220px")
      $("#tr-navbar-section .playback-info-bar").css("text-align", "center")

recodringSnapshotDivHeight = ->
  deviceAgent = navigator.userAgent.toLowerCase()
  agentID = deviceAgent.match(/(iPad|iPhone|iPod)/i)
  if !(agentID)
    if $(window).width() >= 490
      tab_width = $("#timelapse-recording-tab").width()
      calcuHeight = $(window).innerHeight() - $('.center-tabs').height() - $("#tr-navbar-section").height()
      if $('#tr_fullscreen > img').height() < calcuHeight
        $("#tr-navbar-section").removeClass 'navbar-section'
        $("#tr-navbar-section").css 'width', $('#timelapse-recording-tab .left-column').width()
      else
        $("#tr-navbar-section").addClass 'navbar-section'
        $("#tr-navbar-section").css 'width', $('#timelapse-recording-tab .left-column').width()
    else
      $("#tr-navbar-section").removeClass 'navbar-section'
      $("#tr-navbar-section").css("width", "100%")
  if tab_width is 0
    setTimeout (-> recodringSnapshotDivHeight()), 500

DoNextImg = ->
  return if totalFrames is 0
  if snapshotInfos.length is snapshotInfoIdx
    Pause()
    currentFrameNumber = snapshotInfos.length
    snapshotInfoIdx = snapshotInfos.length - 1
  snapshot = snapshotInfos[snapshotInfoIdx]

  data = {}
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
      frameDateTime = moment.tz(snapshot.created_at*1000, Evercam.Camera.timezone).format('DD/MM/YYYY HH:mm:ss')
      SetInfoMessage(currentFrameNumber, frameDateTime)
    $("#tr_imgPlayback").attr("src", response.snapshots[0].data)

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
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/timelapse/recordings/snapshots/#{snapshot.created_at}"

  sendAJAXRequest(settings)

Pause = ->
  isPlaying = false
  $("#tr_divFrameMode").removeClass("hide").addClass("show")
  $("#tr_divPlayMode").removeClass("show").addClass("hide")
  PauseAfterPlay = true

handlePlay = ->
  $("#tr_btnPlayRec").on "click", ->
    return if totalFrames is 0

    playDirection = 1
    playStep = 1
    $("#tr_divFrameMode").removeClass("show").addClass("hide")
    $("#tr_divPlayMode").removeClass("hide").addClass("show")
    isPlaying = true
    if snapshotInfos.length is snapshotInfoIdx + 1
      snapshotInfoIdx = 0
      currentFrameNumber = 1
    DoNextImg()

  $("#tr_btnPauseRec").on "click", ->
    Pause()

  $("#tr_btnFRwd").on "click", ->
    SetPlaySpeed 10, -1

  $("#tr_btnRwd").on "click", ->
    SetPlaySpeed 5, -1

  $("#tr_btnFFwd").on "click", ->
    SetPlaySpeed 10, 1

  $("#tr_btnFwd").on "click", ->
    SetPlaySpeed 5, 1

  $(".tr_skipframe").on "click", ->
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

UpdateSnapshotRec = (snapInfo) ->
  showLoader()
  frameDateTime = moment.tz(snapInfo.created_at*1000, Evercam.Camera.timezone).format('MM/DD/YYYY HH:mm:ss')
  SetInfoMessage(currentFrameNumber, frameDateTime)
  loadImage(snapInfo.created_at)

SetPlaySpeed = (step, direction) ->
  playDirection = direction
  playStep = step

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

handleSlider = ->
  onSliderMouseMove = (ev) ->
    if snapshotInfos == null || snapshotInfos.length == 0 then return

    sliderStartX = $("#tr_divSlider").offset().left
    sliderEndX = sliderStartX + $("#tr_divSlider").width()

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
    $("#tr_popup").html("Frame #{frameNo}, #{shortDate(snapshotInfos[idx].created_at)}")
    $("#tr_popup").show()
    $("#tr_popup").offset({ top: ev.pageY + 20, left: x })

    $("#tr_divSlider").css('background-position', "#{(ev.pageX - sliderStartX)}px 0px")
    $("#tr_divPointer").css('background-position', "#{(ev.pageX - sliderStartX)}px 0px")
  $("#tr_divSlider").mousemove(onSliderMouseMove)

  onSliderMouseOut = ->
    $("#tr_popup").hide()
    $("#tr_divSlider").css('background-position', '-3px 0px')
    $("#tr_divPointer").css('background-position', '-3px 0px')

  $("#tr_divSlider").mouseout(onSliderMouseOut)

  onSliderClick = (ev) ->
    sliderStartX = $("#tr_divSlider").offset().left
    sliderEndX = sliderStartX + $("#tr_divSlider").width()
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

  $("#tr_divSlider").click(onSliderClick)

showLoader = ->
  if $('#timelapse-recording-tab').width() is 0
    $("#timelapse_imgloader").hide()
  else
    if $("#tr_imgPlayback").attr("src").indexOf('nosnapshots') != -1
      $("#tr_imgPlayback").attr("src","/assets/plain.png")
    $("#timelapse_imgloader").width($('#timelapse-recording-tab .left-column').width())
    $("#timelapse_imgloader").height($('#tr_imgPlayback').height())
    $("#timelapse_imgloader").css("top", $('#tr_imgPlayback').css('top'))
    $("#timelapse_imgloader").css("left", $('#tr_imgPlayback').css('left'))
    $("#timelapse_imgloader").show()

HideLoader = ->
  $("#timelapse_imgloader").hide()

NoRecordingDayOrHour = ->
  showLoader()
  $("#tr_imgPlayback").attr("src", "/assets/nosnapshots.svg")
  $("#tr_divInfo").fadeOut()
  $("#tr_divPointer").width(0)
  $("#tr_divSliderBackground").width(0)
  hideDaysLoadingAnimation()
  $("#tr_snapshot_tab_save").hide()
  totalFrames = 0
  HideLoader()

onClickSnapshotMagnifier = ->
  $('#tr-snapshot-magnifier').on 'click', ->
    $('.zoomContainer').remove()
    if $('.enabled').length == 0
      initElevateZoom()
      $(this).toggleClass 'enabled'
    else
      $(this).toggleClass 'enabled'
      $('.zoomContainer').hide()

turnOffZoomEffect = ->
  $('#tr-snapshot-magnifier').removeClass 'enabled'
  $('.zoomContainer').hide()

initElevateZoom = ->
  $('#tr_imgPlayback').elevateZoom
    zoomType: 'lens',
    scrollZoom: true,
    lensShape: 'round',
    lensSize: 230

handleResize = ->
  calculateWidth()
  recodringSnapshotDivHeight()
  $(window).resize ->
    checkCalendarDisplay()
    turnOffZoomEffect()

getSnapshotDate = (date) ->
  dt = $("#timelapse_rec_calendar").datepicker('getDate')
  moment.utc([dt.getFullYear(), dt.getMonth(), dt.getDate(), dt.getHours(), date.getMinutes(), date.getSeconds(), date.getMilliseconds()])

saveImage = ->
  $('#tr-save-recording-image').on 'click', ->
    date_time = new Date(snapshotInfos[snapshotInfoIdx].created_at*1000)
    download($("#tr_imgPlayback").attr('src'), "#{Evercam.Camera.id}-#{getSnapshotDate(date_time).toISOString()}.jpg", "image/jpg")
    $('#timelapse-recording-tab .play-options').css('display','none')
    setTimeout opBack , 1500

handleBodyLoadContent = ->
  offset = $('#camera_time_offset').val()
  CameraOffset = parseInt(offset)/3600
  CameraOffsetHours = Math.floor(CameraOffset)
  CameraOffsetMinutes = 60 *(CameraOffset - CameraOffsetHours)
  currentDate = new Date($("#camera_selected_time").val())
  cameraCurrentHour = currentDate.getHours()

  timestamp = getTimestampFromUrl()
  # if timestamp isnt ""
  #   playFromTimeStamp = moment.utc(timestamp)/1000
  #   playFromDateTime = new Date(moment.utc(timestamp)
  #   .format('MM/DD/YYYY HH:mm:ss'))
  #   playFromDateTime.setHours(playFromDateTime.getHours() + CameraOffsetHours)
  #   playFromDateTime
  #   .setMinutes(playFromDateTime.getMinutes() + CameraOffsetMinutes)
  #   currentDate = playFromDateTime
  #   cameraCurrentHour = currentDate.getHours()
  #   $("#timelapse_rec_calendar").datepicker('update', currentDate)

  # $("#tdI#{cameraCurrentHour}").addClass("active")
  # PreviousImageHour = "tdI#{cameraCurrentHour}"
  # $("#timelapse_rec_calendar").datepicker('setDate', currentDate)
  # selectCurrentDay()
  $(".btn-group").tooltip()

  showLoader()
  HighlightCurrentMonth()
  GetCameraInfo()

handleTabOpen = ->
  $('.nav-tab-timelapse-recordings').on 'shown.bs.tab', ->
    undo_recordings()
    initScheduleCalendar()
    initTimelapseRecordings()
    if snapshotInfos isnt null && snapshotInfos.length > 0
      date_time = new Date(snapshotInfos[snapshotInfoIdx].created_at*1000)
      url = "#{Evercam.request.rootpath}/timelapse/recordings/snapshots/#{moment.utc(date_time).toISOString()}"
      if history.replaceState
        window.history.replaceState({}, '', url)

checkCalendarDisplay = ->
  if $('#timelapse-recording-tab .col-recording-right').css('display') == 'none'
    $('#timelapse-recording-tab .left-column').animate { width: "99.4%" }, ->
      recodringSnapshotDivHeight()
  else
    calculateWidth()
    recodringSnapshotDivHeight()

calendarShow = ->
  $('#timelapse-recording-tab .ui-datepicker-trigger').on 'click', ->
    $('#timelapse-recording-tab .col-recording-right').toggle 'slow', ->
      $('#tr_calendar .fas').css 'color', 'white'
      checkCalendarDisplay()
    $('#tr_calendar .fas').css 'color', '#68a2d5'
    turnOffZoomEffect()

fullscreenImage = ->
  $("#tr_imgPlayback").dblclick ->
    screenfull.toggle $(this)[0]

  if screenfull.enabled
    document.addEventListener screenfull.raw.fullscreenchange, ->
      if screenfull.isFullscreen
        $("#tr_imgPlayback").css('width','auto')
      else
        $("#tr_imgPlayback").css('width','100%')

#****************Timelapse Recordings******************************************************

initScheduleCalendar = ->
  window.timelapseScheduleCalendar = $('#tr-recording-calendar').fullCalendar
    axisFormat: 'HH'
    defaultView: 'agendaWeek'
    allDaySlot: false
    slotDuration: '00:60:00'
    columnFormat: 'ddd'
    defaultDate: '1970-01-01'
    dayNamesShort: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
    editable: true
    eventClick: (event, element) ->
      event.preventDefault
      if (window.confirm("Are you sure you want to delete this event?"))
        timelapseScheduleCalendar.fullCalendar('removeEvents', event._id)
        updateScheduleFromCalendar()
    eventDrop: (event) ->
      updateScheduleFromCalendar()
    eventResize: (event) ->
      updateScheduleFromCalendar()
    eventLimit: true
    eventOverlap: false
    eventAfterRender: (event, $el, view ) ->
      start = moment(event.start).format('HH:mm')
      end = moment(event.end).format('HH:mm')
      $el.find(".fc-bg").text(start + "-" + end)
    eventColor: '#458CC7'
    firstDay: 1
    height: 'auto'
    select: (start, end) ->
      # TODO: select whole day range when allDaySlot is selected
      timelapseScheduleCalendar.fullCalendar('renderEvent',
        start: start
        end: end - 1
      , true)
      timelapseScheduleCalendar.fullCalendar('unselect')
      updateScheduleFromCalendar()
    selectHelper: true
    selectable: true
    timezone: 'local'

updateScheduleFromCalendar = ->
  $('#tr-select-schedule-presets').val("None")
  Evercam.Camera.timelapse_recording.schedule = parseCalendar()
  Evercam.Camera.timelapse_recording.frequency =
    $("#tr-recording-frequency").val()
  Evercam.Camera.timelapse_recording.storage_duration =
    $("#tr-recording-duration").val()

updateScheduleToOn = ->
  Evercam.Camera.timelapse_recording.schedule = fullWeekSchedule
  Evercam.Camera.timelapse_recording.frequency =
    $("#tr-recording-frequency").val()
  Evercam.Camera.timelapse_recording.storage_duration =
    $("#tr-recording-duration").val()

parseCalendar = ->
  events = $('#tr-recording-calendar').fullCalendar('clientEvents')
  schedule =
    'Monday': []
    'Tuesday': []
    'Wednesday': []
    'Thursday': []
    'Friday': []
    'Saturday': []
    'Sunday': []
  _.forEach events, (event) ->
    startTime = "#{moment(event.start).get('hours')}:#{moment(event.start).get('minutes')}"
    endTime = "#{moment(event.end).get('hours')}:#{moment(event.end).get('minutes')}"
    day = moment(event.start).format('dddd')
    schedule[day] = schedule[day].concat("#{startTime}-#{endTime}")
  schedule

perfomAction = ->
  status = $('input[name=tr_recording]:checked').val()
  switch status
    when "on","paused"
      updateScheduleToOn()
    when "on-scheduled"
      updateSchedulePresets()

handleFrequencySelect = ->
  $("#tr-recording-frequency").off("change").on "change" , (event) ->
    perfomAction()

handleDurationSelect = ->
  $("#tr-recording-duration").off("change").on "change" , (event) ->
    perfomAction()

handleStatusSelect = ->
  $("#tr-recording-toggle input").off("ifChecked").on "ifChecked", (event) ->
    status = Evercam.Camera.timelapse_recording.status
    storage = Evercam.Camera.timelapse_recording.storage_duration
    Evercam.Camera.timelapse_recording.status = $(this).val()
    switch $(this).val()
      when "on"
        hideScheduleCalendar()
        showFrequencySelect()
        hidePresetSelect()
        showDurationSelect()
        updateFrequencyTo60()
        updateScheduleToOn()
      when "on-scheduled"
        showScheduleCalendar()
        showFrequencySelect()
        showPresetSelect()
        showDurationSelect()
        updateFrequencyTo60()
        updateScheduleToOn()
      when "off"
        hideFrequencySelect()
        hidePresetSelect()
        hideDurationSelect()
        hideScheduleCalendar()
        updateScheduleToOff()

renderCloudRecordingDuration = ->
  $("#tr-recording-duration").val(Evercam.Camera.timelapse_recording.storage_duration)
  recording_duration_value = $("#tr-recording-duration :selected").text()
  if Evercam.Camera.timelapse_recording.status is "off"
    $("#timelapse-recording-tab .tr_recording-text .tr_storage").hide('slow')
    $("#timelapse-recording-tab.tr_recording-text .tr_off-status").text('Off')
  else if Evercam.Camera.timelapse_recording.status is "on"
    $("#timelapse-recording-tab .tr_recording-text .tr_off-status").text('Continuous')
    $("#timelapse-recording-tab .tr_recording-text .tr_storage").show('slow')
    $("#timelapse-recording-tab .tr_storage-duration").text(recording_duration_value)
  else if Evercam.Camera.timelapse_recording.status is "paused"
    $("#timelapse-recording-tab .tr_recording-text .tr_off-status").text('Paused')
    $("#timelapse-recording-tab .tr_storage-duration").text(recording_duration_value)
  else
    $("#timelapse-recording-tab .tr_recording-text .off-status").text('On Schedule')
    $("#timelapse-recording-tab .tr_recording-text .tr_storage").show('slow')
    $("#timelapse-recording-tab .tr_storage-duration").text(recording_duration_value)

renderCloudRecordingFrequency = ->
  $("#tr-recording-frequency").val(Evercam.Camera.timelapse_recording.frequency)
  timelapse_recording_frequency_value = $("#tr-recording-frequency :selected").text()
  if Evercam.Camera.timelapse_recording.status is "off"
    $("#timelapse-recording-tab .tr_recording-text .tr_frequency-text").hide('slow')
    $("#timelapse-recording-tab .tr_recording-text .tr_off-status").text('Off')
  else if Evercam.Camera.timelapse_recording.status is "on"
    $("#timelapse-recording-tab .tr_recording-text .tr_off-status").text('Continuous')
    $("#timelapse-recording-tab .tr_recording-text .tr_frequency-text").show('slow')
    $("#timelapse-recording-tab .tr_recording-frequency").text(timelapse_recording_frequency_value)
  else if Evercam.Camera.timelapse_recording.status is "paused"
    $("#timelapse-recording-tab .tr_recording-text .tr_off-status").text('Paused')
    $("#timelapse-recording-tab .tr_recording-frequency").text(timelapse_recording_frequency_value)
  else
    $("#timelapse-recording-tab .tr_recording-text .tr_off-status").text('On Schedule')
    $("#timelapse-recording-tab .tr_recording-text .tr_frequency-text").show('slow')
    $("#timelapse-recording-tab .tr_recording-frequency").text(timelapse_recording_frequency_value)

renderCloudRecordingStatus = ->
  switch Evercam.Camera.timelapse_recording.status
    when "on"
      $("#tr-recording-on").iCheck('check')
      showFrequencySelect()
      hidePresetSelect()
      showDurationSelect()
      hideScheduleCalendar()
      renderCloudRecordingDuration()
      renderCloudRecordingFrequency()
      showSchedulePresetValue()
    when "on-scheduled"
      $("#tr-recording-on-scheduled").iCheck('check')
      showScheduleCalendar()
      showFrequencySelect()
      showPresetSelect()
      showDurationSelect()
      renderCloudRecordingDuration()
      renderCloudRecordingFrequency()
      showSchedulePresetValue()
    when "off"
      $("#tr-recording-off").iCheck('check')
      hideScheduleCalendar()
      hideFrequencySelect()
      hidePresetSelect()
      hideDurationSelect()
      showSchedulePresetValue()
    when "paused"
      $("#tr-recording-paused").iCheck('check')
      showSchedulePresetValue()

updateScheduleToOff = ->
  Evercam.Camera.timelapse_recording.schedule = fullWeekSchedule
  status = "off"

updateFrequencyTo60 = ->
  $("#cloud-recording-frequency").val(60)

hideDurationSelect = ->
  $('#tr-recording-duration-wrap').hide('slow')

hideFrequencySelect = ->
  $('#tr-recording-frequency-wrap').hide('slow')

showPresetSelect = ->
  $('#tr-schedule-recording-presets').show('slow')

showFrequencySelect = ->
  $('#tr-recording-frequency-wrap').show('slow')

hidePresetSelect = ->
  $('#tr-schedule-recording-presets').hide('slow')

showDurationSelect = ->
  $('#tr-recording-duration-wrap').show('slow')
  if Evercam.Camera.timelapse_recording.storage_duration is -1
    $('#tr-recording-duration').attr('disabled',true)

hideScheduleCalendar = ->
  $('#tr-calendarfull-wrap').hide('slow')
  $("#uniform-tr-recording-on-scheduled div").removeClass 'checked'

showSchedulePresetValue = ->
  schedule_preset_value = Evercam.Camera.timelapse_recording.schedule
  switch true
    when _.isEqual(schedule_preset_value, workingDaySchedule)
      $("#tr-select-schedule-presets").val("MF_24_hr")
    when _.isEqual(schedule_preset_value, siteWorkingSchedule)
      $("#tr-select-schedule-presets").val("MF_Site_Working")
    when _.isEqual(schedule_preset_value, siteWorkingSaturdaySchedule)
      $("#tr-select-schedule-presets").val("MF_Site_Working_Sat")
    when _.isEqual(schedule_preset_value, officeClosedSchedule)
      $("#tr-select-schedule-presets").val("MF_Sat_sun_off")
    else
      $("#tr-select-schedule-presets").val("None")

showScheduleCalendar = ->
  $("#uniform-tr-recording-on-scheduled div").addClass 'checked'
  $("#timelapse-recording-tab .setting-schedule .modal-dialog").animate { "margin-top": "7%" }
  $('#tr-calendarfull-wrap').show('slow')
  timelapseScheduleCalendar.fullCalendar('render')
  if timelapseScheduleCalendar.is(':visible')
    renderEvents()

renderEvents = ->
  schedule = Evercam.Camera.timelapse_recording.schedule
  days = _.keys(schedule)
  calendarWeek = currentCalendarWeek()

  _.forEach days, (weekDay) ->
    day = schedule[weekDay]
    unless day.length == 0
      _.forEach day, (event) ->
        start = event.split("-")[0]
        end = event.split("-")[1]
        event =
          start: moment("#{calendarWeek[weekDay]} #{start}", "YYYY-MM-DD HH:mm")
          end: moment("#{calendarWeek[weekDay]} #{end}", "YYYY-MM-DD HH:mm")
        timelapseScheduleCalendar.fullCalendar('renderEvent', event, true)

currentCalendarWeek = ->
  calendarWeek = {}
  weekStart = timelapseScheduleCalendar.fullCalendar('getView').start
  weekEnd = timelapseScheduleCalendar.fullCalendar('getView').end
  day = weekStart
  while day.isBefore(weekEnd)
    weekDay = day.format("dddd")
    calendarWeek[weekDay] = day.format('YYYY-MM-DD')
    day.add 1, 'days'
  calendarWeek

window.fullWeekSchedule =
  "Monday": ["00:00-23:59"]
  "Tuesday": ["00:00-23:59"]
  "Wednesday": ["00:00-23:59"]
  "Thursday": ["00:00-23:59"]
  "Friday": ["00:00-23:59"]
  "Saturday": ["00:00-23:59"]
  "Sunday": ["00:00-23:59"]

window.workingDaySchedule =
  "Monday": ["0:0-23:59"]
  "Tuesday": ["0:0-23:59"]
  "Wednesday": ["0:0-23:59"]
  "Thursday": ["0:0-23:59"]
  "Friday": ["0:0-23:59"]
  "Saturday": []
  "Sunday": []

window.siteWorkingSchedule =
  "Monday": ["8:0-18:0"]
  "Tuesday": ["8:0-18:0"]
  "Wednesday": ["8:0-18:0"]
  "Thursday": ["8:0-18:0"]
  "Friday": ["8:0-18:0"]
  "Saturday": []
  "Sunday": []

window.siteWorkingSaturdaySchedule =
  "Monday": ["8:0-18:0"]
  "Tuesday": ["8:0-18:0"]
  "Wednesday": ["8:0-18:0"]
  "Thursday": ["8:0-18:0"]
  "Friday": ["8:0-18:0"]
  "Saturday": ["8:0-14:0"]
  "Sunday": []

window.officeClosedSchedule =
  "Monday": ["0:0-8:0","18:0-23:59"]
  "Tuesday": ["0:0-8:0","18:0-23:59"]
  "Wednesday": ["0:0-8:0","18:0-23:59"]
  "Thursday": ["0:0-8:0","18:0-23:59"]
  "Friday": ["0:0-8:0","18:0-23:59"]
  "Saturday": ["0:0-23:59"]
  "Sunday": ["0:0-23:59"]

showEditButton = ->
  $('#tr-show-schedule-calendar').off('click').on 'click' , ->
    if Evercam.Camera.timelapse_recording.status is "on-scheduled"
      setTimeout showScheduleCalendar, 5
    editScheduleCalendar()

editScheduleCalendar = ->
  $('#timelapse-recording-calendar-wrap').removeClass('hide' , 'fade')
  $('#timelapse-recording-calendar-wrap').addClass('fade in')
  $('#timelapse-recording-tab .setting-schedule').show()
  $(document).click (event) ->
    if $(event.target).closest('.modal-content').get(0) == null
      $('#timelapse-recording-tab .setting-schedule').hide()
    return

updateSchedulePresets = ->
  schedule_preset = $("#tr-select-schedule-presets").val()
  switch schedule_preset
    when "None"
      localStorage.setItem 'todoData', @value
      updateScheduleFromCalendar()
    when "MF_24_hr"
      reRenderCalendarEvents(workingDaySchedule)
    when "MF_Site_Working"
      reRenderCalendarEvents(siteWorkingSchedule)
    when "MF_Site_Working_Sat"
      reRenderCalendarEvents(siteWorkingSaturdaySchedule)
    when "MF_Sat_sun_off"
      reRenderCalendarEvents(officeClosedSchedule)

reRenderCalendarEvents = (preset_schedule) ->
  $('#tr-recording-calendar').fullCalendar('destroy')
  Evercam.Camera.timelapse_recording.schedule = preset_schedule
  Evercam.Camera.timelapse_recording.frequency =
    $("#tr-recording-frequency").val()
  Evercam.Camera.timelapse_recording.storage_duration =
    $("#tr-recording-duration").val()
  initScheduleCalendar()
  renderEvents()

saveScheduleSettings = ->
  $("#timelapse-recording-tab .tr-schedule-save").off('click').on 'click', ->
    new_tr_values = Evercam.Camera.timelapse_recording
    new_frequency = parseInt(new_tr_values.frequency)
    new_storage = parseInt(new_tr_values.storage_duration)
    new_status = new_tr_values.status
    new_schedule = new_tr_values.schedule

    if new_frequency isnt old_frequency or
       new_storage isnt old_storage_duration or
       new_status isnt old_status or
       is_equal_schedule(new_schedule, old_schedule) isnt true
      updateSchedule()
      is_saving_recordings = true
    $('#timelapse-recording-calendar-wrap').modal('hide')

is_equal_schedule = (schedule, original) ->
  if "#{schedule["Monday"]}".trim() is "#{original["Monday"]}".trim() and
     "#{schedule["Tuesday"]}".trim() is "#{original["Tuesday"]}".trim() and
     "#{schedule["Wednesday"]}".trim() is "#{original["Wednesday"]}".trim() and
     "#{schedule["Thursday"]}".trim() is "#{original["Thursday"]}".trim() and
     "#{schedule["Friday"]}".trim() is "#{original["Friday"]}".trim() and
     "#{schedule["Saturday"]}".trim() is "#{original["Saturday"]}".trim() and
     "#{schedule["Sunday"]}".trim() is "#{original["Sunday"]}".trim()
    return true
  else
    return false

updateSchedule = ->
  NProgress.start()
  data =
    api_id: Evercam.User.api_id
    api_key: Evercam.User.api_key
    frequency: Evercam.Camera.timelapse_recording.frequency
    storage_duration: Evercam.Camera.timelapse_recording.storage_duration
    status: Evercam.Camera.timelapse_recording.status
    schedule: JSON.stringify(Evercam.Camera.timelapse_recording.schedule)

  onError = (data) ->
    switch data.status
      when 403
        showFeedback("You aren't authorized to change the scheduling for camera '#{Evercam.Camera.id}'.")
      else
        showFeedback("Updating recording settings has failed. Please contact support.")
    NProgress.done()

  onSuccess = (data) ->
    duration = JSON.parse(data).timelapse_recordings[0].storage_duration
    frequency = JSON.parse(data).timelapse_recordings[0].frequency
    status = JSON.parse(data).timelapse_recordings[0].status
    Evercam.Camera.timelapse_recording.storage_duration = duration
    Evercam.Camera.timelapse_recording.frequency = frequency
    Evercam.Camera.timelapse_recording.status = status
    $('#tr-recording-duration').val(duration)
    renderCloudRecordingDuration()
    renderCloudRecordingFrequency()
    $('#tr-recording-duration').prop("disabled", false)
    showFeedback("Timelapse recording schedule was successfully updated.")
    saveOldCRValues()
    NProgress.done()

  settings =
    error: onError
    success: onSuccess
    data: data
    dataType: 'text'
    contentType: "application/x-www-form-urlencoded"
    type: "POST"
    url: "#{Evercam.API_URL}cameras/#{Evercam.Camera.id}/apps/timelapse-recording"

  $.ajax(settings)

saveOldCRValues = ->
  old_frequency = Evercam.Camera.timelapse_recording.frequency
  old_storage_duration = Evercam.Camera.timelapse_recording.storage_duration
  old_status = Evercam.Camera.timelapse_recording.status
  old_schedule = Evercam.Camera.timelapse_recording.schedule

undo_recordings = ->
  Evercam.Camera.timelapse_recording.schedule = old_schedule
  Evercam.Camera.timelapse_recording.frequency = old_frequency
  Evercam.Camera.timelapse_recording.storage_duration = old_storage_duration
  Evercam.Camera.timelapse_recording.status = old_status

on_hide_modal = ->
  $("#timelapse-recording-calendar-wrap").on "hide.bs.modal", ->
    unless is_saving_recordings
      undo_recordings()
      initScheduleCalendar()
      initTimelapseRecordings()

initTimelapseRecordings = ->
  saveOldCRValues()
  initScheduleCalendar()
  renderCloudRecordingDuration()
  renderCloudRecordingFrequency()
  renderCloudRecordingStatus()
  showEditButton()
  handleStatusSelect()
  $('#tr-select-schedule-presets').change(updateSchedulePresets)
  saveScheduleSettings()

#******************************************************************************************

window.initializeTimelapseRecordingsTab = ->
  initDatePicker()
  handleBodyLoadContent()
  handleSlider()
  handlePlay()
  handleResize()
  onClickSnapshotMagnifier()
  initTimelapseRecordings()
  handleFrequencySelect()
  handleDurationSelect()
  handleTabOpen()
  saveImage()
  on_hide_modal()
  calendarShow()
  fullscreenImage()
