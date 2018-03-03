format_time = null
Evercam_API_URL = 'https://api.evercam.io/v1/'

handleEditable = ->
  $('.makeNonEditable').on 'click', ->
    $('#userProfile input:text').attr 'readonly', 'readonly'
    $('select').attr 'disabled', 'disabled'
    return
  $('.makeEditable').on 'click', ->
    $('#userProfile input:text').removeAttr 'readonly'
    $('select').removeAttr 'disabled'
    return

showHideMessage = ->
  $('#hide').click ->
    $('.hide-p').fadeOut()

  $('#hide-2').click ->
    $('.hide-p').fadeOut()

  $('#show').click ->
    $('.hide-p').fadeIn()


handlePasswordChange = ->
  $('#change-password').on 'click', ->
    NProgress.start()
    if $('#new-password').val() != $('#password_again').val()
      $('#wrong-confirm-password').show()
      $('#password_again').addClass 'border-red'
      NProgress.done()
      return false
    $('#wrong-confirm-password').hide()
    NProgress.done()
    $('#password_again').removeClass 'border-red'
    true

initializeiCheck = ->
  $("input[type=radio], input[type=checkbox]").iCheck
    checkboxClass: "icheckbox_flat-blue"
    radioClass: "iradio_flat-blue"

onDeleteClick = ->
  $("#close-account").on 'click', ->
    is_checked = true
    $(".camera-delete").each ->
      if !$(this).is(":checked")
        is_checked = false
        return
    if !is_checked
      $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
      Notification.show "Please tick the box(es) to confirm you would like to delete each of your cameras"
      return false
    if $("#delete-camera").val().toLowerCase() isnt 'delete'
      $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
      Notification.show "Please type 'Delete' to confirm delete your account"
      return false
    return true

saveUserSettings = ->
  if $.cookie("hide-offline-cameras")
    $("#hide-offline-cameras").prop("checked", true)
  $("#hide-offline-cameras").on "click", ->
    hide_cameras = $(this).prop("checked")
    if hide_cameras
      $.cookie("hide-offline-cameras", $(this).prop("checked"), { expires: 365, path: "/" })
    else
      $.removeCookie("hide-offline-cameras", { path: "/" })

initLiveCameraSelect = ->
  $('#api-call-camera').select2
    templateSelection: widgetFormat
    templateResult: widgetFormat
    escapeMarkup: (m) ->
      m
  $('.api-call-input .select2-container--default').width '100%'

initRecordedCameraSelect = ->
  $('#recorded-call-camera').select2
    templateSelection: widgetFormat
    templateResult: widgetFormat
    escapeMarkup: (m) ->
      m
  $('.api-call-input .select2-container--default').width '100%'

widgetFormat = (state) ->
  is_offline = undefined
  is_offline = ''
  if !state.id
    return state.text
  if state.id == '0'
    return state.text
  $ '<span><img style=\'height:30px;margin-bottom:1px;margin-top:1px;width:35px;\' src=\'' + state.element.attributes[2].value + '\' class=\'img-flag\' />&nbsp;' + state.text + '</span>'

updateLiveSnapshotUrl =->
  camera_name = $('#api-call-camera').val()
  $('#live-snapshot-url').val("#{Evercam_API_URL}cameras/#{camera_name}/live/snapshot?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}")
  $("#live-api-url").attr 'href', "#{Evercam_API_URL}cameras/#{camera_name}/live/snapshot?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"
  $("#snapshot-api-url").attr 'href', "https://dash.evercam.io/v1/cameras/#{camera_name}/live/snapshot?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"
  $('#live-snapshot-dashboard-url').val("https://dash.evercam.io/v1/cameras/#{camera_name}/live/snapshot?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}")

updateRecordedSnapshotUrl = ->
  date_time = ''
  stringDateTime = ''
  camera_name = $('#recorded-call-camera').val()
  if $('#api-call-datetime').val()
    stringDateTime = $('#api-call-datetime').val() + 'T' + $('#api-call-hour').val() + ':' + $('#api-call-minutes').val() + ':' + $('#api-call-seconds').val() + '.000Z'
    date_time = stringDateTime
  loadRecordedSnapshot(camera_name, date_time)

loadRecordedSnapshot = (recording_camera_name, recording_time) ->
  data =
    api_id: Evercam.User.api_id
    api_key: Evercam.User.api_key

  onError = (jqXHR, status, error) ->
    $('.recorded-url').hide()
    $('#no-recorded-snapshot').show()
    $('#recorded-dashboard-url').val("https://dash.evercam.io/v1/cameras/#{recording_camera_name}/recordings/snapshots/#{recording_time}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}")
    $("#recorded-dashboard-link").attr 'href', "https://dash.evercam.io/v1/cameras/#{recording_camera_name}/recordings/snapshots/#{recording_time}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"

  onSuccess = (response, status, jqXHR) ->
    if response.snapshots.length > 0
      $('.recorded-url').show()
      $('#no-recorded-snapshot').hide()
      $('#recorded-snapshot-url').val("#{Evercam_API_URL}cameras/#{recording_camera_name}/recordings/snapshots/#{recording_time}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}&view=true")
      $("#recorded-api-link").attr("href", "#{Evercam_API_URL}cameras/#{recording_camera_name}/recordings/snapshots/#{recording_time}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}&view=true")
      $('#recorded-dashboard-url').val("https://dash.evercam.io/v1/cameras/#{recording_camera_name}/recordings/snapshots/#{recording_time}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}")
      $("#recorded-dashboard-link").attr 'href', "https://dash.evercam.io/v1/cameras/#{recording_camera_name}/recordings/snapshots/#{recording_time}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'GET'
    url: "#{Evercam.API_URL}cameras/#{recording_camera_name}/recordings/snapshots/#{recording_time}"
  $.ajax(settings)

initDatepicker = ->
  $('#api-call-datetime').datetimepicker
    timepicker: false
    closeOnDateSelect: 0
    onSelectDate: (dp, $input) ->
      if $input.val() != lastSelectedDate
        lastSelectedDate = $input.val()
        updateRecordedSnapshotUrl()

    format: 'Y-m-d'

  i = 0
  while i < 60
    option = '<option value="' + FormatNumTo2(i) + '">' + FormatNumTo2(i) + '</option>'
    if i < 24
      $('#api-call-hour').append option
    $('#api-call-minutes').append option
    $('#api-call-seconds').append option
    i++

FormatNumTo2 = (n) ->
  if n < 10
    '0' + n
  else
    n

setDate = ->
  DateTime = new Date(moment.utc().format('MM/DD/YYYY, HH:mm:ss'))
  Datefrom = format_time.formatDate(DateTime, 'Y-m-d')
  $('#api-call-datetime').val Datefrom
  get_hour = DateTime.getHours()
  hour_value = FormatNumTo2(get_hour)
  $('#api-call-hour').val hour_value

initClipboard = ->
  clipboard = new Clipboard('.btn')
  clipboard.on 'success', (e) ->
    $('.bb-alert').width '100px'
    Notification.show 'Copied!'

window.initializeUserAccount = ->
  format_time = new DateFormatter()
  $.validate()
  Metronic.init()
  Layout.init()
  QuickSidebar.init()
  Notification.init(".bb-alert")
  handleEditable()
  showHideMessage()
  handlePasswordChange()
  saveUserSettings()
  onDeleteClick()
  NProgress.done()
  initLiveCameraSelect()
  initRecordedCameraSelect()
  initDatepicker()
  setDate()
  initClipboard()
  updateLiveSnapshotUrl()
  updateRecordedSnapshotUrl()
  $('#api-call-camera').change(updateLiveSnapshotUrl)
  $('#recorded-call-camera').change(updateRecordedSnapshotUrl)
  $('#api-call-hour').change(updateRecordedSnapshotUrl)
  $('#api-call-minutes').change(updateRecordedSnapshotUrl)
  $('#api-call-seconds').change(updateRecordedSnapshotUrl)

initialize = ->
  markers = []
  map = new (google.maps.Map)(document.getElementById('map-canvas'), mapTypeId: google.maps.MapTypeId.ROADMAP)
  defaultBounds = new (google.maps.LatLngBounds)(new (google.maps.LatLng)(-33.8902, 151.1759), new (google.maps.LatLng)(-33.8474, 151.2631))
  map.fitBounds defaultBounds
  # Create the search box and link it to the UI element.
  input = document.getElementById('pac-input')
  map.controls[google.maps.ControlPosition.TOP_LEFT].push input
  searchBox = new (google.maps.places.SearchBox)(input)
  # Listen for the event fired when the user selects an item from the
  # pick list. Retrieve the matching places for that item.
  google.maps.event.addListener searchBox, 'places_changed', ->
    `var marker`
    `var i`
    places = searchBox.getPlaces()
    if places.length == 0
      return
    i = 0
    marker = undefined
    while marker = markers[i]
      marker.setMap null
      i++
    # For each place, get the icon, place name, and location.
    markers = []
    bounds = new (google.maps.LatLngBounds)
    i = 0
    place = undefined
    while place = places[i]
      image =
        url: place.icon
        size: new (google.maps.Size)(71, 71)
        origin: new (google.maps.Point)(0, 0)
        anchor: new (google.maps.Point)(17, 34)
        scaledSize: new (google.maps.Size)(25, 25)
      # Create a marker for each place.
      marker = new (google.maps.Marker)(
        map: map
        icon: image
        title: place.name
        position: place.geometry.location)
      markers.push marker
      bounds.extend place.geometry.location
      i++
    map.fitBounds bounds
    return
  # Bias the SearchBox results towards places that are within the bounds of the
  # current map's viewport.
  google.maps.event.addListener map, 'bounds_changed', ->
    bounds = map.getBounds()
    searchBox.setBounds bounds
    return
  return
