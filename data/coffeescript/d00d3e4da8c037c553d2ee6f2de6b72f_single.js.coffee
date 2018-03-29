#= require evercam.js.coffee
#= require cameras/single/info.js.coffee
#= require cameras/single/live_view.js.coffee
#= require cameras/single/sharing.js.coffee
#= require cameras/single/snapshots_navigator.js.coffee
#= require cameras/single/timelapse_recording.js.coffee
#= require cameras/single/logs.js.coffee
#= require cameras/single/local_storage.js.coffee
#= require cameras/single/settings.js.coffee
#= require cameras/single/testsnapshot.js.coffee
#= require cameras/single/archives.js.coffee
#= require cameras/single/single_snapmail.js.coffee
#= require cameras/single/timelapse.js.coffee
#= require cameras/single/compare.js.coffee
#= require saveimage.js
#= require jquery.thumbhover.js
#= require jquery.images-compare.js
#= require cameras/single/nvr_recording.js.coffee
#= require downloadjs

window.sendAJAXRequest = (settings) ->
  token = $('meta[name="csrf-token"]')
  if token.size() > 0
    headers =
      "X-CSRF-Token": token.attr("content")
    settings.headers = headers
  xhrRequestChangeMonth = $.ajax(settings)

initializeiCheck = ->
  $("input[type=radio]").iCheck
    radioClass: "iradio_flat-blue"

initializeDropdowns = ->
  $('[data-toggle="tooltip"]').tooltip()
  $(".dropdown-toggle").dropdown()
  $(".dropdown-toggle i").addClass 'fas fa-bars icon-bar-alignment'

switchToTab = ->
  $(".nav-tab-#{Evercam.request.tabpath}").tab('show')

handleTabClick = ->
  $('.nav-tabs a').on 'click', ->
    clicked_path = $(this).attr('data-target').replace('#', '')
    if window.history and window.history.pushState
      window.history.pushState( {} , "#{clicked_path}", "#{window.Evercam.request.rootpath}/#{clicked_path}" );
    if clicked_path is "live"
      if $(window).width() >= 992
        handleLiveViewMenuWidth()
    else
      $('.center-tabs').width '100%'
      $('.center-tabs').removeClass 'center-tabs-live'
      $('#read-only-sharing-tab').removeClass 'read-only-sharing-tab'
      $('.delete-share-icon').removeClass 'colour-white'
      $('#ul-nav-tab').removeClass 'nav-menu-text'
  $(".nav-tabs").tabdrop "layout"

handleBackForwardButton = ->
  window.addEventListener 'popstate', (e) ->
    tab = document.location.pathname
      .replace(window.Evercam.request.rootpath, '')
      .split('/')[1]
    $(".nav-tab-#{tab}").tab('show')

handleCameraModalSubmit = ->
  $('#settings-modal').on 'click', '#add-button', ->
    $('#settings-modal').modal('hide')

showNonAuthorizedUserAlert = ->
  $('#add-to-public-cameras').on 'click', ->
    $('.public-camera-alert').fadeIn()
    setTimeout (->
      $('.public-camera-alert').fadeOut()
    ), 5000

handleAddToMyCameras = ->
  $('#add-to-cameras').on 'click', ->
    data =
      camera_id: Evercam.Camera.id
      email: Evercam.User.username
      permissions: "minimal"

    onError = (jqXHR, status, error) ->
      Notification.show("Failed to add camera.")

    onSuccess = (data, status, jqXHR) ->
      if data.success
        Notification.show("Camera successfully added.")
        window.location = "/v1/cameras/#{Evercam.Camera.id}"
      else
        message = "Adding a camera share failed."
        switch data.code
          when "camera_not_found_error"
            message = "Unable to locate details for the camera in the system. Please refresh your view and try again."
          when "duplicate_share_error"
            message = "The camera has already been shared with the specified user."
          when "duplicate_share_request_error"
            message = "A share request for that email address already exists for this camera."
          when "share_grantor_not_found_error"
            message = "Unable to locate details for the user granting the share in the system."
          when "invalid_parameters"
            message = "Invalid rights specified for share creation request."
          else
            message = data.message
        Notification.show(message)

    settings =
      cache: false
      data: data
      dataType: 'json'
      error: onError
      success: onSuccess
      type: 'POST'
      url: '/share'
    sendAJAXRequest(settings)

readOnlyCameraDeleteOption = ->
  $("#delete-read-only-camera").hide()
  $("#ro-sharing-tab").hide()
  hrefs = $('.cameralist-height a')
  hrefs.each ->
    if $(this).data('camera-id') == Evercam.Camera.id
      $("#delete-read-only-camera").show()
      $("#ro-sharing-tab").show()
      return

disabledCreateClipButton = ->
  if $('#archive-create-button').val()
    $('#recording-archive-button').attr 'disabled', 'disabled'
    $('#recording-archive-button').removeAttr 'data-target'
    $('#archive-button').attr 'disabled', 'disabled'
    $('#archive-button').removeAttr 'data-target'
  else
    $('#recording-archive-button').removeAttr 'disabled'
    $('#recording-archive-button').attr 'data-target', '#archive-modal'
    $('#archive-button').removeAttr 'disabled'
    $('#archive-button').attr 'data-target', '#archive-modal'

showDurationError = ->
  $('#to-date').keyup ->
    date_value = @value
    if date_value > 60
      $('#to-date').val '60'
      $('.duration-error').addClass 'duration-text-error'
    else if date_value < 0
      $('#to-date').val '1'
      $('.duration-error').addClass 'duration-text-error'
    else
      $('.duration-error').removeClass 'duration-text-error'

liveViewMenuDisplay = ->
  if $("#live-view-tab").hasClass 'active'
    handleLiveViewMenuWidth()
  else
    $('.center-tabs').width '100%'
    $('.center-tabs').removeClass 'center-tabs-live'
    $('#read-only-sharing-tab').removeClass 'read-only-sharing-tab'
    $('.delete-share-icon').removeClass 'colour-white'
    $('#ul-nav-tab').removeClass 'nav-menu-text'

handleLiveViewMenuWidth = ->
  if $(window).width() >= 992
    content_width = Metronic.getViewPort().width
    side_bar_width = $(".page-sidebar").width()
    live_view_menu_width = content_width - side_bar_width
    $('.center-tabs').addClass 'center-tabs-live'
    $('#ul-nav-tab').addClass 'nav-menu-text'
    $('#read-only-sharing-tab').addClass 'read-only-sharing-tab'
    $('.delete-share-icon').addClass 'colour-white'
    $('.center-tabs').width live_view_menu_width
  else
    $('.center-tabs').width '100%'
    $('.center-tabs').removeClass 'center-tabs-live'
    $('#read-only-sharing-tab').removeClass 'read-only-sharing-tab'
    $('.delete-share-icon').removeClass 'colour-white'
    $('#ul-nav-tab').removeClass 'nav-menu-text'

handleResize = ->
  $(window).resize ->
    liveViewMenuDisplay()

initTimepicker = ->
  $('.timepicker-default').timepicker
    minuteStep: 1
    showSeconds: false
    showMeridian: false
    template: false
    $('.timepicker-default').val(getPastOneHour())

  $('#from-date').on 'click', ->
    $('.timepicker-default').timepicker 'hideWidget'

getPastOneHour = (d) ->
  d = moment().tz(Evercam.Camera.timezone)
  d.hours(d.hours() - 1)
  return "#{FormatNumTo2(d.hours())}
    :#{FormatNumTo2(d.minutes())}
    :#{FormatNumTo2(d.seconds())}"

FormatNumTo2 = (n) ->
  if n < 10
    "0#{n}"
  else
    n

initializeTabs = ->
  window.initializeInfoTab()
  window.initializeLiveTab()
  window.initializeRecordingsTab()
  window.initializeLogsTab()
  window.initializeSharingTab() if $("#shares").length > 0
  window.initializeLocalStorageTab()
  window.initializeSettingsTab()
  window.initializeArchivesTab()
  window.initializeCompareTab()
  window.initializeTimelapseRecordingsTab() if $("#timelapse-recording-tab").length > 0
  window.initializeLocalRecordingsTab() if $("#local-recordings").length > 0

window.initializeCameraSingle = ->
  initializeTabs()
  handleTabClick()
  switchToTab()
  handleBackForwardButton()
  handleAddToMyCameras()
  initializeiCheck()
  initializeDropdowns()
  Metronic.init()
  Layout.init()
  QuickSidebar.init()
  SaveImage.init()
  readOnlyCameraDeleteOption()
  showDurationError()
  liveViewMenuDisplay()
  handleResize()
  initTimepicker()
  showNonAuthorizedUserAlert()
