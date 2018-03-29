#= require jquery
#= require jquery_ujs
#= require bootstrap
#= require ladda/spin.min.js
#= require ladda/ladda.min.js
Evercam_MEDIA_URL = 'https://media.evercam.io/v1/'
Evercam_API_URL = 'https://api.evercam.io/v1/'
Dasboard_URL = 'https://dash.evercam.io'
API_ID = ''
API_Key = ''
iframeWindow = undefined
gotSnapshot = false
loader = null

sortByKey = (array, key) ->
  array.sort (a, b) ->
    x = a[key]
    y = b[key]
    (if (x < y) then -1 else ((if (x > y) then 1 else 0)))

loadVendors = ->
  data = {}

  onError = (jqXHR, status, error) ->
    false

  onSuccess = (result, status, jqXHR) ->
    vendors = sortByKey(result.vendors, "name")
    for vendor in vendors
      $("#camera-vendor").append("<option value='#{vendor.id}'>#{vendor.name}</option>")

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json; charset=utf-8"
    type: 'GET'
    url: "#{Evercam_API_URL}vendors"

  jQuery.ajax(settings)
  true

loadVendorModels = (vendor_id) ->
  $("#camera-model option").remove()
  $("#camera-model").prop("disabled", true)
  if vendor_id is ""
    $("#camera-model").append('<option value="">Unknown / not specified</option>');
    $("#camera-snapshot-url").val('')
    $("#vemdor-image").attr("src", "/assets/plain.png")
    $("#model-image").attr("src", "/assets/plain.png")
    return
  $("#camera-model").append('<option value="">Loading...</option>');

  data = {}
  data.vendor_id = vendor_id
  data.limit = 400

  onError = (jqXHR, status, error) ->
    false

  onSuccess = (result, status, jqXHR) ->
    $("#camera-model option").remove()
    if result.models.length == 0
      $("#camera-model").append('<option value="">No Model Found</option>');
      return

    models = sortByKey(result.models, "name")
    for model in models
      jpg_url = if model.defaults.snapshots and model.defaults.snapshots.jpg.toLowerCase() isnt "unknown" then model.defaults.snapshots.jpg else ''
      default_username = if model.defaults.auth != null and model.defaults.auth != undefined then model.defaults.auth.basic.username else ''
      default_password = if model.defaults.auth != null and model.defaults.auth != undefined then model.defaults.auth.basic.password else ''
      if model.name.toLowerCase().indexOf('default') isnt -1
        $("#camera-model").prepend("<option jpg-val='#{jpg_url}' username-val='#{default_username}' password-val='#{default_password}' selected='selected' value='#{model.id}'>#{model.name}</option>")
        hasModelImage($("#camera-vendor").val(), model.id)
      else
        $("#camera-model").append("<option jpg-val='#{jpg_url}' username-val='#{default_username}' password-val='#{default_password}' value='#{model.id}'>#{model.name}</option>")
    $("#camera-model").removeAttr("disabled")
    if $("#camera-model").find(":selected").attr("jpg-val") isnt 'Unknown'
      selected_option = $("#camera-model").find(":selected")
      cleanAndSetJpegUrl selected_option.attr("jpg-val")
      $("#default-username").text(selected_option.attr("username-val"))
      $("#default-password").text(selected_option.attr("password-val"))
      $("#camera-snapshot-url").removeClass("invalid").addClass("valid")

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json; charset=utf-8"
    type: 'GET'
    url: "#{Evercam_API_URL}models"

  jQuery.ajax(settings)
  true

hasModelImage = (vendor_id, model_id) ->
  img = new Image()
  image_url = "https://evercam-public-assets.s3.amazonaws.com/#{vendor_id}/#{model_id}/thumbnail.jpg"
  img.onload = ->
    $("#model-image").attr("src", image_url)
  img.onerror = ->
    $("#model-image").attr("src", "/assets/plain.png")
  img.src = image_url

handleVendorModelEvents = ->
  $("#camera-vendor").on "change", ->
    img = new Image()
    image_url = "https://evercam-public-assets.s3.amazonaws.com/#{$(this).val()}/logo.jpg"
    img.onload = ->
      $("#vemdor-image").attr("src", image_url)
    img.onerror = ->
      $("#vemdor-image").attr("src", "/assets/plain.png")
    img.src = image_url
    loadVendorModels($(this).val())

  $("#camera-model").on "change", ->
    selected_option = $(this).find(":selected")
    hasModelImage($("#camera-vendor").val(), $(this).val())
    snapshot_url = selected_option.attr("jpg-val")
    $("#default-username").text(selected_option.attr("username-val"))
    $("#default-password").text(selected_option.attr("password-val"))
    if snapshot_url isnt 'Unknown'
      cleanAndSetJpegUrl snapshot_url

cleanAndSetJpegUrl = (jpeg_url) ->
  if jpeg_url.indexOf('/') == 0
    jpeg_url = jpeg_url.substr(1)
  $("#camera-snapshot-url").val jpeg_url

useAuthentication = ->
  $("#required-authentication").on 'click', ->
    if $(this).is(":checked")
      $("#authentication").removeClass("hide")
    else
      $("#authentication").addClass("hide")

handleInputEvents = ->
  $("#camera-url").on 'keyup', (e) ->
    if validate_hostname($(this).val())
      $(this).removeClass("invalid").addClass("valid")
    else
      $(this).removeClass("valid").addClass("invalid")
    validAllInformation()
  $("#camera-url").on 'focus', (e) ->
    $(".info-box .info-header").text("EXTERNAL IP / URL")
    $(".info-box .info-text").text("Put the public URL or IP address of your camera. You will need to have setup port forwarding for your camera.")
  $(".external-url").on 'click', ->
    $(".info-box .info-header").text("EXTERNAL IP / URL")
    $(".info-box .info-text").text("Put the public URL or IP address of your camera.")

  $("#camera-port").on 'keyup', (e) ->
    if validateInt($(this).val())
      $(this).removeClass("invalid").addClass("valid")
    else
      $(this).removeClass("valid").addClass("invalid")
    validAllInformation()
  $("#camera-port").on 'focus', (e) ->
    $(".info-box .info-header").text("EXTERNAL PORT")
    $(".info-box .info-text").text("The port should be a 2-5 digit number. The default external port is 80.")
  $(".port").on 'click', ->
    $(".info-box .info-header").text("EXTERNAL PORT")
    $(".info-box .info-text").text("The port should be a 2-5 digit number. The default external port is 80.")

  $("#camera-snapshot-url").on 'keyup', (e) ->
    if $(this).val() is ''
      $(this).removeClass("valid").addClass("invalid")
    else
      $(this).removeClass("invalid").addClass("valid")
    validAllInformation()

  $("#camera-snapshot-url").on 'focus', (e) ->
    $(".info-box .info-header").text("SNAPSHOT URL")
    $(".info-box .info-text").text("If you know your Camera Vendor & Model we can work this out for you. You can also enter it manually for your camera.")
  $(".snapshot-url").on 'click', ->
    $(".info-box .info-header").text("SNAPSHOT URL")
    $(".info-box .info-text").text("If you know your Camera Vendor & Model we can work this out for you. You can also enter it manually for your camera.")

  $("#camera-name").on 'keyup', (e) ->
    $(this).removeClass("invalid").addClass("valid")
  $("#camera-id").on 'keyup', (e) ->
    $(this).removeClass("invalid").addClass("valid")

  $("#user-email").on 'keyup', (e) ->
    if validateEmail($(this).val())
      $(this).removeClass("invalid").addClass("valid")
    else
      $(this).removeClass("valid").addClass("invalid")
  $("#user-password").on 'keyup', (e) ->
    $(this).removeClass("invalid").addClass("valid")
  $("#username").on 'keyup', (e) ->
    $(this).removeClass("invalid").addClass("valid")
  $(".default-username").on 'click', ->
    $("#camera-username").val($("#default-username").text())
  $(".default-password").on 'click', ->
    $("#camera-password").val($("#default-password").text())

validate_hostname = (str) ->
  ValidIpAddressRegex = /^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$/
  ValidHostnameRegex = /^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])$/
  ValidIpAddressRegex.test(str) or ValidHostnameRegex.test(str)

validateInt = (value) ->
  reg = /^(0|[0-9][1-9]|[1-9][0-9]*)$/
  reg.test value

validateEmail = (email) ->
  reg = /^([A-Za-z0-9_\-\.])+\@([A-Za-z0-9_\-\.])+\.([A-Za-z]{2,4})$/
  #remove all white space from value before validating
  emailtrimed = email.replace(RegExp(' ', 'gi'), '')
  reg.test emailtrimed

validAllInformation = ->
  if $("#camera-port") is ''
    if $("#camera-url").hasClass('valid') && $("#camera-snapshot-url").hasClass('valid')
      $(".test-image").removeClass('hide')
      $(".help-texts").addClass('hide')
    else
      $(".test-image").addClass('hide')
      $(".help-texts").removeClass('hide')
  else
    if $("#camera-url").hasClass('valid') && $("#camera-port").hasClass('valid') && $("#camera-snapshot-url").hasClass('valid')
      $(".test-image").removeClass('hide')
      $(".help-texts").addClass('hide')
    else
      $(".test-image").addClass('hide')
      $(".help-texts").removeClass('hide')

testSnapshot = ->
  $("#test-snapshot").on 'click', ->
    initLadda(this)
    port = $("#camera-port").val() unless $("#camera-port").val() is ''
    data = {}
    data.external_url = "http://#{$('#camera-url').val()}:#{port}"
    data.jpg_url = $('#camera-snapshot-url').val()
    data.cam_username = $("#camera-username").val() unless $("#camera-username").val() is ''
    data.cam_password = $("#camera-password").val() unless $("#camera-password").val() is ''
    data.vendor_id = $("#camera-vendor").val() unless $("#camera-vendor").val() is ''

    onError = (jqXHR, status, error) ->
      $(".snapshot-msg").html(jqXHR.responseJSON.message)
      $(".snapshot-msg").removeClass("msg-success").addClass("msg-error")
      $(".snapshot-msg").show()
      if loader isnt null
        loader.stop()

    onSuccess = (result, status, jqXHR) ->
      if result.status is 'ok'
        $("#testimg").attr('src', result.data)
        $(".snapshot-msg").html("We got a snapshot!")
        $(".snapshot-msg").removeClass("msg-error").addClass("msg-success")
        $(".snapshot-msg").show()
        $("#test-snapshot").hide()
        $("#continue-step2").show()
        gotSnapshot = true
        if loader isnt null
          loader.stop()

    settings =
      cache: false
      data: data
      dataType: 'json'
      error: onError
      success: onSuccess
      contentType: "application/x-www-form-urlencoded"
      type: 'POST'
      url: "#{Evercam_MEDIA_URL}cameras/test"

    jQuery.ajax(settings)

handleContinueBtn = ->
  $("#continue-step2").on 'click', ->
    switchTab("camera-details", "camera-info")

  $("#continue-step3").on 'click', ->
    if $("#camera-name").val() is ''
      $("#camera-name").removeClass("valid").addClass("invalid")
      return
    if $("#camera-id").val() is ''
      $("#camera-id").removeClass("valid").addClass("invalid")
      return
    $("#camera-name").removeClass("invalid").addClass("valid")
    switchTab("camera-info", "user-create")

autoCreateCameraId = ->
  $("#camera-name").on 'keyup', ->
    $("#camera-id").val $(this).val().replace(RegExp(" ", "g"), "-").toLowerCase()

hasCameraInfo = ->
  if $("#camera-url").val() is '' && $("#camera-snapshot-url").val() is ''
    $("#camera-url").removeClass("valid").addClass("invalid")
    $("#camera-snapshot-url").removeClass("valid").addClass("invalid")
    switchTab("user-create", "camera-details")
    return false
  if $("#camera-name").val() is '' && $("#camera-id").val() is ''
    $("#camera-name").removeClass("valid").addClass("invalid")
    $("#camera-id").removeClass("valid").addClass("invalid")
    switchTab("user-create", "camera-info")
    return false
  true

autoLogInDashboard = () ->
  data = {
    'session[login]': $("#username").val()
    'session[password]': $("#user-password").val()
    'session[widget]': 'login-from-widget'
    'authenticity_token': $("#authenticity_token").val()
  }

  onError = (jqXHR, status, error) ->
    false
  onSuccess = (result, status, jqXHR) ->
    parent.location.href = "#{Dasboard_URL}"
    true

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/x-www-form-urlencoded"
    type: 'POST'
    url: "#{Dasboard_URL}/sessions"

  jQuery.ajax(settings)

initLadda = (control_id) ->
  loader = Ladda.create(control_id)
  loader.start()
  progress = 0
  interval = setInterval(->
    progress = Math.min(progress + 0.025, 1)
    loader.setProgress(progress)
    if (progress == 1)
      loader.stop()
      clearInterval(interval)
  , 200)

createUserAccount = ->
  $("#create-account").on 'click', ->
    if $("#username").val() is ''
      $("#username").removeClass("valid").addClass("invalid")
      return
    if $("#user-email").val() is '' || !validateEmail($("#user-email").val())
      $("#user-email").removeClass("valid").addClass("invalid")
      return
    if $("#user-password").val() is ''
      $("#user-password").removeClass("valid").addClass("invalid")
      return
    if !hasCameraInfo()
      return

    initLadda(this)
    if API_ID isnt '' && API_Key isnt ''
      createCamera(API_ID, API_Key)
      return

    data = {}
    data.firstname = $("#username").val()
    data.lastname = $("#username").val()
    data.username = $("#username").val()
    data.email = $("#user-email").val()
    data.password = $("#user-password").val()
    data.token = $("#app_token").val()

    onError = (jqXHR, status, error) ->
      $("#message-user-create").text(jqXHR.responseJSON.message)
      $("#message-user-create").removeClass("hide")
      if loader isnt null
        loader.stop()

    onSuccess = (result, status, jqXHR) ->
      getAPICredentials()

    settings =
      cache: false
      data: data
      dataType: 'json'
      error: onError
      success: onSuccess
      contentType: "application/x-www-form-urlencoded"
      type: 'POST'
      url: "#{Evercam_API_URL}users"

    jQuery.ajax(settings)

getAPICredentials = ->
  data = {}
  data.password = $("#user-password").val()

  onError = (jqXHR, status, error) ->
    if loader isnt null
      loader.stop()
    false

  onSuccess = (result, status, jqXHR) ->
    API_ID = result.api_id
    API_Key = result.api_key
    createCamera(result.api_id, result.api_key)

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json; charset=utf-8"
    type: 'GET'
    url: "#{Evercam_API_URL}users/#{$("#user-email").val()}/credentials"

  jQuery.ajax(settings)

createCamera = (api_id, api_key) ->
  data = {}
  data.name = $("#camera-name").val()
  data.vendor = $("#camera-vendor").val() unless $("#camera-vendor").val() is ''
  data.model = $('#camera-model').val() unless $("#camera-model").val() is ''
  data.is_public = false
  data.is_online = true
  data.cam_username = $("#camera-username").val() unless $("#camera-username").val() is ''
  data.cam_password = $("#camera-password").val() unless $("#camera-password").val() is ''
  data.external_host = $("#camera-url").val()
  data.external_http_port = $("#camera-port").val() unless $("#camera-port").val() is ''
  data.jpg_url = $("#camera-snapshot-url").val()

  onError = (jqXHR, status, error) ->
    $("#message-camera-info").text(jqXHR.responseJSON.message)
    $("#message-camera-info").removeClass("hide")
    $("#message-user-create").addClass("hide")
    switchTab("user-create", "camera-info")
    if loader isnt null
      loader.stop()

  onSuccess = (result, status, jqXHR) ->
    parent.location.href = "#{Dasboard_URL}/v1/cameras?api_id=#{api_id}&api_key=#{api_key}"

  onDuplicateError = (xhr) ->
    switchTab("user-create", "camera-info")
    $("#message-camera-info").text(xhr.responseText.message)
    $("#message-camera-info").removeClass("hide")
    $("#message-user-create").addClass("hide")
    if loader isnt null
      loader.stop()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    statusCode: {409: onDuplicateError, 400: onDuplicateError },
    contentType: "application/x-www-form-urlencoded"
    type: 'POST'
    url: "#{Evercam_API_URL}cameras?api_id=#{api_id}&api_key=#{api_key}"

  jQuery.ajax(settings)

clearForm = ->
  $("#camera-id").val('')
  $("#camera-id").removeClass('valid').removeClass("invalid")
  $("#camera-name").val('')
  $("#camera-name").removeClass('valid').removeClass("invalid")
  $("#user-email").val('')
  $("#user-email").removeClass('valid').removeClass("invalid")
  $("#username").val('')
  $("#username").removeClass('valid').removeClass("invalid")
  $("#user-password").val('')
  $("#user-password").removeClass('valid').removeClass("invalid")
  $("#camera-username").val('')
  $("#camera-password").val('')
  $("#camera-port").val('')
  $("#camera-port").removeClass('valid').removeClass("invalid")
  $("#camera-url").val('')
  $("#camera-url").removeClass('valid').removeClass("invalid")
  $("#camera-snapshot-url").val('')
  $("#camera-snapshot-url").removeClass('valid').removeClass("invalid")
  $("#camera-vendor").val('')
  $("#camera-model option").remove()
  $("#camera-model").append('<option value="">Unknown / Not specified</option>');
  switchTab("user-create", "camera-details")
  $("#required-authentication").removeAttr("checked")
  $("#authentication").addClass("hide")
  $("#message-camera-info").addClass("hide")
  $("#message-user-create").addClass("hide")
  $("#testimg").attr('src', '')
  $(".snapshot-msg").hide()
  $("#test-snapshot").show()
  $("#continue-step2").hide()
  API_ID = ''
  API_Key = ''

onClickTabs = ->
  $(".nav-steps li").on 'click', ->
    if !gotSnapshot
      return
    previousTab = $(".nav-steps li.active").attr("href")
    $(".nav-steps li").removeClass('active')
    currentTab = $(this).attr("href")
    $(this).addClass('active')
    $("#{previousTab}").fadeOut(300, ->
      $("#{currentTab}").fadeIn(300)
    )

switchTab = (hideTab, showTab) ->
  $(".nav-steps li").removeClass('active')
  $("##{hideTab}").fadeOut(300, ->
    $("##{showTab}").fadeIn(300)
  )
  $("#li-#{showTab}").addClass('active')

initAddCamera = ->
  url = window.location.origin
  embedCode = '&lt;div evercam&#61;"add-camera-public"&gt;&lt;&#47;div&gt;' + '&lt;script type&#61;"text/javascript" src&#61;"' + url + '&#47;widgets/add.camera.js"&gt;&lt;&#47;script&gt;'
  $('#code').html embedCode
  $('.placeholder').empty()
  iframe = jQuery('<iframe />').css(
    'overflow': 'hidden'
    'width': '100%'
    'height': '420px').attr(
    'src': '/widgets/cameras/public/add'
    'frameborder': '0'
    scrolling: 'no').appendTo('.placeholder')
  return

resizeIframe = (iframeControl) ->
  iframeWindow = iframeControl
  iframeControl.style.height = iframeControl.contentWindow.document.body.scrollHeight + 'px'
  return

handleWindowResize = ->
  $(window).resize ->
  if !iframeWindow
    return
  resizeIframe iframeWindow
  return

window.initializeAddCameraPublic = ->
  useAuthentication()
  loadVendors()
  handleVendorModelEvents()
  handleInputEvents()
  testSnapshot()
  handleContinueBtn()
  createUserAccount()
  onClickTabs()

window.initializeAddCamera = ->
  initAddCamera();
  $("#code").on "click", ->
    this.select();
  handleWindowResize()

$(window, document, undefined).ready ->
  wskCheckbox = do ->
    wskCheckboxes = []
    SPACE_KEY = 32

    addEventHandler = (elem, eventType, handler) ->
      if elem.addEventListener
        elem.addEventListener eventType, handler, false
      else if elem.attachEvent
        elem.attachEvent 'on' + eventType, handler
      return

    clickHandler = (e) ->
      e.stopPropagation()
      if @className.indexOf('checked') < 0
        @className += ' checked'
      else
        @className = 'chk-span'
      return

    keyHandler = (e) ->
      e.stopPropagation()
      if e.keyCode == SPACE_KEY
        clickHandler.call this, e
        # Also update the checkbox state.
        cbox = document.getElementById(@parentNode.getAttribute('for'))
        cbox.checked = !cbox.checked
      return

    clickHandlerLabel = (e) ->
      id = @getAttribute('for')
      i = wskCheckboxes.length
      while i--
        if wskCheckboxes[i].id == id
          if wskCheckboxes[i].checkbox.className.indexOf('checked') < 0
            wskCheckboxes[i].checkbox.className += ' checked'
          else
            wskCheckboxes[i].checkbox.className = 'chk-span'
          break
      return

    findCheckBoxes = ->
      labels = document.getElementsByTagName('label')
      i = labels.length
      while i--
        posCheckbox = document.getElementById(labels[i].getAttribute('for'))
        if posCheckbox != null and posCheckbox.type == 'checkbox'
          text = labels[i].innerText
          span = document.createElement('span')
          span.className = 'chk-span'
          span.tabIndex = i
          labels[i].insertBefore span, labels[i].firstChild
          addEventHandler span, 'click', clickHandler
          addEventHandler span, 'keyup', keyHandler
          addEventHandler labels[i], 'click', clickHandlerLabel
          wskCheckboxes.push
            'checkbox': span
            'id': labels[i].getAttribute('for')
      return

    { init: findCheckBoxes }
  wskCheckbox.init()
  return
