# TODO: refactor this file

validate_hostname = (str) ->
  ValidIpAddressRegex = /^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$/
  ValidHostnameRegex = /^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])$/
  ValidIpAddressRegex.test(str) || ValidHostnameRegex.test(str)

showFeedback = (message) ->
  Notification.show(message)
  true

$ ->
  $(document).on 'input', "#camera-url-web", () ->
    val = $(this).val()
    a = document.createElement('a')
    a.href = val
    $(this).val(a.hostname)
    $('#port-web').val(a.port)
    $('#snapshot-web').val(a.pathname)

  $(document).on 'click', "#test", (event) ->
    event.preventDefault()
    $('#test-error').text('')
    intRegex = /^\d+$/
    port = $('#port').val()
    ext_url = $('#camera-url').val()
    vandor_model = $("#camera-vendor").val()
    $snap = $('#snapshot')
    jpg_url = $snap.val()
    # Auto-fix snapshot
    if (jpg_url.indexOf('/') == 0)
      $snap.val(jpg_url.substring(1))
      jpg_url = $snap.val()

    # Check for vendor model and snap-url
    if vandor_model is "Select Camera Vendor" || jpg_url is ""
      showFeedback("Please choose camera vendor or add your camera snapshot URL.")
      return

    # Validate port
    if(port != '' && (!intRegex.test(port) || port > 65535))
      showFeedback("Port value is incorrect")
      return
    else if (port != '')
      port = ':' + port

    # Validate host
    if (ext_url == '' || !validate_hostname(ext_url))
      showFeedback("External IP Address (or URL) is incorrect")
      return
    else if (ext_url.indexOf('192.168') == 0 || ext_url.indexOf('127.0.0') == 0 || ext_url.indexOf('10.') == 0)
      showFeedback("This is the Internal IP. Please use the External IP.")
      return

    params = ['external_url=http://' + ext_url + port,
              'jpg_url=/' + jpg_url,
              'cam_username=' + $('#camera-username').val(),
              'cam_password=' + $('#camera-password').val()]

    loader = Ladda.create(this)
    loader.start()
    progress = 0
    interval = setInterval(->
      progress = Math.min(progress + 0.025, 1)
      loader.setProgress(progress)
      if (progress == 1)
        loader.stop()
        clearInterval(interval)
    , 200)
    NProgress.start()

    #send request for test snapshot
    data = {}
    data.external_url = "http://#{ext_url}#{port}"
    data.jpg_url = jpg_url
    data.cam_username = $("#camera-username").val() unless $("#camera-username").val() is ''
    data.cam_password = $("#camera-password").val() unless $("#camera-password").val() is ''
    data.vendor_id = $("#camera-vendor").val()
    data.camera_exid = window.Evercam.Camera.id if window.Evercam.Camera.id

    onError = (jqXHR, status, error) ->
      $('#test-error').text(jqXHR.responseJSON.message)
      console.log('Error getting the snapshot from the camera. The camera response was:')
      console.log(jqXHR.responseJSON.response)
      loader.stop()
      $('#testimg').addClass 'hide'
      $('.img-holder-text').removeClass 'hide'
      NProgress.done()

    onSuccess = (result, status, jqXHR) ->
      $('#test-error').text('')
      if result.status is 'ok'
        if (result.data.indexOf('data:text/html') == 0)
          showFeedback("We got a response, but it's not an image")
          $('#testimg').addClass 'hide'
          $('.img-holder-text').removeClass 'hide'
        else
          showFeedback("We got a snapshot")
          $('#testimg').removeClass 'hide'
          $('.img-holder-text').addClass 'hide'
          $('#testimg').attr('src', result.data)
          if window.Evercam.Camera.is_online isnt undefined && !window.Evercam.Camera.is_online
            location.reload()
      loader.stop()
      NProgress.done()

    settings =
      cache: false
      data: data
      dataType: 'json'
      error: onError
      success: onSuccess
      contentType: "application/x-www-form-urlencoded"
      type: 'POST'
      url: "#{window.Evercam.MEDIA_API_URL}cameras/test"

    jQuery.ajax(settings)
