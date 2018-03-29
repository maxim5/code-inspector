images_array = {}
share_users_select = undefined
is_logged_intercom = false
retry_once = true
share_permissions = undefined

showError = (message) ->
  $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
  Notification.show(message)

showFeedback = (message) ->
  $(".bb-alert").removeClass("alert-danger").addClass("alert-info")
  Notification.show(message)

isUnauthorized = (response, message) ->
  if response.responseText.indexOf("/v1/users/signin") isnt -1
    showError("Your session has expired.")
    location = window.location
    location.assign(location.protocol + "//" + location.host)
  else
    showError(message)

sendAJAXRequest = (settings) ->
  token = $('meta[name="csrf-token"]')
  if token.size() > 0
    headers =
      "X-CSRF-Token": token.attr("content")
    settings.headers = headers
  jQuery.ajax(settings)
  true

loadShares = (is_for_user) ->
  data =
    api_id: Evercam.User.api_id
    api_key: Evercam.User.api_key
  data.user_id = Evercam.User.username if is_for_user

  onError = (jqXHR, status, error) ->
    isUnauthorized(jqXHR, "Failed to load camera share.")

  onSuccess = (response, success, jqXHR) ->
    if response.shares.length is 0 && retry_once
      retry_once = false
      loadShares(true)
    else
      $.each response.shares, (index, share) ->
        share['share_id'] = share['id']
        share.permissions = have_full_rights(share['rights'])
        share.type = "share"
        addSharingCameraRow(share)
      loadSharesRequests()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/shares"
  jQuery.ajax(settings)

loadSharesRequests = ->
  data =
    api_id: Evercam.User.api_id
    api_key: Evercam.User.api_key
    status: "PENDING"

  onError = (jqXHR, status, error) ->
    isUnauthorized(jqXHR, "Failed to load camera share.")

  onSuccess = (response, success, jqXHR) ->
    $.each response.share_requests, (index, share_request) ->
      share_request['share_id'] = share_request['id']
      share_request.permissions = have_full_rights(share_request['rights'])
      share_request.type = "share_request"
      addSharingCameraRow(share_request)
    setTimeout(getEmptyImages, 3000)

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'GET'
    url: "#{Evercam.MEDIA_API_URL}cameras/#{Evercam.Camera.id}/shares/requests"
  jQuery.ajax(settings)

have_full_rights = (rights) ->
  if rights.indexOf("edit") isnt -1
    "full"
  else if rights.indexOf("share") isnt -1
    "minimal+share"
  else
    "minimum"

addownerRow = (owner) ->
  $("#request-share-owner-email").html(owner['email'])
  $("#request-share-owner").html(owner['fullname'])
  row = "<tr>"
  row += "  <td class='owner-email' share-username='#{owner['username']}' share-email='#{owner['email']}'>"
  row += "    <div class='gravatar-placeholder'>"
  row += "      <img class='gravatar' email='#{owner['email']}'/>"
  row += "    </div>"
  row += "    <div class='username-id'>"
  row += "      #{owner['fullname']}"
  if Evercam.Camera.is_owner
    row += "      <small>(you)</small>"
  row += "      <br>"
  row += "      <small class='blue'>#{owner['email']}</small>"
  row += "    </div>"
  row += "  </td>"
  row += "  <td class='share-by'></td>"
  row += "  <td class='is-owner'>Is Owner</td>"
  row += "  <td></td>"
  row += "  <td class='col-lg-2'>"
  if Evercam.Camera.is_owner
    row += "    <a href='#' id='transfer' data-toggle='modal' data-target='#change_owner2'>"
    row += "      Transfer"
    row += "    </a>"
  row += "    </td>"
  row += "</tr>"
  $('#sharing_list_table tbody').append(row)

addSharingCameraRow = (details, shared_avatar) ->
  row = $('<tr id="row-share-'+details['share_id']+'">')
  if details.type == "share_request"
    row.attr("share-request-email", details['email'])
  else
    row.attr("share-username", details['user_id'])
    row.attr("share-email", details['email'])
    $("#new_owner_email").append(
      "<option value='#{details['user_id']}' share_id='#{details['share_id']}'>#{details['fullname']}</option>"
    )
  cell = $('<td>', {class: "col-lg-4"})
  avatar_placeholder = $('<div>', {class: "gravatar-placeholder"})
  avatar = $('<img>', {class: "gravatar"})
  avatar.attr("email", details['email'])
  avatar.attr("id", "gravatar-#{details['share_id']}")
  if shared_avatar isnt undefined
    avatar.attr("src", shared_avatar)
  else
    avatar.attr("src", getFavicon(details['email']))
  avatar_placeholder.append(avatar)
  cell.append(avatar_placeholder)
  username_id = $('<div>', {class: "username-id"})
  username_id.addClass("sharee_info")
  username_id.append(document.createTextNode(" " + (if details.type == "share_request" then details['email'] else details['fullname'])))
  if details.type isnt "share_request" && details["user_id"] is Evercam.User.username
    suffix = $('<small>', {class: "blue"})
    suffix.text(" (you)")
    username_id.append(suffix)
    $("#share_id").val(details['share_id'])

  if details.type == "share_request"
    suffix = $('<small>', {class: "blue"})
    suffix.text(" ...pending")
    username_id.append(suffix)
  else
    line_breake = $('<br>')
    username_id.append(line_breake)
    suffix = $('<small>', {class: "blue"})
    suffix.text(details['email'])
    username_id.append(suffix)
  cell.append(username_id)
  row.append(cell)

  cell = $('<td>', {class: "col-lg-3"})
  cell.addClass("share-by")
  username_id = $('<div>', {class: "username-id"})
  username_id.append(document.createTextNode(" " + details['sharer_name']))
  line_breake = $('<br>')
  username_id.append(line_breake)
  suffix = $('<small>', {class: "blue"})
  suffix.text(details['sharer_email'])
  username_id.append(suffix)
  cell.append(username_id)
  row.append(cell)
  cell = $('<td>', {class: "col-lg-2"})
  div = $('<div>', {class: "input-group"})
  select = $('<select>', {class: "form-control reveal", "show-class": "show-save"})
  select.change(onPermissionsFocus)
  option = $('<option>', {value: "minimal"})
  if details.permissions != "full"
    option.attr("selected", "selected")
  option.text("Read Only")
  select.append(option)

  option = $('<option>', {value: "minimal+share"})
  option.text("Read Only + Share")
  if details.permissions == "minimal+share"
    option.attr("selected", "selected")
  select.append(option)

  option = $('<option>', {value: "full"})
  if details.permissions == "full"
    option.attr("selected", "selected")

  if !Evercam.Camera.has_edit_right && Evercam.Camera.has_share_right
    option.attr("disabled", "disabled")

  option.text("Full Rights")
  select.append(option)

  div.append(select)
  cell.append(div)
  row.append(cell)

  cell = $('<td>', {class: "col-lg-1"})
  button = $('<button>', {class: "save show-save btn btn-primary"})
  button.text("Save")
  if details.type == "share"
    button.click(onSaveShareClicked)
  else
    button.click(onSaveShareRequestClicked)
  cell.append(button)
  row.append(cell)

  cell = $('<td>', {class: "col-lg-2"})
  div = $('<div>', {class: "form-group"})
  divPopup =$('<div>', {class: "popbox"})
  divPopup.attr("id", "popbox-#{details["share_id"]}")
  span = $('<span>', {class: "open"})
  span.attr("id", "open-#{details["share_id"]}")
  span.append($('<span>', {class: "remove"}))
  if details.type == "share"
    remove_icon = $('<i>', {class: "fa"})
    remove_icon.addClass("fa-trash-alt").addClass("icon-font")
    remove_icon.attr("title", "Remove")
    span.append(remove_icon)
    divPopup.append(span)
    divCollapsePopup = $('<div>', {class: "collapse-popup"})
    divBox2 = $('<div>', {class: "box-new"})
    divBox2.attr("id", "box-#{details["share_id"]}")
    divArrow = $('<div>', {class: "arrow"})
    divArrow.attr("id", "arrow-#{details["share_id"]}")
    divBox2.append(divArrow)
    divArrowBorder = $('<div>', {class: "arrow-border"})
    divArrowBorder.attr("id", "arrow-border-#{details["share_id"]}")
    divBox2.append(divArrowBorder)
    divMessage = $('<div>', {class: "margin-bottom-10"})
    divMessage.append($(document.createTextNode("Are you sure?")))
    divBox2.append(divMessage)
    divButtons = $('<div>', {class: "margin-bottom-10"})
    inputDelete = $('<input type="button" value="Yes, Remove">')
    inputDelete.addClass("btn btn-primary delete-btn delete-share")
    inputDelete.attr("camera_id", details["camera_id"])
    inputDelete.attr("share_id", details["share_id"])
    inputDelete.click(onDeleteShareClicked)
    divButtons.append(inputDelete)
    divButtons.append("<div id='close-popup-#{details["share_id"]}' class='btn delete-btn closepopup grey'><div class='text-center' fit>CANCEL</div></div>")
    divBox2.append(divButtons)
    divCollapsePopup.append(divBox2)
    divPopup.append(divCollapsePopup)
    div.append(divPopup)
  else
    revoke_icon = $('<i>', {class: "fas"})
    revoke_icon.addClass("fa-trash-alt").addClass("icon-font")
    revoke_icon.attr("title", "Revoke")
    span.append(revoke_icon)
    divPopup.append(span)

    spanResend = $('<span>', {class: "resend-share-request"})
    resend_icon = $('<i>', {class: "fa"})
    resend_icon.addClass("fa-paper-plane").addClass("icon-font")
    resend_icon.attr("title", "Resend Email")
    spanResend.append(resend_icon)
    spanResend.attr("camera_id", details["camera_id"])
    spanResend.attr("share_request_id", details["share_id"])
    spanResend.attr("email", details["email"])
    spanResend.click(resendCameraShareRequest)
    divPopup.append(spanResend)
    divCollapsePopup = $('<div>', {class: "collapse-popup"})
    divBox2 = $('<div>', {class: "box-new"})
    divBox2.attr("id", "box-#{details["share_id"]}")
    divArrow = $('<div>', {class: "arrow"})
    divArrow.attr("id", "arrow-#{details["share_id"]}")
    divBox2.append(divArrow)
    divArrowBorder = $('<div>', {class: "arrow-border"})
    divArrowBorder.attr("id", "arrow-border-#{details["share_id"]}")
    divBox2.append(divArrowBorder)
    divMessage = $('<div>', {class: "margin-bottom-10"})
    divMessage.append($(document.createTextNode("Are you sure?")))
    divBox2.append(divMessage)
    divButtons = $('<div>', {class: "margin-bottom-10"})
    inputDelete = $('<input type="button" value="Yes, Revoke">')
    inputDelete.addClass("btn btn-primary delete-btn delete-share-request-control")
    inputDelete.attr("camera_id", details["camera_id"])
    inputDelete.attr("share_request_id", details["share_id"])
    inputDelete.attr("email", details["email"])
    inputDelete.click(onDeleteShareRequestClicked)
    divButtons.append(inputDelete)
    divButtons.append("<div id='close-popup-#{details["share_id"]}' class='btn delete-btn closepopup grey'><div class='text-center' fit>CANCEL</div></div>")
    divBox2.append(divButtons)
    divCollapsePopup.append(divBox2)
    divPopup.append(divCollapsePopup)
    div.append(divPopup)

  cell.append(div)
  row.append(cell)

  row.hide()
  $('#sharing_list_table tbody').append(row)
  row.find('.save').hide()
  row.fadeIn()
  initPopup(details["share_id"])

initPopup = (id) ->
  $("#popbox-#{id}").popbox
    open: "#open-#{id}"
    box: "#box-#{id}"
    arrow: "#arrow-#{id}"
    arrow_border: "#arrow-border-#{id}"
    close: "#close-popup-#{id}"

getEmptyImages = ->
  img_tags = $("#shares .gravatar")
  img_tags.each ->
    img_id = $(this).attr("id")
    img = document.getElementById("#{img_id}")
    if img.naturalWidth < 3
      $(this).attr("src", "https://gravatar.com/avatar/446b9c716e6561d9318bc34f55870323")

resendCameraShareRequest = ->
  NProgress.start()
  control = $(this)
  data =
    camera_id: control.attr("camera_id")
    email: control.attr("email")
    share_request_id: control.attr("share_request_id")
    user_name: $("#user_name").val()

  onError = (jqXHR, status, error) ->
    isUnauthorized(jqXHR, "Failed to resend camera share request.")
    NProgress.done()

  onSuccess = (data, status, jqXHR) ->
    if data.success
      showFeedback("A notification email has been sent to the specified email address.")
    else
      showFeedback(data.message)
    NProgress.done()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'POST'
    url: '/share/request/resend'
  sendAJAXRequest(settings)

onSetCameraAccessClicked = (event) ->
  event.preventDefault()
  NProgress.start()
  selected = $('input[name=sharingOptionRadios]:checked').val()
  button = $('#set_permissions_submit')
  divText = $('#Sharespublic_discoverable')
  cameraId = Evercam.Camera.id

  data = {}
  switch selected
    when "public_discoverable"
      data.public = true
      data.discoverable = true
      $('.show-on-public').show()
      $('.show-on-private').hide()
      divText = $('#Sharespublic_discoverable')
    when "public_undiscoverable"
      data.public = true
      data.discoverable = false
      $('.show-on-public').show()
      $('.show-on-private').hide()
      divText = $('#Sharespublic_undiscoverable')
    else
      data.public = false
      data.discoverable = false
      $('.show-on-public').hide()
      $('.show-on-private').show()
      divText = $('#Sharesprivate')

  onError = (jqXHR, status, error) ->
    isUnauthorized(jqXHR, "Update of camera permissions failed. Please contact support.")
    button.removeAttr('disabled')

  onSuccess = (data, status, jqXHR) ->
    if data.success
      showFeedback("Camera permissions successfully updated.")
      $('.desc').hide()
      divText.show()
    else
      showError("Update of camera permissions failed. Please contact support.")
    button.removeAttr('disabled')
    NProgress.done()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'POST'
    url: '/share/camera/' + cameraId

  button.attr('disabled', 'disabled')
  sendAJAXRequest(settings)

getTransferFromUrl = ->
  is_transfer = window.Evercam.request.subpath.
  replace(RegExp("shares", "g"), "").
  replace(RegExp("/", "g"), "")
  if is_transfer is 'transfer'
    $('#change_owner2').modal('show')

onDeleteShareClicked = (event) ->
  NProgress.start()
  event.preventDefault()
  control = $(event.currentTarget)
  row = control.closest('tr')
  data =
    camera_id: control.attr("camera_id")
    email: row.attr('share-email')
  onError = (jqXHR, status, error) ->
    isUnauthorized(jqXHR, "Delete of camera shared failed. Please contact support.")
    NProgress.done()

  onSuccess = (data, status, jqXHR) ->
    if data.success
      onComplete = ->
        row.remove()
      row.fadeOut(onComplete)
      showFeedback("Camera share deleted successfully.")
      if $("#user_name").val() is row.attr('share-username')
        window.location = '/'
    else
      showError("Delete of camera shared failed. Please contact support.")
    NProgress.done()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'DELETE'
    url: '/share'
  sendAJAXRequest(settings)

onDeleteShareRequestClicked = (event) ->
  event.preventDefault()
  NProgress.start()
  control = $(event.currentTarget)
  row = control.closest('tr')
  data =
    camera_id: control.attr("camera_id")
    email: row.attr("share-request-email")
  onError = (jqXHR, status, error) ->
    isUnauthorized(jqXHR, "Delete of share request failed. Please contact support.")
    NProgress.done()

  onSuccess = (data, success, jqXHR) ->
    if data.success
      onComplete = ->
        row.remove()
      row.fadeOut(onComplete)
      showFeedback("Camera share request deleted successfully.")
    else
      showError("Delete of share request failed. Please contact support.")
    NProgress.done()
    true
  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'DELETE'
    url: '/share/request'
  sendAJAXRequest(settings)
  true

mapShareValues = (shares, type) ->
  $.map(shares, (share) ->
    {
      camera_id: share.camera_id,
      share_id: share.id,
      fullname: share.fullname,
      sharer_name: share.sharer_name,
      sharer_email: share.sharer_email,
      avatar: getFavicon(share["email"]),
      type: type,
      permissions: share_permissions,
      email: share.email,
      user_id: share.user_id,
    }
  )

onAddSharingUserClicked = (event) ->
  $('#submit_share_button').attr 'disabled', 'disabled'
  event.preventDefault()
  emailAddress = share_users_select.val()
  emailbodyMsg = $('#sharing-message').val()
  permissions = $('#sharingPermissionLevel').val()
  share_permissions = permissions

  onError = (jqXHR, status, error) ->
    isUnauthorized(jqXHR, "Failed to share camera.")
    NProgress.done()

  onSuccess = (data, status, jqXHR) ->
    logCameraViewed() unless is_logged_intercom
    if data
      total_shares = mapShareValues(data.shares, "share").concat(mapShareValues(data.share_requests, "share_request"))
      shared_avatar = $("#select2-sharing-user-email-container .gravatar1").attr("src")
      total_shares.forEach (share) ->
        addSharingCameraRow(share, shared_avatar)
      if total_shares.length == 1 && data.errors.length == 0
        showFeedback("Camera successfully shared with user")
      else if total_shares.length > 1 && data.errors.length == 0
        showFeedback("Camera successfully shared with all users")
      else if total_shares.length == 0 && data.errors.length == 1
        showError(data.errors[0].text)
      else if total_shares.length == 0 && data.errors.length > 1
        $ul = $('<ul style="float: left;">')
        data.errors.forEach (error) ->
          $ul.append "<li>#{error.text}</li>"
        showError($ul.html())
      else
        $ul = $('<ul style="float: left;">')
        data.errors.forEach (error) ->
          $ul.append "<li>#{error.text}</li>"
        html = "
          <p>Camera has been successfully shared but few errors came along, see below.</p>
          #{$ul.html()}
        "
        showFeedback(html)
      $('#sharing-message').val("")
      share_users_select.val("").trigger("change")

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
          message = data.message.substring(data.message.indexOf('["') + 2, data.message.indexOf('"]'))
      showError(message)
    NProgress.done()
    true
  createShare(Evercam.Camera.id, emailAddress, emailbodyMsg, permissions, onSuccess, onError, Evercam.User.api_key, Evercam.User.api_id)
  true

onSaveShareClicked = (event) ->
  event.preventDefault()
  NProgress.start()
  button = $(this)
  row = button.closest('tr')
  control = row.find('select')
  data =
    permissions: control.val()
    camera_id: Evercam.Camera.id
    email: row.attr('share-username')
  onError = (jqXHR, status, error) ->
    isUnauthorized(jqXHR, "Failed to update camera share.")
    NProgress.done()

  onSuccess = (data, success, jqXHR) ->
    if data.success
      showFeedback("Camera share updated.")
      button.fadeOut()
    else
      showError("Update of share failed. Please contact support.")
    NProgress.done()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'PATCH'
    url: '/share/' + Evercam.Camera.id
  sendAJAXRequest(settings)

onSaveShareRequestClicked = (event) ->
  event.preventDefault()
  NProgress.start()
  button = $(this)
  row = button.closest('tr')
  control = row.find('select')
  data =
    permissions: control.val()
    camera_id: Evercam.Camera.id
    email: row.attr('share-request-email')
  onError = (jqXHR, status, error) ->
    isUnauthorized(jqXHR, "Failed to update camera share request. Please contact support.")
    NProgress.done()

  onSuccess = (data, success, jqXHR) ->
    if data.success
      showFeedback("Share request successfully updated.")
      button.fadeOut()
    else
      showError("Update of share request failed. Please contact support.")
    NProgress.done()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'PATCH'
    url: '/share/request/'
  sendAJAXRequest(settings)

generateRightList = (permissions) ->
  rights = ["list", "snapshot"]
  baseRights = ["snapshot", "view", "edit", "delete", "list", "share"]
  if permissions == "full"
    baseRights.forEach (right) ->
      if right != "delete"
        rights.push(right)
        rights.push("grant~#{right}")
  else if permissions == "minimal+share"
    rights = ["list", "snapshot", "share"]
  rights.join(",")

createShare = (cameraID, email, bodyMessage, permissions, onSuccess, onError, apiKey, apiId) ->
  NProgress.start()
  data =
    email: email
    message: bodyMessage
    rights: generateRightList(permissions)
    api_key: apiKey
    api_id: apiId

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'POST'
    url: "#{Evercam.API_URL}cameras/#{cameraID}/shares"
  sendAJAXRequest(settings)

logCameraViewed = ->
  is_logged_intercom = true
  data = {}
  data.has_shared = true

  onError = (jqXHR, status, error) ->
    message = jqXHR.responseJSON.message

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

onPermissionsFocus = (event) ->
  $(this).parent().parent().parent().find("td:eq(3) button").fadeIn()

onSharingOptionsClicked = (event) ->
  test = $(this).val();
  $("div.desc").hide();
  $("#Shares" + test).show();

initializePopup = ->
  $(".popbox2").popbox
    open: ".open2"
    box: ".box2"
    arrow: ".arrow2"
    arrow_border: ".arrow-border2"
    close: ".closepopup2"

getFavicon = (email) ->
  if email isnt undefined
    signature = hex_md5(email)
    index = email.indexOf("@")
    domain = email.substr((index+1))
    favicon = "https://favicon.yandex.net/favicon/#{domain}"
    "https://gravatar.com/avatar/#{signature}?d=#{favicon}"

addGravatarFallbacks = ->
  img_tags = $("#shares .gravatar")
  img_tags.each ->
    img = $(this)
    email = img.attr "email"
    getGravatar(img, email)

getGravatar = (img, email) ->
  favicon = "https://favicon.yandex.net/favicon/"
  signature = hex_md5(email)
  index = email.indexOf("@")
  domain = email.substr((index+1))
  favicon_url = favicon + domain
  img_src = "https://gravatar.com/avatar/#{signature}?d=#{favicon_url}"
  if domain is "hotmail.com"
    img_src = "https://gravatar.com/avatar/#{signature}"
  data = {}

  onSuccess = (data, success, jqXHR) ->
    length = jqXHR.responseText.length
    if length < 100
      img_src = "https://gravatar.com/avatar/#{signature}"
    images_array["#{domain}"] = img_src
    if img isnt null
      img.attr "src", img_src

  onError = (jqXHR, status, error) ->
    images_array["#{domain}"] = img_src
    if img isnt null
      img.attr "src", img_src

  settings =
    cache: false
    data: data
    dataType: 'html'
    error: onError
    success: onSuccess
    type: 'GET'
    url: "#{favicon_url}"
  jQuery.ajax(settings)

validateEmail = (email) ->
  re = /^([A-Za-z0-9_\-\.])+\@([A-Za-z0-9_\-\.])+\.([A-Za-z]{2,4})$/
  re.test email

disableShareButton = ->
  if $('#sharing-user-email').val()
    $('#submit_share_button').removeAttr 'disabled'
  else
    $('#submit_share_button').attr 'disabled', 'disabled'

getSharedUsers = ->
  data =
    api_id: Evercam.User.api_id
    api_key: Evercam.User.api_key
    camera_id: Evercam.Camera.id

  onError = (jqXHR, status, error) ->
    true

  onSuccess = (users, status, jqXHR) ->
    $.each users, (i, user) ->
      $("#sharing-user-email").append(
        "<option value='#{user.email}'>#{user.name} (#{user.email})</option>"
      )
    share_users_select = $('#sharing-user-email').select2
      tags: true,
      placeholder: 'Enter Email Address',
      selectOnClose: true,
      closeOnSelect: false,
      tokenSeparators: [',', ';', ' '],
      templateSelection: format,
      templateResult: format
      createTag: (term, data) ->
        value = term.term
        if value
          share_users_select.select2("open")
        else
          share_users_select.select2("close")
        if validateEmail(value)
          return {
            id: value
            text: value
          }
        null
    share_users_select.val("").trigger("change")
    share_users_select.on 'select2:unselecting', (e) ->
      disableShareButton()
      $(this).data 'unselecting', true
    share_users_select.on 'select2:opening', (e) ->
      if $(this).data('unselecting')
        $(this).removeData 'unselecting'
        disableShareButton()
        e.preventDefault()
      setTimeout(getEmptyImagesForSelect2, 1500)
    share_users_select.on 'select2:close', (e) ->
      disableShareButton()
      setTimeout(onCloseSelect2SetGravatar, 1000)
    share_users_select.on 'select2:select', (e) ->
      disableShareButton()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'GET'
    url: "#{Evercam.API_URL}shares/users"
  jQuery.ajax(settings)

format = (state) ->
  if !state.id
    return state.text
  if state.id == '0'
    return state.text
  if state.element
    image_src = getFavicon(state.element.value) #images_array[domain]
    if image_src is undefined
      image_src = "https://gravatar.com/avatar/446b9c716e6561d9318bc34f55870323"
    return $("<span><img id='#{state.id}' style='width: 22px;height: auto;' src='#{image_src}' class='gravatar1'/>&nbsp;#{state.text}</span>")
  else
    state.text

getEmptyImagesForSelect2 = ->
  selected_email = $("#select2-sharing-user-email-container .gravatar1").attr("id")
  img_tags = $("#select2-sharing-user-email-results .gravatar1")
  img_tags.each ->
    img_id = $(this).attr("id")
    img = document.getElementById("#{img_id}")
    if img.naturalWidth < 3
      $(this).attr("src", "https://gravatar.com/avatar/446b9c716e6561d9318bc34f55870323")
    if selected_email isnt undefined && selected_email is img_id
      $(this).attr("src", $("#select2-sharing-user-email-container .gravatar1").attr("src"))

onCloseSelect2SetGravatar = ->
  disableShareButton()
  img_id = $("#select2-sharing-user-email-container .gravatar1").attr("id")
  img = document.getElementById("#{img_id}")
  if img.naturalWidth < 3
    $("#select2-sharing-user-email-container .gravatar1").attr("src", "https://gravatar.com/avatar/446b9c716e6561d9318bc34f55870323")

window.initializeSharingTab = ->
  $("#gravatar-0").attr("src", getFavicon($("#gravatar-0").attr("email")))
  loadShares(false)
  $('#set_permissions_submit').click(onSetCameraAccessClicked)
  $('.delete-share').click(onDeleteShareClicked)
  $('.delete-share-request-control').click(onDeleteShareRequestClicked)
  $('#submit_share_button').click(onAddSharingUserClicked)
  $('.update-share-button').click(onSaveShareClicked)
  $('.update-share-request-button').click(onSaveShareRequestClicked)
  $('.resend-share-request').click(resendCameraShareRequest)
  $('.save').hide()
  $('.reveal').focus(onPermissionsFocus);
  $("input[name$='sharingOptionRadios']").click(onSharingOptionsClicked);
  initializePopup()
  Notification.init(".bb-alert")
  getTransferFromUrl()
  getSharedUsers()

window.Evercam.Share.createShare = createShare
