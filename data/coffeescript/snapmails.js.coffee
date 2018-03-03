#= require responsiveslides.js
#= require detect_timezone.js

unselect_all = false
is_logged_intercom = false
camera_select = null
appApiUrl = "https://snapmail.evercam.io/api/snapmails"

initNotification = ->
  Notification.init(".bb-alert")

window.sendAJAXRequest = (settings) ->
  token = $('meta[name="csrf-token"]')
  if token.size() > 0
    headers =
      "X-CSRF-Token": token.attr("content")
    settings.headers = headers
  xhrRequestChangeMonth = $.ajax(settings)

rslidesInit = ->
  $('.bxslider').bxSlider({
    infiniteLoop: true,
    auto: true
  })

noSnapmailText = ->
  if $("#divSnapmails div").length is 0
    $("#divLoadingApps").show()
  else
    $("#divLoadingApps").hide()

initSnapmails = ->
  snapmails = Evercam.Snapmails
  $('#divSnapmails').html()
  if snapmails.length is 0
    $("#divLoadingApps").show()
  else
    $.each snapmails, (index, snapmail) ->
      $('#divSnapmails').append getSnapmailHtml(snapmail, index)
      selectDays(snapmail.id, snapmail.notify_days)
      initPopup(snapmail.id)
      $("#divLoadingApps").hide()
      rslidesInit()

loadSnapmails = ->
  onError = (jqXHR, status, error) ->
    $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
    Notification.show("Failed to retrive snapmails.")

  onSuccess = (snapMails, status, jqXHR) ->
    $('#divSnapmails').html()
    if snapMails.length is 0
      $("#divLoadingApps").show()
    else
      $.each snapMails.snapmails, (index, snapmail) ->
        $('#divSnapmails').append getSnapmailHtml(snapmail, index)
        selectDays(snapmail.id, snapmail.notify_days)
        initPopup(snapmail.id)
        $("#divLoadingApps").hide()
        rslidesInit()

  settings =
    data: {}
    dataType: 'json'
    error: onError
    success: onSuccess
    type: 'GET'
    url: "#{Evercam.API_URL}snapmails?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"

  $.ajax(settings)

selectDays = (snapmail_id, notify_days) ->
  days = notify_days.split(',')
  $.each days, (i, day) ->
    span_id = "divDays#{snapmail_id} span.#{day}"
    $("##{span_id}").removeClass("days-unchek").addClass("days-chek")

getSnapmailHtml = (snapMail, index) ->
  cameras = snapMail.camera_ids.split(',') if snapMail.camera_ids
  camera_names = snapMail.camera_names.replace(/,/g, ", ")
  recipients = snapMail.recipients.replace(/,/g, ", ")
  html = '<div id="dataslot' + snapMail.id + '" class="list-border margin-bottom10">'
  html += '    <div class="col-xs-12 col-sm-6 col-md-4 padding-left-0" style="min-height:0px;">'
  html += '    <div class="card" style="min-height:0px;">'
  html += '        <div class="snapstack-loading" id="snaps-' + snapMail.id + '" >'
  html += '           <ul class="bxslider" id="snapmail' + index + '">'
  $.each cameras, (i, camera) ->
    thumbnail_url = "#{Evercam.MEDIA_API_URL}cameras/#{camera}/thumbnail?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}";
    html += '           <li><img src="' + thumbnail_url + '" class="stackimage" style="visibility: visible" id="stackimage-' + snapMail.id + '-' + camera + '" alt="' + snapMail.camera_names.split(',')[i] + '" ><p>' + snapMail.camera_names.split(',')[i] + '</p></li>'
  html += '           </ul>'
  html += '        </div>'
  html += '        <input type="hidden" id="txtCamerasId' + snapMail.id + '" value="' + snapMail.camera_ids + '" /><input type="hidden" id="txtRecipient' + snapMail.id + '" value="' + (if snapMail.recipients is null then '' else snapMail.recipients) + '" /><input type="hidden" id="txtTime' + snapMail.id + '" value="' + snapMail.notify_time + '" />'
  html += '        <input type="hidden" id="txtDays' + snapMail.id + '" value="' + snapMail.notify_days + '" /><input type="hidden" id="txtUserId' + snapMail.id + '" value="' + snapMail.user_id + '" /><input type="hidden" id="txtTimezone' + snapMail.id + '" value="' + snapMail.timezone + '" />'
  html += '        <div class="hash-label snapmail-title"><a data-toggle="modal" data-target="#snapmail-form" class="tools-link edit-snapmail" data-val="' + snapMail.id + '" title="' + camera_names + '">' + camera_names + '</a><span class="line-end"></span></div>'
  html +='         <div class="camera-time"><span class="spn-label">@</span><div class="div-snapmail-values">' + snapMail.notify_time + ' (' + snapMail.timezone + ')</div><div class="clear-f"></div></div>'
  html +='         <div class="camera-email"><span class="spn-label">sent to</span><div class="div-snapmail-values snapmail-title" title="' + recipients + '">' + recipients + '<span class="line-end"></span></div><div class="clear-f"></div></div>'
  html +='         <div class="camera-days"><span class="spn-label margin-top-4">on</span><div id="divDays' + snapMail.id + '" class="div-snapmail-values"> <span class="days-unchek Monday">M</span><span class="days-unchek Tuesday">T</span><span class="days-unchek Wednesday">W</span><span class="days-unchek Thursday">T</span><span class="days-unchek Friday">F</span><span class="days-unchek Saturday">S</span><span class="days-unchek Sunday">S</span> </div><div class="clear-f"></div></div>'
  html +='         <div class="snapmail-edit"><i class="fa fa-edit main-color icon-font plus-btn tools-link edit-snapmail" title="Edit" data-toggle="modal" data-target="#snapmail-form" data-val="' + snapMail.id + '" data-action="e"></i></div>'
  html +='         <div class="snapmail-pause">'
  html +='           <i class="fa ' + (if snapMail.is_paused then "fa-play" else "fa-pause") + ' main-color icon-font plus-btn tools-link pause-snapmail" title="' + (if snapMail.is_paused then "Resume" else "Pause") + ' Snapmail" data-status="' + !snapMail.is_paused + '" data-val="' + snapMail.id + '"></i>'
  html +='         </div>'
  html +='         <div class="snapmail-clone" data-val="' + snapMail.id + '"><i class="fa fa-clone main-color tools-link icon-font plus-btn" title="Clone"></i></div>'
  html += '    </div>'

  html += '    <div class="" style="min-height:0px;">'
  html += '        <div class="text-right delete-snapmail">'
  html += '             <span id=pop-' + snapMail.id + ' class="popbox2"><div id="open-' + snapMail.id + '" href="javascript:;" class="tools-link open2" data-val="' + snapMail.id + '"><div class="icon-button red margin-24 margin-left-0"><i class="fas fa-trash-alt main-color icon-font plus-btn" title="Delete"></i><paper-ripple class="circle recenteringTouch" fit></paper-ripple></div></div>'
  html += '             <div class="collapse-popup">'
  html += '               <div class="box-snapmail" id="box-' + snapMail.id + '" style="width:288px;">'
  html += '                   <div class="arrow2" id="arrow-' + snapMail.id + '"></div>'
  html += '                   <div class="arrow-border2" id="arrow-border-' + snapMail.id + '"></div>'
  html += '                   <div class="margin-bottom-10">Are you sure?</div>'
  html += '                   <div class="margin-bottom-10"><input id="delete-snapmail" class="btn btn-primary delete-btn" type="button" value="Yes, Remove" data-val="' + snapMail.id + '"/><div href="#" id="close-popup-' + snapMail.id + '" class="btn closepopup2 grey" fit><div class="text-center">Cancel</div></div></div>'
  html += '               </div>'
  html += '             </div></span>'
  html += '       </div>'
  html += '       </div>'
  html += '    </div>'
  html += '</div>'
  html

makeMailTo = (emails) ->
  if emails is null
    return ''
  arEmails = emails.split(',')
  strEmails = ''
  i = 0
  while i < arEmails.length
    strEmails += arEmails[i] + ', '
    #'<a href="mailto:' + arEmails[i] + ';">' + arEmails[i] + '</a>, '
    i++
  strEmails.substring 0, strEmails.lastIndexOf(',')

initPopup = (key) ->
  $("#pop-#{key}").popbox
    open: "#open-#{key}"
    box: "#box-#{key}"
    arrow: "#arrow-#{key}"
    arrow_border: "#arrow-border-#{key}"
    close: "#close-popup-#{key}"

initCameraSelect = ->
  camera_select = $('#ddlCameras').select2
    placeholder: 'Select Camera',
    allowClear: true,
    templateSelection: format,
    templateResult: format
    escapeMarkup: (m) ->
      m

  $('#ddlCameras').on 'select2:select', (e) ->
    $('.select2-container').removeClass('error-border')

format = (state) ->
  is_offline = ""
  if !state.id
    return state.text
  if state.id == '0'
    return state.text
  if state.element.className is "onlinec"
    is_offline = '<i class="red main-sidebar fa fa-unlink"></i>'
  return $("<span><img style='height:30px;margin-bottom:1px;margin-top:1px;width:35px;' src='#{state.element.attributes[1].value}' class='img-flag' />&nbsp;#{state.text}</span>&nbsp;#{is_offline}")

initInputTags = ->
  $("#txtRecipient").tagsInput
    'height': 'auto'
    'width': 'auto'
    'defaultText': 'Add Recipients'
    'onAddTag': (email) ->
      $('#txtRecipient_tagsinput').removeClass('error-border')
      $('#add-snapmail').removeAttr("disabled")
      if !validateEmailByVal(email)
        $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
        Notification.show 'Invalid recipient email.'
        $('#txtRecipient_tagsinput').addClass('error-border')
      return
    'onRemoveTag': (email) ->
      if $('#txtRecipient_tagsinput').find('span').length is 0
        $('#add-snapmail').attr("disabled", "disabled")
        $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
        Notification.show 'Please enter recipients to continue.'
        $('#txtRecipient_tagsinput').addClass('error-border')
      return
    'delimiter': [',']
    'removeWithBackspace': true
    'minChars': 0
    'maxChars': 0
    'placeholderColor': '#666666'

validateEmailByVal = (email) ->
  reg = /^([A-Za-z0-9_\-\.\'])+\@([A-Za-z0-9_\-\.])+\.([A-Za-z]{2,4})$/
  addresstrimed = email.replace(RegExp(' ', 'gi'), '')
  if reg.test(addresstrimed) == false
    false
  else
    true

initTimepicker = ->
  $('.timepicker-default').timepicker
    minuteStep: 5
    showSeconds: false
    showMeridian: false

saveSnapmail = ->
  $('#add-snapmail').on 'click', ->
    save_button = $(this)
    showLoadingAnimation()
    $(".bb-alert").removeClass("alert-info").addClass("alert-danger")
    cameraIds = ''
    cameraNames = ''
    $('#ddlCameras :selected').each (i, selected) ->
      cameraIds += $(selected).val() + ','
      cameraNames += $(selected).text() + ', '
    if cameraIds is ''
      $('.select2-container').addClass('error-border')
      Notification.show 'Please select camera(s) to continue.'
      hideLoadingAnimation()
      return false
    else
      $('.select2-container').removeClass('error-border')
    cameraIds = cameraIds.substring(0, cameraIds.lastIndexOf(','))
    cameraNames = cameraNames.substring(0, cameraNames.lastIndexOf(','))
    if $('#txtRecipient').val() != ''
      emails = $('#txtRecipient').val().split(',')
      i = 0
      while i < emails.length
        if !validateEmailByVal(emails[i])
          Notification.show 'Invalid email \'' + emails[i] + '\'.'
          hideLoadingAnimation()
          return
        i++
    else
      if $('#txtkey').val() == ''
        $('#txtRecipient_tagsinput').addClass('error-border')
        Notification.show 'Please enter recipients to continue.'
        hideLoadingAnimation()
        return false
    if $('#txtTime').val() == ''
      Notification.show 'Please select time to continue.'
      hideLoadingAnimation()
      return false
    if $('#ddlTimezone').val() == '0'
      Notification.show 'Please select timezone to continue.'
      hideLoadingAnimation()
      return false
    if GetWeekdaysSelected() == ''
      Notification.show 'Please select day(s) to continue.'
      hideLoadingAnimation()
      return false

    o =
      subject: Evercam.User.username
      camera_exids: cameraIds
      recipients: $('#txtRecipient').val()
      notify_days: GetWeekdaysSelected()
      notify_time: $('#txtTime').val()
      timezone: $('#ddlTimezone').val()

    action = 'POST'
    queryString = ''
    if $('#txtkey').val() != ''
      action = 'PATCH'
      queryString = "/#{$('#txtkey').val()}"
    save_button.attr 'disabled', true

    onError = (jqXHR, status, error) ->
      if jqXHR.status is 500
        Notification.show "500 Internal Server Error"
      else
        response = JSON.parse(jqXHR.responseText)
        Notification.show "#{response.message}"
      save_button.removeAttr('disabled')
      hideLoadingAnimation()

    onSuccess = (result, status, jqXHR) ->
      snapMail = result.snapmails[0]
      $(".bb-alert").removeClass("alert-danger").addClass("alert-info")
      save_button.removeAttr('disabled')
      if $('#txtkey').val() != ''
        $("#dataslot#{snapMail.id}").remove()
        index = $("#divSnapmails").children("#dataslot#{snapMail.id}").index()
      else
        index = $("#divSnapmails div.card").length
      $('#divSnapmails').prepend getSnapmailHtml(snapMail, index)
      selectDays(snapMail.id, snapMail.notify_days)
      noSnapmailText()
      initPopup(snapMail.id)
      $("#snapmail-form").modal("hide")
      clearForm()
      rslidesInit()
      logCameraViewed() unless is_logged_intercom

    settings =
      data: o
      dataType: 'json'
      error: onError
      success: onSuccess
      type: action
      url: "#{Evercam.API_URL}snapmails#{queryString}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"

    $.ajax(settings)

logCameraViewed = ->
  is_logged_intercom = true
  data = {}
  data.has_snapmail = true

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

GetWeekdaysSelected = ->
  wDays = ''
  $.each($("input[class='days']:checked"), ->
    wDays += "#{$(this).val()},"
  )

  if wDays.length > 0
    return wDays.substring(0, wDays.lastIndexOf(','))
  wDays

clearForm = ->
  $('.formButtonCancel').click()
  $('#add-snapmail').removeAttr("disabled")
  $('#txtRecipient_tagsinput').removeClass('error-border')
  $('.caption').html 'New Snapmail'
  $('#txtkey').val ''
  $('#txtRecipient').val ''
  $('#ddlTimezone').val "Europe/Dublin"
  d = new Date
  $('#txtTime').val FormatNumTo2(d.getHours()) + ':' + FormatNumTo2(d.getMinutes())
  $('#divAlert').slideUp()
  $('.select2-search-choice').remove()
  $('span.tag').remove()
  $.fn.select2.defaults.reset()
  $('#ddlCameras :selected').each (i, selected) ->
    $(selected).removeAttr 'selected'
  camera_select.val(null).trigger("change")
  $('#uniform-chkMon .chk-span').removeClass 'checked'
  $("#chkMon").prop("checked", false)
  $('#uniform-chkTue .chk-span').removeClass 'checked'
  $('#chkTue').prop 'checked', false
  $('#uniform-chkWed .chk-span').removeClass 'checked'
  $('#chkWed').prop 'checked', false
  $('#uniform-chkThu .chk-span').removeClass 'checked'
  $('#chkThu').prop 'checked', false
  $('#uniform-chkFri .chk-span').removeClass 'checked'
  $('#chkFri').prop 'checked', false
  $('#uniform-chkSat .chk-span').removeClass 'checked'
  $('#chkSat').prop 'checked', false
  $('#uniform-chkSun .chk-span').removeClass 'checked'
  $('#chkSun').prop 'checked', false
  $('#uniform-chkAllDay .chk-span').removeClass 'checked'
  $('#chkAllDay').prop 'checked', false
  hideLoadingAnimation()

FormatNumTo2 = (n) ->
  if n < 10
    "0#{n}"
  else
    n

EditSnapmail = ->
  $("#divSnapmails").on "click", ".edit-snapmail", ->
    key = $(this).attr("data-val")
    $('#s2id_ddlCameras').hide()
    $('#txtkey').val key
    $('.caption').html 'Edit Snapmail'
    loadExistingSnapmail(key)

loadExistingSnapmail = (key) ->
  $('#ddlTimezone').val $('#txtTimezone' + key).val()
  emails = $('#txtRecipient' + key).val()
  if emails != null or emails != ''
    $('#txtRecipient').importTags(emails)
  days = $('#txtDays' + key).val().split(',')
  i = 0
  $.each days, (i, day) ->
    switch day
      when 'Monday'
        $('#uniform-chkMon .chk-span').addClass 'checked'
        $('#chkMon').prop 'checked', true
      when 'Tuesday'
        $('#uniform-chkTue .chk-span').addClass 'checked'
        $('#chkTue').prop 'checked', true
      when 'Wednesday'
        $('#uniform-chkWed .chk-span').addClass 'checked'
        $('#chkWed').prop 'checked', true
      when 'Thursday'
        $('#uniform-chkThu .chk-span').addClass 'checked'
        $('#chkThu').prop 'checked', true
      when 'Friday'
        $('#uniform-chkFri .chk-span').addClass 'checked'
        $('#chkFri').prop 'checked', true
      when 'Saturday'
        $('#uniform-chkSat .chk-span').addClass 'checked'
        $('#chkSat').prop 'checked', true
      when 'Sunday'
        $('#uniform-chkSun .chk-span').addClass 'checked'
        $('#chkSun').prop 'checked', true
  all_selected()
  cameraIds = $('#txtCamerasId' + key).val().split(',')
  camera_select.val(cameraIds).trigger("change")
  $('#txtTime').val $('#txtTime' + key).val()
  $('#s2id_ddlCameras').show()
  rslidesInit()

handleModelEvents = ->
  $("#snapmail-form").on "hide.bs.modal", ->
    clearForm()

RemoveSnapmail = ->
  $("#divSnapmails").on "click", "#delete-snapmail", ->
    key = $(this).attr("data-val")
    $('#dataslot' + key).fadeOut 500, ->
      onError = (jqXHR, status, error) ->
        $('#dataslot' + key).fadeIn 1000
        Notification.show('Error: ' + response.responseJSON.ExceptionMessage)

      onSuccess = (snapMail, status, jqXHR) ->
        $('#dataslot' + key).remove()
        noSnapmailText()

      settings =
        cache: false
        data: {}
        dataType: 'json'
        error: onError
        success: onSuccess
        type: 'DELETE'
        url: "#{Evercam.API_URL}snapmails/#{key}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"

      $.ajax(settings)

pauseSnapmail = ->
  $("#divSnapmails").on "click", ".pause-snapmail", ->
    control = $(this)
    key = control.attr("data-val")
    onError = (jqXHR, status, error) ->
      Notification.show('Error: ' + response.responseJSON.ExceptionMessage)

    onSuccess = (response, status, jqXHR) ->
      snapmail = response.snapmails[0]
      control.attr("data-status", "#{!snapmail.is_paused}")
      if snapmail.is_paused
        control.removeClass("fa-pause").addClass("fa-play")
        control.attr("title", "Resume Snapmail")
      else
        control.removeClass("fa-play").addClass("fa-pause")
        control.attr("title", "Pause Snapmail")

    settings =
      cache: false
      data: {is_paused: control.attr("data-status")}
      dataType: 'json'
      error: onError
      success: onSuccess
      type: 'PATCH'
      url: "#{Evercam.API_URL}snapmails/#{key}?api_id=#{Evercam.User.api_id}&api_key=#{Evercam.User.api_key}"

    $.ajax(settings)

initializeiCheck = ->
  $(".checkbox-snapmail input[type='checkbox']").iCheck
    checkboxClass: "chk-span"

  $("input[type='checkbox']").on 'ifChecked', (event) ->
    control_id = $(this).context.id
    if control_id is "chkAllDay"
      $("input[class='days']").iCheck('check')
    all_selected()

  $("input[type='checkbox']").on "ifUnchecked", (event) ->
    if !unselect_all
      control_id = $(this).context.id
      if control_id is "chkAllDay"
        $("input[class='days']").iCheck('uncheck')
      else
        unselect_all = true
        $("#chkAllDay").iCheck('uncheck')
    unselect_all = false

all_selected = ->
  if $("input[class='days']:checked").length is 7
    $("#chkAllDay").iCheck('check')

cloneSnapmail = ->
  $("#divSnapmails").on "click", ".snapmail-clone", ->
    key = $(this).attr("data-val")
    loadExistingSnapmail(key)
    $('#add-snapmail').click()
    $(".bb-alert").removeClass("alert-danger").addClass("alert-info")
    Notification.show "Successfully cloned"

showLoadingAnimation = ->
  $("#loading-image-div").removeClass("hide")
  $("#snapmail-form .modal-content").addClass("opacity0")

hideLoadingAnimation = ->
  $("#snapmail-form .modal-content").removeClass("opacity0")
  $("#loading-image-div").addClass("hide")

handleModelEvents = ->
  $(".modal").on "show.bs.modal", centerModal
  $(window).on "resize", ->
    $(".modal:visible").each centerModal

centerModal = ->
  $(this).css "display", "block"
  $dialog = $(this).find(".modal-dialog")
  offset = ($(window).height() - $dialog.height()) / 2
  if $(window).height() > $dialog.height()
    # Center modal vertically in window
    $dialog.css "margin-top", offset

window.initializeSnapmails = ->
  initSnapmails()
  initCameraSelect()
  initInputTags()
  initTimepicker()
  initNotification()
  saveSnapmail()
  clearForm()
  EditSnapmail()
  handleModelEvents()
  initializeiCheck()
  RemoveSnapmail()
  noSnapmailText()
  pauseSnapmail()
  cloneSnapmail()
  handleModelEvents()
