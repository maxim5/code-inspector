# Place all the behaviors and hooks related to the matching controller here.
# All this logic will automatically be available in application.js.
# You can use CoffeeScript in this file: http://coffeescript.org/
subs_table = null
subscriptions = null
format_time = null
month = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

sendAJAXRequest = (settings) ->
  token = $('meta[name="csrf-token"]')
  if token.size() > 0
    headers =
      "X-CSRF-Token": token.attr("content")
    settings.headers = headers
  xhrRequestChangeMonth = jQuery.ajax(settings)
  true

switchTab = ->
  $(".nav-tab-#{Evercam.request.tabpath}").tab('show')

handleTabClick = ->
  $('.nav-tabs a').on 'click', ->
    click_path = $(this).attr('data-target').replace('#', '')
    if window.history and window.history.pushState
      path = window.Evercam.request.rootpath
      window.history.pushState({} , "#{click_path}", "#{path}/#{click_path}")
  $(".nav-tabs").tabdrop "layout"

handleBackForwardButton = ->
  window.addEventListener 'popstate', (e) ->
    tab = document.location.pathname
    .replace(window.Evercam.request.rootpath, '')
    .split('/')[1]
    $(".nav-tab-#{tab}").tab('show')

onCheckoutConfirmCard = ->
  $(".add-card-to-continue").on 'click', ->
    has_credit_card = $("#has-credit-card").val()
    if has_credit_card is "false"
      $("#plan-descprition").html('Add a credit card before changing your plan.')
      $("#change-plan-action").val("")
      $("#btn-change-plan").val($(".stripe-button-el span").text())
      $('#upgradeDwongradeModal').modal('show')
      $("#upgradeDwongradeModal").on "show.bs.modal", ->
        centerModal(this)
      return false

onUpgradeDownGrade = ->
  $('.change-plan').on 'click', ->
    quantity = parseInt($("##{$(this).attr("data-plan-id")}-qty").val())
    if quantity is 0
      Notification.show "Please enter quantity."
      return false
    clearModal()
    plan_control = $(this)
    plan_change_to =  plan_control.val()
    has_credit_card = $("#has-credit-card").val()
    $("#change-plan-id").val(plan_control.attr('data-plan-id'))
    $("#upgradeDwongradeModal").on "show.bs.modal", ->
      if has_credit_card is "false"
        $("#plan-descprition").html('Add a credit card before changing your plan.')
        $("#change-plan-action").val("")
        $("#btn-change-plan").val($(".stripe-button-el span").text())
      else
        if plan_change_to is "Upgrade"
          $("#section-downgrade").hide()
          $("#change-plan-action").val("upgrade")
          $("#btn-change-plan").val("Upgrade my plan")
          $("#plan-descprition").html("The #{plan_control.attr('data-period')} cost for you to upgrade " +
            "to the #{plan_control.attr('data-plan')} will be #{plan_control.attr('data-price')} #{plan_control.attr('data-period')}. We will credit you for any time you have not used on your current plan against the cost of this.")
        else if plan_change_to is "Switch to Monthly" || plan_change_to is "Switch to Annual"
          $("#change-plan-action").val("switch")
          $("#btn-change-plan").val(plan_change_to)
          $("#plan-descprition").html("Your plan and add-ons switch to #{plan_control.attr('data-period')} billing and cost " +
              "to the #{plan_control.attr('data-plan')} will be #{plan_control.attr('data-price')} #{plan_control.attr('data-period')}. We will credit you for any time you have not used on your current plan against the cost of this.")
        else
          $("#change-plan-action").val("downgrade")
          $("#btn-change-plan").val("Downgrade my plan")
          $("#plan-descprition").html("The #{plan_control.attr('data-plan')} plan will change your " +
            "#{plan_control.attr('data-period')} cost to #{plan_control.attr('data-price')}. We will credit you for any time you have not used on your current plan against the cost of this.")
          $("#section-downgrade").show()
      centerModal(this)
    true

clearModal = ->
  $("#change-plan-id").val("")
  $("#change-plan-action").val("")
  $("#plan-descprition").show()
  $(".modal-footer").show()
  $("#confirm-upgrading").hide()
  $("#section-downgrade").hide()
  true

changePlan = ->
  $("#btn-change-plan").on 'click', ->
    if $("#has-credit-card").val() is "false"
      $(".stripe-button-el").click()
      $('.modal').modal('hide')
      return
    action = $("#change-plan-action").val()

    if action is "downgrade" && $("#downgrade-plan").val() is ''
      Notification.show "Please type 'downgrade' to confirm."
      return
    if $("#change-plan-id").val() is ""
      Notification.show "Empty plan id."
      return
    if action is "upgrade"
      $(".change-plan-desc").text("One moment while we upgrade your account...")
    else if action is "switch"
      $(".change-plan-desc").text("One moment while we switch your plan and add-ons...")

    if action is "upgrade" || action is "switch"
      $("#plan-descprition").hide()
      $(".modal-footer").hide()
      $("#confirm-upgrading").show()

    plan_id = $("#change-plan-id").val()
    data = {}
    data.plan_id = plan_id
    data.quantity = $("##{plan_id}-qty").val()

    onError = (jqXHR, status, error) ->
      if action is "upgrade"
        $("#plan-descprition").show()
        $(".modal-footer").show()
        $("#confirm-upgrading").hide()
      false

    onSuccess = (result, status, jqXHR) ->
      if result.success
        Notification.show "Your account has been successfully #{action}d."
        location.reload()
      else
        Notification.show "Failed to #{action} plan."
        $('.modal').modal('hide')

    settings =
      cache: false
      data: data
      dataType: 'json'
      error: onError
      success: onSuccess
      contentType: "application/x-www-form-urlencoded"
      type: 'POST'
      url: "/v1/users/#{Evercam.User.username}/billing/plans/change"

    sendAJAXRequest(settings)

centerModal = (model) ->
  $(model).css "display", "block"
  $dialog = $(model).find(".modal-dialog")
  offset = ($(window).height() - $dialog.height()) / 2
  $dialog.css "margin-top", offset

validateLicenceForm = ->
  $('#submit-licences').on "click", ->
    price_monthly = parseInt($("#new-total-price-monthly").text())
    price_annual = parseInt($("#new-total-price-annual").text())
    one_day = parseInt($("#24-hours-recording-quantity").val())
    one_day_annual = parseInt($("#24-hours-recording-annual-quantity").val())
    $("#pay-custom-licence").hide()
    if price_monthly isnt 0 || price_annual isnt 0 || one_day isnt 0 || one_day_annual isnt 0
      if $("#has-credit-card").val() is "false"
        $("#saveSubscriptions").val($(".stripe-button-el span").text())
        $("#checkout-message").hide()
        $("#add-card-message").show()
      else
        $("#checkout-message").show()
        $("#add-card-message").hide()
        $("#saveSubscriptions").val("Save Plans")
        $("#is-custom-payment").val("false")
      $("#payNowModal").modal("show")
    else
      Notification.show "Please add licences."

  $("#saveSubscriptions").on "click", ->
    if $("#has-credit-card").val() is "false"
      $(".stripe-button-el").click()
      $('#payNowModal').modal('hide')
    else
      $("#form-make-payment").submit()

showAlertMessage = ->
  infinity_req = parseInt($("#licence-required-infinity").text())
  seven_day_req = parseInt($("#licence-required-seven-day").val())
  thirty_day_req = parseInt($("#licence-required-thirty-day").val())
  ninety_day_req = parseInt($("#licence-required-ninety-day").val())

  seven_day_current = parseInt($("#7-days-recording-current-qty").val())
  thirty_day_current = parseInt($("#30-days-recording-current-qty").val())
  ninety_day_current = parseInt($("#90-days-recording-current-qty").val())
  infinity_current = 0
  total_valid = 0

  if $("#custom-licence").html()
    custom_rows  = $('td.no-of-licences')
    custom_rows.each ->
      if $(this).hasClass('7')
        seven_day_current = seven_day_current + parseInt($(this).text())
      if $(this).hasClass('30')
        thirty_day_current = thirty_day_current + parseInt($(this).text())
      if $(this).hasClass('90')
        ninety_day_current = ninety_day_current + parseInt($(this).text())
      if $(this).hasClass('-1')
        infinity_current = infinity_current + parseInt($(this).text())
    if $(".custom-licence-status").hasClass("red")
      $('div#message-custom-licence').show()

  if !isNaN(seven_day_req) && seven_day_req != 0
    if seven_day_current is 0 || seven_day_current < seven_day_req
      total_valid = total_valid + (seven_day_req - seven_day_current)
      changeTotalColor()
  if !isNaN(thirty_day_req) && thirty_day_req != 0
    if thirty_day_current is 0 || thirty_day_current < thirty_day_req
      total_valid = total_valid + (thirty_day_req - thirty_day_current)
      changeTotalColor()
  if !isNaN(ninety_day_req) && ninety_day_req != 0
    if ninety_day_current is 0 || ninety_day_current < ninety_day_req
      total_valid = total_valid + (ninety_day_req - ninety_day_current)
      changeTotalColor()
  if !isNaN(infinity_req)
    if infinity_current is 0 || infinity_current < infinity_req
      total_valid = total_valid + (infinity_req - infinity_current)
      changeTotalColor()

  lic = "7"
  if seven_day_current < seven_day_req
    $(".licence-message-7").prepend( " #{seven_day_req - seven_day_current}")
    $(".licence-message-7").addClass('active')
    $(".licence-message-7").show()
    lic = '7'
  if thirty_day_current < thirty_day_req
    $(".licence-message-30").prepend(" #{thirty_day_req - thirty_day_current}")
    $(".licence-message-30").addClass('active')
    $(".licence-message-30").show()
    lic = '30'
  if ninety_day_current < ninety_day_req
    $(".licence-message-90").prepend(" #{ninety_day_req - ninety_day_current}")
    $(".licence-message-90").addClass('active')
    $(".licence-message-90").show()
    lic = '90'
  if infinity_current < infinity_req
    $(".licence-message-infinity").prepend(" #{infinity_req - infinity_current}")
    $(".licence-message-infinity").addClass('active')
    $(".licence-message-infinity").show()
    lic = 'infinity'
  if $(".licence-message-#{lic}").hasClass('active')
    if $(".licence-message-#{lic}").text().endsWith ","
      text = $(".licence-message-#{lic}").text()
      new_text = text.slice(0, -1)
      $(".licence-message-#{lic}").text(new_text)

changeTotalColor = ->
  $(".licence-alert").show()
  $("#current-total-quantity-monthly").removeClass("green").addClass("red")
  $("#current-total-quantity-annual").removeClass("green").addClass("red")
  $("#total-required-licence").removeClass("green").addClass("red")

FormatNumTo2 = (n) ->
  if n < 10
    return "0#{n}"
  else
    return n

loadBillingHistory = ->
  loadInvoiceHistory()
  data = {}
  onError = (jqXHR, status, error) ->
    $("#no-history").removeClass("hide")
    $("#billing-history").hide()

  onSuccess = (results, status, jqXHR) ->
    if !results
      $("#no-history").removeClass("hide")
      $("#billing-history").hide()
      return
    for item in results.data
      created_date = new Date(item.created*1000)
      row = $('<tr>')
      cell_1 = $('<td>')
      span = $('<span>')
      span.attr("data-toggle", "tooltip")
      span.attr("data-placement", "top")
      span.attr("title", "#{created_date}")
      span.append(document.createTextNode("#{FormatNumTo2(created_date.getDate())} #{month[created_date.getMonth()]}  #{created_date.getFullYear()}"))
      cell_1.append(span)
      row.append(cell_1)

      cell_2 = $('<td>')
      cell_2.attr("id", item.id)
      description = item.description
      failure_message = item.failure_message
      if !description and !failure_message
        getChargeDescription(item.invoice, item.id)
      else if failure_message
        cell_2.append(document.createTextNode(failure_message))
      else
        cell_2.append(document.createTextNode(description))
      row.append(cell_2)

      cell_3 = $('<td>', {class: "text-right"})
      small = $('<small>', {class: "grey"})
      if item.paid
        small.append(document.createTextNode("Paid"))
      else if failure_message
        small.append(document.createTextNode("Failed"))
      else
        small.append(document.createTextNode("Pending"))
      cell_3.append(small)
      cell_3.append(document.createTextNode(" \u20AC#{item.amount/100}"))
      row.append(cell_3)
      $("#billing-history tbody").append(row)

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json; charset=utf-8"
    type: 'GET'
    url: "/v1/users/#{Evercam.User.username}/billing/history"

  sendAJAXRequest(settings)

getChargeDescription = (invoice_id, col_id) ->
  data = {}
  data.invoice_id = invoice_id
  onError = (jqXHR, status, error) ->
    false

  onSuccess = (result, status, jqXHR) ->
    if result.description
      $("##{col_id}").text(result.description)
    else
      $("##{col_id}").text("Created By Admin")

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json; charset=utf-8"
    type: 'GET'
    url: "/v1/users/#{Evercam.User.username}/billing/history"

  sendAJAXRequest(settings)

loadInvoiceHistory = ->
  data = {}
  data.invoices = true
  onError = (jqXHR, status, error) ->
    $("#no-invoice").removeClass("hide")
    $("#invoice-history").hide()

  onSuccess = (results, status, jqXHR) ->
    if !results
      $("#no-invoice").removeClass("hide")
      $("#invoice-history").hide()
      return
    for item in results.data
      created_date = new Date(item.date*1000)
      row = $('<tr>')
      cell_1 = $('<td>')
      a_link = $('<a>')
      a_link.attr("href", "/v1/users/#{Evercam.User.username}/billing/invoices/#{item.id}")
      a_link.append(document.createTextNode("#{FormatNumTo2(created_date.getDate())} #{month[created_date.getMonth()]}  #{created_date.getFullYear()}"))
      cell_1.append(a_link)
      row.append(cell_1)

      cell_2 = $('<td>')
      span = $('<span>', {class: "send-email"})
      a_link = $('<a>')
      a_link.attr("href", "/v1/users/#{Evercam.User.username}/billing/invoices/#{item.id}/send")
      a_link.append(document.createTextNode("Email "))
      span.append(a_link)
      icon = $('<i>', {class: "fa"})
      icon.addClass("fa-send-o")
      span.append(icon)
      cell_2.append(span)
      row.append(cell_2)

      cell_3 = $('<td>', {class: "text-right"})
      small = $('<small>', {class: "green"})
      if item.paid
        small.append(document.createTextNode("Paid"))
      else
        small.removeClass("green").addClass("red")
        small.append(document.createTextNode("Pending"))
      cell_3.append(small)
      cell_3.append(document.createTextNode(" \u20AC#{item.total/100}"))
      row.append(cell_3)
      $("#invoice-history tbody").append(row)
    true

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/json; charset=utf-8"
    type: 'GET'
    url: "/v1/users/#{Evercam.User.username}/billing/history"

  sendAJAXRequest(settings)

handleTabOpen = ->
  $('.tab-custom-licence').on 'shown.bs.tab', ->
    if $(".custom-licence-status").hasClass("red")
      $("#submit-licences").hide()
      $("#pay-custom-licences").show()
    else
      $("#submit-licences").hide()
      $("#pay-custom-licences").hide()
  $('.tab-custom-licence').on 'hide.bs.tab', ->
    $("#submit-licences").show()
    $("#pay-custom-licences").hide()

payCustomLicence = ->
  $('#pay-custom-licences').on "click", ->
    if $("#has-credit-card").val() is "false"
      $("#saveSubscriptions").val($(".stripe-button-el span").text())
      $("#checkout-message").hide()
      $("#add-card-message").show()
      $("#pay-custom-licence").hide()
    else
      $("#checkout-message").hide()
      $("#add-card-message").hide()
      $("#pay-custom-licence").show()
      $("#saveSubscriptions").val(" Pay ")
      $("#is-custom-payment").val("true")
    $("#payNowModal").modal("show")

addlicencesrquired = ->
  licen = " (" + $('#total-required-licence').text() + ")"
  licences = $('#total-required-licence').text()
  $('h3#licences').append(licen)

initializeInvoiceTable = ->
  if $(".subscriptions #insight-id").val()
    table = $('#custom-invoices').DataTable({
      ajax: {
        url: $('#insight-custom-url').val(),
        dataSrc: 'result',
        dataType: 'json',
        cache: true,
        type: 'GET',
        error: (xhr, error, thrown) ->
          if xhr.responseJSON
            Notification.show(xhr.responseJSON.message)
      },
      columns: [
        {data: ( row, type, set, meta ) ->
          path = "/v1/users/" + Evercam.User.username + "/billing/invoices"
          return "<a href = '#{path}/#{row.NUMBER}/custom'>#{row.NUMBER}</a>"
        , className: 'id'},
        {data: "POSTDATE", sClass: 'date'},
        {data: ( row, type, set, meta ) ->
          if row.REFERENCE
            return row.REFERENCE
          else
            return "None"
        , className: 'reference'},
        {data: ( row, type, set, meta ) ->
          return row.FRGAMTVATINC.toFixed(2)
        , className: 'amount'}
        {data: ( row, type, set, meta ) ->
          return row.FRGDUEAMT.toFixed(2)
        , className: 'due'}
        {data: ( row, type, set, meta ) ->
          if row.NEXTCREATEDATE
            return row.NEXTCREATEDATE
          else
            return "None"
        , className: 'next_due'},
      ],
      autoWidth: false,
      info: false,
      bPaginate: false,
      bFilter: false,
      "language": {
        "emptyTable": "No data available"
      },
      order: [[ 0, "desc" ]],
    })

addCreditCard = ->
  if $("#has-credit-card").val() is "false"
    $("#open-modal").val("Add Card")
  $("#open-modal").on "click", ->
    if $("#has-credit-card").val() is "false"
      $(".stripe-button-el").click()
      $("#add-modal").modal('hide')
    else
      $("#add-modal").modal('show')

initializeSubscriptionTable = ->
  if subscriptions != null
    subs_table = $('#subscription_data').DataTable({
      ajax: {
        url: $("#sub-url").val(),
        dataSrc: 'subscription',
        dataType: 'json',
        cache: true,
        type: 'GET',
        error: (xhr, error, thrown) ->
          if xhr.responseJSON
            Notification.show(xhr.responseJSON.message)
      },
      columns: [
        { data: ( row, type, set, meta ) ->
          return row.plan.name
        },
        { data: "quantity" },
        { data: ( row, type, set, meta ) ->
          return "<i class='fa fa-eur'></i>  #{calculatePrice(row)}"
        },
        { data: ( row, type, set, meta ) ->
          start = new Date(moment.utc(row.created*1000).format('MM/DD/YYYY, HH:mm:ss'))
          start_date = format_time.formatDate(start, 'd M Y, H:i:s')
          return start_date
        },
        { data: ( row, type, set, meta ) ->
          if row.plan.id.indexOf('annual') != -1
            period = "Annual"
          else
            period = "Month"
          return period
        },
        { data: ( row, type, set, meta ) ->
          renderRenewalOptions(row)
        },
        { data: ( row, type, set, meta ) ->
          time = row.created*1000
          start = new Date(moment.utc(time).format('MM/DD/YYYY, HH:mm:ss'))
          if row.cancel_at_period_end is false
            mon = parseInt(start.getMonth())
            if row.plan.id.indexOf('annual') != -1
              start.setMonth(mon + 12)
            else
              start.setMonth(mon + 1)
            return format_time.formatDate(start, 'd M Y, H:i:s')
          else
            return ""
        },
        {data: (row, type, set, meta) ->
          return renderEditOptions(row)
        }
      ],
      autoWidth: false,
      info: false,
      bSort: false,
      bPaginate: false,
      bFilter: false,
      "language": {
        "emptyTable": "No data available"
      },
      order: [[ 0, "desc" ]],
      drawCallback: ->
        initializePopup()
        editSubscription()
        hideImgLoader()
        deleteEditSubscription()
    })

calculatePrice = (row) ->
  price = row.plan.amount / 100
  return price*parseInt(row.quantity)

checkPriceShow = ->
  $("#edit-modal #sub-type").on "change", ->
    showPrice("edit")
  $("#edit-modal #sub-quantity").on "input", ->
    showPrice("edit")
  $("#add-modal #sub-type").on "change", ->
    showPrice("add")
  $("#add-modal #sub-quantity").on "input", ->
    showPrice("add")
  $("#open-modal").on "click", ->
    showPrice("add")

showPrice = (option) ->
  selected = $("##{option}-modal #sub-type").find('option:selected')
  price = selected.data('price')
  price = price * $("##{option}-modal #sub-quantity").val()
  if !isNaN(price) && price != 0
    total_price = " #{price}"
    $("##{option}-modal #sub-price .fa-eur").text(total_price)

renderEditOptions = (row) ->
  div = $('<div>')
  div1 = $('<div>', {class: "form-group"})
  divedit = $('<i>',{class: 'sub-edit fa fa-edit icons'})
  divedit.attr('title', 'edit')
  divedit.attr('href','#')
  divedit.attr('data-toggle','modal')
  divedit.attr('data-target','#edit-modal')
  divedit.attr('sub-id', row.id )
  divedit.attr('sub-quantity', row.quantity )
  divedit.attr('sub-plan', row.plan.id )
  divedit.attr('sub-price', calculatePrice(row) )
  div1.append(divedit)
  div.append(div1)
  return div.html()

renderRenewalOptions = (row) ->
  div1 = $('<div>')
  div = $('<div>', {class: "input-group"})
  select = $('<select>', {class: "renew-del form-control reveal"})
  option = $('<option>', {value: row.id, text: "Yes"})
  option.attr('data-subtype', row.plan.id)
  option.attr('data-subquantity', row.quantity)
  if row.cancel_at_period_end is false
    option.attr('selected', 'selected')
  select.append(option)
  option = $('<option>', {value: row.id, text: "No"})
  if row.cancel_at_period_end is true
    option.attr('selected', 'selected')
  select.append(option)
  div.append(select)
  div1.append(div)
  return div1.html()

addSubscription = ->
  $("#add-modal #add_subs").on "click", ->
    $("#add-modal").modal('hide')
    quantity = $("#add-modal #sub-quantity").val()
    if quantity is 0 || !quantity.toString().match(/^[0-9]+$/)
      Notification.show "Quantity is invalid"
      return
    showImgLoader()
    NProgress.start()
    $("#add-modal #add_subs").attr('disabled','disabled')
    data = {}
    data.plan = $("#add-modal #sub-type").val()
    data.quantity = $("#add-modal #sub-quantity").val()

    onError = (jqXHR, status, error) ->
      hideImgLoader()
      NProgress.done()
      $("#add-modal #add_subs").removeAttr('disabled')
      Notification.show "Something went wrong.\nPlease try again."

    onSuccess = (results, status, jqXHR) ->
      NProgress.done()
      $("#add-modal #add_subs").removeAttr('disabled')
      location.reload()

    settings =
      cache: false
      data: data
      dataType: 'json'
      error: onError
      success: onSuccess
      contentType: "application/x-www-form-urlencoded"
      type: 'POST'
      url: "/v1/payments"

    sendAJAXRequest(settings)

editSubscription = ->
  $("#subscription_data .sub-edit").on "click", ->
    $("#edit-modal #sub-type").val($(this).attr('sub-plan'))
    $("#edit-modal #sub-quantity").val($(this).attr('sub-quantity'))
    $("#edit-modal #edit_subs").attr('subscription_id' , $(this).attr('sub-id'))
    showPrice("edit")

deleteEditSubscription = ->
  $("#edit-modal #edit_subs").off("click").on "click", ->
    sub_id = $(this).attr("subscription_id")
    sub_type = $("#edit-modal #sub-type").val()
    sub_quantity = $("#edit-modal #sub-quantity").val()
    message = "You have successfuly edited your subscription."
    saveEditedSubscriptions(sub_id, sub_type, sub_quantity, message)
  $(".subscriptions .renew-del").on "change", ->
    selected = $(this).find('option:selected')
    if selected.text() is "No"
      deleteSub(selected.val())
    else
      sub_id = selected.val()
      sub_type = selected.data('subtype')
      sub_quantity = selected.data('subquantity')
      msg = 'You have successfuly activated auto-renewal for your subscription'
      saveEditedSubscriptions(sub_id, sub_type, sub_quantity, msg)

saveEditedSubscriptions = (id, type, quantity, message) ->
  $("#edit-modal").modal('hide')
  if quantity is 0 || !quantity.toString().match(/^[0-9]+$/)
    Notification.show "Quantity is invalid"
    return
  showImgLoader()
  NProgress.start()
  $("#subscriptions .delete-sub2").attr('disabled','disabled')
  data = {}
  data.subscription_id = id
  data.plan = type
  data.quantity =  quantity

  onError = (jqXHR, status, error) ->
    $("#add-modal #edit_subs").removeAttr('disabled')
    Notification.show "Something went wrong.\nPlease try again"
    NProgress.done()
    hideImgLoader()

  onSuccess = (results, status, jqXHR) ->
    $("#edit-modal #edit_subs").removeAttr('disabled')
    Notification.show message
    NProgress.done()
    subs_table.ajax.reload()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/x-www-form-urlencoded"
    type: 'PATCH'
    url: "/v1/users/#{Evercam.User.username}/billing"

  sendAJAXRequest(settings)

deleteSub = (sub_id) ->
  showImgLoader()
  NProgress.start()
  data = {}
  data.subscription_id = sub_id

  onError = (jqXHR, status, error) ->
    msg = 'Something went wrong while deleting your subscription.' +
      '\nPlease try again'
    Notification.show msg
    NProgress.done()
    hideImgLoader()

  onSuccess = (results, status, jqXHR) ->
    NProgress.done()
    msg = "You have successfuly cancelled auto-renewal for your subscription."
    Notification.show msg
    subs_table.ajax.reload()

  settings =
    cache: false
    data: data
    dataType: 'json'
    error: onError
    success: onSuccess
    contentType: "application/x-www-form-urlencoded"
    type: 'Delete'
    url: "/v1/users/#{Evercam.User.username}/billing"

  sendAJAXRequest(settings)

showImgLoader = ->
  $("#subscriptions #img-subscription-loader").removeClass 'hide'
  $("#subscriptions #subscription_data").addClass 'opacity-transparent'

hideImgLoader = ->
  $("#subscriptions #img-subscription-loader").addClass 'hide'
  $("#subscriptions #subscription_data").removeClass 'opacity-transparent'

initializePopup = ->
  $(".popbox2").popbox
    open: ".open-delete"
    box: ".box2"
    arrow: ".arrow"
    arrow_border: ".arrow-border"
    close: ".closepopup"

window.initializeSubscription = ->
  format_time = new DateFormatter()
  subscriptions = jQuery.parseJSON($("#subs-all").val()) if $("#subs-all").val()
  NProgress.done()
  Notification.init(".bb-alert")
  initializeInvoiceTable()
  validateLicenceForm()
  showAlertMessage()
  addlicencesrquired()
  handleTabOpen()
  payCustomLicence()
  addSubscription()
  initializeSubscriptionTable()
  checkPriceShow()
  addCreditCard()
  setTimeout loadBillingHistory, 2000
  handleTabClick()
  setTimeout switchTab, 200
  handleBackForwardButton()

window.initializeChangePlan = ->
  onUpgradeDownGrade()
  onCheckoutConfirmCard()
  changePlan()

$ ->
  $('[data-toggle="tooltip"]').tooltip()
  return
