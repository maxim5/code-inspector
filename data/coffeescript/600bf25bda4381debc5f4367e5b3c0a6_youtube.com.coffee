# Manage which videos appear on your main YouTube subscription feed
#
# I have no idea why they removed this in the new design. Pretty stupid.
#
# Settings are stored in localStorage, one entry per hidden video, indexed by
# YouTube ID.
#
# Author: Robert Speicher <rspeicher@gmail.com>
# Version: 1.0.1

$ ->
  container = 'div.feed-item-container'

  # Storage
  # ------------------------------

  isHidden = (elem) ->
    localStorage.getItem("hidden:#{$(elem).data('id')}")?

  hide = (elem) ->
    $(elem).data('hidden', true)
    localStorage.setItem("hidden:#{$(elem).data('id')}", true)

  show = (elem) ->
    $(elem).data('hidden', false)
    localStorage.removeItem("hidden:#{$(elem).data('id')}")

  # Styles
  # ------------------------------

  # Add the CSS for our 'hide' button
  $('body').append('<style type="text/css" id="hide-feed-item-css"></style>')
  $('#hide-feed-item-css').text('''
    .hidden {
      display: none;
    }
    div.feed-header-subscribe label {
      margin-left: 15px;
    }
    span.hide-feed-item, span.show-feed-item {
      position: absolute;
      top: 15px;
      right: 2px;
      color: #BEBEBE;
      border: 1px solid #BEBEBE;
      padding: 3px 4px;
      font-weight: bold;
      font-size: 70%;
      line-height: 6px;
    }
    span.show-feed-item {
      color: green;
      border-color: green;
    }
    span.hide-feed-item:hover, span.show-feed-item:hover {
      color: black;
      border-color: black;
      cursor: pointer;
    }
  ''')

  # DOM
  # ------------------------------

  # Copy the "Show uploads only" checkbox and modify it so we can toggle hidden items
  setupGlobalToggle = ->
    subscribe_toggle = $('div.feed-header-subscribe label')
    hidden_toggle    = subscribe_toggle.clone()
    form_elements    = $('span', subscribe_toggle).first().clone()

    $(hidden_toggle).text('Show hidden items') # This removes everything inside the label, including form elements
    $(hidden_toggle).prepend(form_elements)
    $('span',  hidden_toggle).removeClass('checked')
    $('input', hidden_toggle).attr('name', 'show_hidden').attr('id', 'show_hidden').removeAttr('checked').removeClass('feed-filter')
    $(hidden_toggle).click -> processItems()
    $('div.feed-header-subscribe').append(hidden_toggle)

  # Determines which toggle button to show for <tt>elem</tt> based on whether
  # or not that element is hidden
  setupToggleButtons = (elem) ->
    if isHidden(elem)
      $('.hide-feed-item', elem).addClass('hidden')
      $('.show-feed-item', elem).removeClass('hidden')
    else
      $('.hide-feed-item', elem).removeClass('hidden')
      $('.show-feed-item', elem).addClass('hidden')

  processItems = ->
    # Set up each feed item
    $(container).each ->
      # Get and store the video ID
      id = $('a[href^="/watch"]', this).first().attr('href').replace(/.*v=([^&]+).*/, '$1')
      $(this).data('id', id)

      # Set up its data-hidden attribute
      $(this).data('hidden', isHidden(this))

      # Decide whether or not to hide this item
      if $('#show_hidden:checked').length == 1
        $(this).removeClass('hidden')
      else
        if isHidden(this)
          $(this).addClass('hidden')
        else
          $(this).removeClass('hidden')

      # Immediately load this item's thumbnail
      # As videos are removed from the feed, the videos below it are pushed up.
      # If they get pushed up far enough, their thumbnails won't load as we
      # scroll down.
      $('img[data-thumb][alt*="Thumbnail"]', this).attr('src', -> $(this).data('thumb'))

      # Add the elements to hide or show this video
      if $('.hide-feed-item, .show-feed-item', this).length == 0
        $('.feed-item-main', this).append('<span class="hide-feed-item" title="Hide this item">X</span>')
        $('.feed-item-main', this).append('<span class="show-feed-item" title="Show this item">&#x2713;</span>')

      # Decide which toggle button to hide based on the item's status
      setupToggleButtons(this)

      # Add the click handler for the toggle buttons
      $('.hide-feed-item', this).click =>
        hide(this)
        setupToggleButtons(this)
        processItems()
      $('.show-feed-item', this).click =>
        show(this)
        setupToggleButtons(this)
        processItems()

  # Initial page load
  setupGlobalToggle()
  processItems()

  # TODO: Would be great to bind to the actual AJAX event that's populating this
  $('button.feed-load-more').live('click', -> setTimeout(processItems, 500))
