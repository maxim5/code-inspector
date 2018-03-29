###
Mechanics: Impulses
Science Project (http://scienceproject.codeplex.com)
P. Hurst 2011-2012
###

$ ->
  
  CloseMenu = ()->
    $("[data-impulses-context-menu-open=true]").attr("data-impulses-context-menu-open",null);
    $("[data-impulses-context-menu]").remove()


  $("*").live "click", (e)->
    switch (e.which)
      when 1 # left button
        # Check if we're clicking on a menu
        menu = $(this).parents("[data-impulses-context-menu]")
        if (menu.length)
          e.preventDefault()
          # Trigger impulse
          $.impulses.trigger(this)

        else
          # Close a menu if it's open
          CloseMenu()

      # when 2 # middle button
      when 3 # right click
        impulseParent = $(this).parents("[data-impulses]").first()
        if (impulseParent)
          e.preventDefault()
          impulseParent.attr("data-impulse-context-menu-open","true")

          # Loop through data attributes and discover any available menus