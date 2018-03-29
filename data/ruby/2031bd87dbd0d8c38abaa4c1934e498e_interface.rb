# Interface abstracts a lot of the elements of the console interface out into
# one location.
module Interface
  # Displays the main header for the program.
  def header
    clear_screen
    print @header
  end

  # Displays the main menu for the program and returns their selection as a
  # symbol.
  def main_menu
    selection = input(@menu) {|o| (1..2) === o.to_i }.to_i
 
    if 1 == selection
      :continue
    elsif 2 == selection
      :quit
    end
  end

  # Displays the message passed to the function, and gets a string from the
  # standard input stream. This method takes a block that specifies what is
  # an acceptable string and will loop until it is satisfied.
  def input( message )
    selection = nil

    print message
    until yield(selection = gets.strip)
      print message
    end

    selection
  end

  # Displays a simple seperator.
  def seperator
    puts "*"*49
  end

  # Simply goes to the next line.
  def newline
    puts
  end

  # Clears the screen by placing many newline characters into the stream,
  # pushing old text off screen.
  def clear_screen
    40.times { puts }
  end

  # Displays a message and waits until enter is pressed before continuing
  def pause
    print "Press Enter To Continue..."
    $stdin.getc
  end

  # Pauses and clears screen... Imagine that.
  def pause_and_clear_screen
    pause
    clear_screen
  end

@header = <<'HEADER'

    To further fuel your gambling addiction,
at Rapleaf's request, Justin Camerer presents...
 ____  _            _        _            _    _
|  _ \| |          | |      | |          | |  | |
| |_) | | __ _  ___| | __   | | __ _  ___| | _| |
|  _ <| |/ _` |/ __| |/ /   | |/ _` |/ __| |/ / |
| |_) | | (_| | (__|   < |__| | (_| | (__|   <|_|
|____/|_|\__,_|\___|_|\_\____/ \__,_|\___|_|\_(_)

HEADER

end
