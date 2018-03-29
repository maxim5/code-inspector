require 'optparse'

module FacebookCommands
  class << self
    def stream(*args)
      if args.empty?
        puts 'stream by itself does nothing'
        return
      end

      if (args.first == 'help')
        puts 'stream publish'
        PUBLISH_PARAMS.each { |param|
          puts "  -#{param[0,1]}, --#{param} #{param.upcase}"
        }
        puts 'stream read'
        return
      end

      Stream.send(args.shift, *args)
    end
    
    def status(*args)
      # So we can do 'facebook status "Foo bar baz buz."'
      if args.first == 'help'
        puts 'This is an alias for `stream publish -m`'
        return
      else
        if args.first.nil?
          puts "Reading input from STDIN."
          message = STDIN.read.gsub("\n","   ")
        else
          message = args.first
        end
        Stream.send('publish', "-m " + message)
      end
    end

    private

    PUBLISH_PARAMS = ['message',
                      'picture',
                      'name',
                      'link',
                      'caption',
                      'description',
                      'source']

    class Stream
      class << self
        def publish(*args)
          url, parameters, option_parser = 'me/feed', Hash.new, OptionParser.new

          PUBLISH_PARAMS.each { |param|
            short = "-#{param[0,1]}"
            long = "--#{param} #{param.upcase}"
            option_parser.on(short, long) { |value|
              parameters[param] = value
            }
          }
          option_parser.on('--friend FRIEND') { |friend|
            url = "#{friend}/feed"
          }
          option_parser.parse(args)

          unless parameters.has_key?('message')
            puts 'MESSAGE is required'
            return
          end

          result = JSON.parse(FacebookCL.post(url, parameters).body)
          if (result.has_key?('id'))
            puts "Success!  Post id is: #{result['id']}"
          elsif (result.has_key?('error'))
            puts 'Bummer. Error:'
            puts result['error']['message']
          else
            puts 'Unknown error..'
          end
        end

        def read(*args)
          # TODO: Make it so we can read from anyone's feed.
          stream = FacebookCL.get('me/feed')['data']
          if args.shift == 'filter'
            must_contain = args.first
          end
          
          # Believe it or not, there /is/ a reason I do the stream.each
          # twice. If I only did it once, I'd have to check must_contain
          # every time. It's never going to change. I'd rather check it
          # one time, then loop through, than check every time, which is
          # inefficient.
          # We show the id so someone can copypaste it to use the 'like' command
          # that I'm about to code.
          if must_contain
            stream.each { |post|
              if post['message'].include? must_contain
                puts "[#{post['id']}] #{post['from']['name']} | #{post['message']}"
                puts "\n------------------------------------------------------------\n"
              end
            }
          else
            stream.each { |post|
              puts "[#{post['id']}] #{post['from']['name']} | #{post['message']}"
              puts "\n------------------------------------------------------------\n"
            }
          end
        end

        def method_missing(verb, *args)
          puts "#{verb} is not an action on stream"
        end
      
        # Yay pyramid!
      end # end class
    end # end Stream
  end # end class
end # end module