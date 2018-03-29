#
# data.rb - Main executable for GUI interface
#
# ====================================================================
# Copyright (c) 2014 Tony Doan <tdoan@tdoan.com>.  All rights reserved.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution.  The terms
# are also available at http://github.com/tdoan/kang/tree/master/COPYING.
# If newer versions of this license are posted there, you may use a
# newer version instead, at your option.
# ====================================================================
#

require 'kang/offset_data'

module Kang
  class Data
    attr_accessor :line_number,:match_string,:regex_string

    def initialize(reg_text="",match_text="")
      self.match_string = match_text
      self.regex_string = reg_text
      @line_number=1
      @multiline=false
      update_match
    end

    def match_string=(match_string)
      @match_string = match_string
      update_match
    end

    def regex_string=(regex_string)
      @regex_string = regex_string
      begin
        @re = Regexp.new(@regex_string,extended)
        update_match
      rescue RegexpError
        @re = nil
      end
    end

    def multiline=(multiline)
      if multiline
        @multiline = true
      else
        @multiline = false
      end
      update_match
    end

    def extended=(extended)
      if extended
        @extended = Regexp::EXTENDED
      else
        @extended = false
      end
      update_regexp
    end

    def multiline
      @multiline
    end

    def extended
      @extended
    end

    def match
      @match
    end

    def line_number=(line_number)
      if (self.num_lines) and (line_number > 0) and (line_number <= self.num_lines)
        @line_number = line_number
        update_match
      end
    end

    def num_lines
      #if (defined? @line_matches) and @multiline
        #@line_matches.size - 1 #line_match is padded with a leading nil to deal with 0 offset
      #else
        #nil
      #end
      @match_string.scan(/^.*$/).size
    end

    def line(line_num)
      @line_matches[line_num][0]
    end

    def regex_valid?
      @re.nil? ? false : true
    end

    def match?
      @match ? true : false
    end

    def match_begin(group=0)
      if @match
        @match.begin(group)
      else
        nil
      end
    end

    def match_end(group=0)
      if @match
        @match.end(group)
      else
        nil
      end
    end

    def match_group_count
      if @match
        @match.length
      else
        0
      end
    end

    def matches
      if @match and (@match.length > 0)
        if @match.names and @match.names.size>0
          names = @match.names.unshift("0")
        else
          names = Range.new(0,@match.length-1).to_a.collect{|o| o.to_s}
        end
        return names.zip(@match.to_a)
      else
        []
      end
    end

    private

    def update_match
      if @re
        if @multiline
          @line_matches = [nil]
          @match_string.scan(/^.*$/) {|m| @line_matches << $~}
          @line_number = 1 if @line_number > self.num_lines
          line_match = @line_matches[@line_number]
          @match = @re.match(line_match[0])
          @match = OffSetMatchData.new(line_match.begin(0), @match) if @match
        else
          @match = @re.match(@match_string)
        end
      else
        @match = nil
      end
    end

    def update_regexp
      begin
        @re = Regexp.new(@regex_string, extended)
      rescue RegexpError
        @re = nil
      end
      update_match
    end
  end
end
