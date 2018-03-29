require 'tuple.rb'
require 'schema.rb'
require 'aggregator.rb'
require 'clock.rb'

Now = 0

class Stream

    attr_reader :data, :children, :schema

    def initialize(parent = nil, &block)
        @parent = parent
        @children = []
        @update_actions = []
        @data = []
        @parent.subscribe self if @parent
        @schema = Schema.new
        block.call(@schema) if block
    end


    def subscribe(stream)
        @children << stream
        @data.each { |t| stream.add(t) }
    end


    def add(tuple)
        data << tuple
        @children.dup.each { |child| child.add(tuple) }
        @update_actions.each { |action| action.call }
    end


    def remove(tuple)
        data.shift
        @children.dup.each { |child| child.remove(tuple) }
        @update_actions.each { |action| action.call }
    end


    def on_update(&block)
        @update_actions << block
    end


    def [](interval)
        WindowedStream.new(self, interval)
    end


    def last
        data.last
    end

#     def last(number = 1)
#         Class.new(Stream) do
#             define_method :add do |tuple|
#             end

#             define_method :remove do |tuple|
#             end

#             define_method :data do
#                 if number > 1
#                     @parent.data.last(number)
#                 else
#                     @parent.data.last
#                 end
#             end
#         end.new(self)
#     end


    def filter(&predicate)
        Class.new(Stream) do
            define_method :add do |tuple|
                super(tuple) if predicate.call(tuple)
            end

            define_method :remove do |tuple|
                super(tuple) if predicate.call(tuple)
            end

             define_method :pretty_print do |pp|
                 pp.group(1, "ยงfilter:", '') {
                     pp.seplist(self.pretty_print_instance_variables, lambda { text ',' }) {|v|
                        pp.breakable
                         v = v.to_s if Symbol === v
                         pp.text v
                         pp.text '='
                         pp.group(1) {
                            pp.breakable ' '
                            pp.pp(self.instance_eval(v))
                         }
                     }
                 }
             end
        end.new(self)
    end


     def map(&block)
        Class.new(Stream) do
            define_method :add do |tuple|
                super(tuple.select(block.call(tuple)))
            end

            define_method :remove do |tuple|
                super(block.call(tuple))
            end

             define_method :pretty_print do |pp|
                 pp.group(1, "ยงmap:", '') {
                     pp.seplist(self.pretty_print_instance_variables, lambda { text ',' }) {|v|
                         pp.breakable
                         v = v.to_s if Symbol === v
                         pp.text v
                         pp.text '='
                         pp.group(1) {
                             pp.breakable ' '
                             pp.pp(self.instance_eval(v))
                         }
                     }
                 }
             end
        end.new(self)
     end


     def groupby(*fields, &block)
         Map.new(self, fields, &block)
     end


#     def partitionby(*fields, &block)
#         child = Class.new(Stream) do
#             def initialize(parent)
#                 @substreams = Hash.new { |hash, key| hash[key] = Stream.new }
#                 super
#             end

#             define_method :add do |tuple|
#                 key = fields.map { |field| tuple[field] }
#                 key = key[0] if key.length == 1
#                 @substreams[key].add(tuple)
#             end

#             define_method :remove do |tuple|
#                 key = fields.map { |field| tuple[field] }
#                 key = key[0] if key.length == 1
#                 @substreams[key].remove(tuple)
#             end

#             define_method :data do
#                 @substreams.values.map { |stream| block.call(stream).data }.flatten.sort { |a, b| a.timestamp <=> b.timestamp }
#             end
#         end.new(self)
#     end


     def sum(field)
         SumAggregator.new(self, field)
     end


     def avg(field = nil)
         AvgAggregator.new(self, field)
     end




#     def min(field)
#         fold(data.first[field], lambda { |m, n| m = [m, n[field]].min })
#     end


#     def sort(*fields)
#         Class.new(Stream) do
#             define_method :add do |tuple|
#             end

#             define_method :remove do |tuple|
#             end

#             define_method :data do
#                 comparator = lambda do |a, b, f|
#                     first, rest = f
#                     result = a[first] <=> b[first]
#                     if result == 0 && rest
#                         comparator.call(a, b, rest)
#                     else
#                         result
#                     end
#                 end
#                 @parent.data.sort { |a, b| comparator.call(a, b, fields) }
#             end

#         end.new(self)
#     end


#     def join(other, field_a, field_b = field_a)
#         Class.new(Stream) do
#             define_method :add do |tuple|
#             end

#             define_method :remove do |tuple|
#             end

#             define_method :data do
#                 result = []
#                 for elem in @parent.data
#                     for other_elem in other.data
#                         if elem[field_a] == other_elem[field_b]
#                             result << elem.merge(other_elem)
#                         end
#                     end
#                 end
#                 result
#             end
#         end.new(self)
#     end


    def >>(action)
        action.call(self)
    end


    def pretty_print_instance_variables
        [:data]
    end


    def to_s
        "ยง[#{data.join(" ")}]"
    end
end


def groupby(*fields, &block)
    lambda { |stream| stream.groupby(*fields, &block) }
end


def partitionby(*fields, &block)
    lambda { |stream| stream.partitionby(*fields, &block) }
end


def map(&block)
    lambda { |stream| stream.map(&block) }
end


def filter(&predicate)
    lambda { |stream| stream.filter(&predicate) }
end


class WindowedStream < Stream
    def initialize(parent, interval)
        @clock = Clock.instance
        @clock.on_advance do
            to_remove = data.select { |tuple| !interval.include?(tuple.timestamp - @clock.now) }
            to_remove.each { |tuple| remove(tuple) }
        end
        @interval = interval
        super(parent)
    end


    def add(tuple)
        super if @interval.include?(tuple.timestamp - @clock.now)
    end
end


class Map
    def initialize(parent, fields, &block)
        @parent = parent
        @substreams = {}
        @results = {}
        @fields = fields
        @children = []
        @block = block
        @parent.subscribe self
    end


    def add(tuple)
        key = @fields.map { |field| tuple[field] }
        key = key[0] if key.length == 1
        @substreams[key] ||= Stream.new
        @results[key] ||= @block.call(@substreams[key])
        @substreams[key].add(tuple)
    end


    def remove(tuple)
        key = @fields.map { |field| tuple[field] }
        key = key[0] if key.length == 1
        @substreams[key].remove(tuple)
    end


    def keys
        @results.keys
    end


    def [](key)
        @results[key]
    end


    def to_s
        @substreams.to_s
    end

#     def join(other)
#         Class.new(HashStream) do
#             define_method :add do |tuple|
#             end

#             define_method :remove do |tuple|
#             end

#             define_method :keys do
#                 @parent.keys & other.keys
#             end

#             define_method :[] do |key|
#                 p = @parent
#                 Class.new(Stream) do
#                     define_method :data do
#                         value_a = p[key]
#                         value_b = other[key]

#                         if value_a.nil? or value_b.nil?
#                             nil
#                         else
#                             [value_a.data, value_b.data]
#                         end
#                     end
#                 end.new
#             end
#         end.new(self)
#     end
end


class Fixnum
    def s
        self
    end

    def min
        self * 60
    end
end
