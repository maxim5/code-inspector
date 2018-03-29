module Dam
  
  
  PLACEHOLDER_PATTERN = %r{:[^:@/-]+}
  
  # provides the basis of the DSL to define a stream
  class StreamDefinition
    def initialize
      @filters = []
      @limit = 10
    end
    
    def limit(amount = nil)
      amount.nil? ? @amount : @amount = amount
    end
    
    def filters
      @filters
    end
    
    def accepts(args = {})
      @filters << args
    end
    
    def params
      ParamsProxy
    end
  end
  
  
  # a class that will allow us to note which param has been used from the placeholders
  class ParamsProxy
    attr_reader :key
    def initialize(key)
      @key = key
    end
    
    def self.[] key
      ParamsProxy.new(key)
    end
  end
  
  # a templated stream is one which contains placeholders, and thus will only be defined through his instances
  class TemplatedStream
    attr_reader :name
    
    def initialize(name, definition)
      @name = name
      @definition = definition
      
      extract_placeholders!
      make_glob_pattern!
      make_regexp!
    end
    
    def apply(params)
      if params.is_a? String
        name = params
        params = extract_params(name)
      else
        name = replace_placeholders(params)
      end
      Stream.new(name, @definition, :params => params)
    end
    
    def instances
      elems = Dam::Storage.keys(@glob_pattern)
      elems.collect {|elem| apply(elem) }
    end
    
    def matches? what
      what =~ @regexp
    end
    
    private
    
    def replace_placeholders(params)
      name = @name
      params.each_pair do |key, value|
        name = name.gsub(":#{key}", value.to_s)
      end
      
      name
    end
    
    def extract_params(what)
      Hash[*@placeholders.collect {|pat| pat[1..-1].to_sym}.zip(@regexp.match(what).captures).flatten]
    end
    
    def extract_placeholders!
      @placeholders = @name.scan(PLACEHOLDER_PATTERN)
    end
    
    def make_glob_pattern!
      @glob_pattern = @placeholders.inject(@name) do |name, placeholder|
        name.sub(placeholder, "*")
      end
    end
    
    def make_regexp!
      @regexp = Regexp.new(@placeholders.inject(@name) do |name, placeholder|
        name.sub(placeholder, "([^/:-]+)")
      end)
    end
  end
  
  class Stream
    def Stream.lookup(name)
      @streams ||= {}
      if @streams.has_key? name
        @streams[name]
      else
        template = @streams.values.find {|stream| stream.respond_to?(:instances) && stream.matches?(name)  }
        template.apply(name) if template
      end
    end
    
    def Stream.[](name)
      lookup(name)
    end
    
    def Stream.register(name, stream)
      @streams ||= {}
      @streams[name] = stream
      stream
    end
    
    def Stream.has_placeholder? string
      string =~ PLACEHOLDER_PATTERN
    end
    
    def Stream.all
      @streams.values.collect {|stream| stream.respond_to?(:instances) ? stream.instances : stream }.flatten
    end
    
    
    attr_reader :name
        
    def initialize(name, definition, params = {})
      @name = name
      @definition = definition
      @params = params.delete(:params)
    end
    
    def limit
      @definition.limit
    end
    
    def filters
      @definition.filters
    end
    
    def all
      Dam::Storage.get(self.name).collect do |json|
        Activity.from_json json
      end
    end
    
    def first
      Activity.from_json(Dam::Storage.head(self.name))
    end

    def matches? activity
      filters.any? do |filter|
        return true if filter == :all || filter == activity.type.name
        
        filter.any? do |key, value|
          attr_match(value, activity.attributes[key.to_s])
        end
      end
    end
    
    def instantiate!
      ensure_exists!
      self
    end
    
    private
    
    def ensure_exists!
      if Dam::Storage.database.keys("stream:#{name}").size == 0
        Dam::Storage.database.push_head("stream:#{name}", 1)
        Dam::Storage.database.pop_head("stream:#{name}")
      end
    end
  
    def attr_match(condition, element)
      # match a nil element with a nil condition
      return condition.nil? if element.nil?
      
      if condition.respond_to? :each_pair # condition is a hash
        condition.all? do |key, value|
          eval_condition(value, element[key])
        end
      else
        eval_condition(condition, element)
      end
    end
    
    def eval_condition(arg, val)
      case arg
      when ParamsProxy
        @params[arg.key] == val
      when Proc
        arg.call(val)
      else
        arg == val
      end
    end
  end
end