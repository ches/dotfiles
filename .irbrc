# You want RubyGems available by default.
require 'rubygems'

IRB.conf[:AUTO_INDENT] = true

# For crazy syntax coloring and stuff, gem install wirble
begin
  require 'wirble'
  Wirble.init(:skip_prompt => true)
  Wirble.colorize
rescue
  # But if not, we'll do the good stuff anyway
  require 'irb/completion'

  # Lifted from Leopard's default in /etc/irbrc
  # Set up permanent history. Wirble does this for you, with the same opts
  HISTFILE = "~/.irb_history"
  MAXHISTSIZE = 1000
  begin
    histfile = File::expand_path(HISTFILE)
    if File::exists?(histfile)
      lines = IO::readlines(histfile).collect { |line| line.chomp }
      puts "Read #{lines.nitems} saved history commands from '#{histfile}'." if $VERBOSE
      Readline::HISTORY.push(*lines)
    else
      puts "History file '#{histfile}' was empty or non-existent." if $VERBOSE
    end
    Kernel::at_exit do
      lines = Readline::HISTORY.to_a.reverse.uniq.reverse
      lines = lines[-MAXHISTSIZE, MAXHISTSIZE] if lines.nitems > MAXHISTSIZE
      puts "Saving #{lines.length} history lines to '#{histfile}'." if $VERBOSE
      File::open(histfile, File::WRONLY|File::CREAT|File::TRUNC) { |io| io.puts lines.join("\n") }
    end
  rescue => e
    puts "Error when configuring permanent history: #{e}" if $VERBOSE
  end

  # Easily see all the methods local to an object's class
  # Wirble's `po` method does this
  class Object
    def local_methods
      (methods - Object.instance_methods).sort
    end
    private :local_methods
  end
end

# Log SQL to STDOUT if in Rails console
if ENV.include?('RAILS_ENV') && !Object.const_defined?('RAILS_DEFAULT_LOGGER')
  require 'logger'
  RAILS_DEFAULT_LOGGER = Logger.new(STDOUT)
end

# The below adds nice ri documentation via obj.ri_* with completion,
# and supports the old Wirble way with `r 'String#count` and completion
# as well. Courtesy:
#
#   http://eigenclass.org/hiki/irb+ri+completion
#
# The author recommends FastRI because RI is so slow.
module Kernel
  def r(arg)
    puts `ri "#{arg}"`
  end
  private :r
end

class Object
  def puts_ri_documentation_for(obj, meth)
    case self
    when Module
      candidates = ancestors.map{|klass| "#{klass}::#{meth}"}
      candidates.concat(class << self; ancestors end.map{|k| "#{k}##{meth}"})
    else
      candidates = self.class.ancestors.map{|klass|  "#{klass}##{meth}"}
    end
    candidates.each do |candidate|
      #puts "TRYING #{candidate}"
      # desc = `fri '#{candidate}'`
      # unless desc.chomp == "nil"
      # uncomment to use ri (and some patience)
      desc = `ri -T '#{candidate}' 2>/dev/null`
      unless desc.empty?
        puts desc
        return true
      end
    end
    false
  end
  private :puts_ri_documentation_for

  def method_missing(meth, *args, &block)
    if md = /ri_(.*)/.match(meth.to_s)
      unless puts_ri_documentation_for(self,md[1])
        "Ri doesn't know about ##{meth}"
      end
    else
      super
    end
  end

  def ri_(meth)
    unless puts_ri_documentation_for(self,meth.to_s)
      "Ri doesn't know about ##{meth}"
    end
  end
end

RICompletionProc = proc{|input|
  bind = IRB.conf[:MAIN_CONTEXT].workspace.binding
  case input
  when /(\s*(.*)\.ri_)(.*)/
    pre = $1
    receiver = $2
    meth = $3 ? /\A#{Regexp.quote($3)}/ : /./ #}
    begin
      candidates = eval("#{receiver}.methods", bind).map do |m|
        case m
        when /[A-Za-z_]/; m
        else # needs escaping
          %{"#{m}"}
        end
      end
      candidates = candidates.grep(meth)
      candidates.map{|s| pre + s }
    rescue Exception
      candidates = []
    end
  when /([A-Z]\w+)#(\w*)/ #}
    klass = $1
    meth = $2 ? /\A#{Regexp.quote($2)}/ : /./
    candidates = eval("#{klass}.instance_methods(false)", bind)
    candidates = candidates.grep(meth)
    candidates.map{|s| "'" + klass + '#' + s + "'"}
  else
    IRB::InputCompletor::CompletionProc.call(input)
  end
}
#Readline.basic_word_break_characters= " \t\n\"\\'`><=;|&{("
Readline.basic_word_break_characters= " \t\n\\><=;|&"
Readline.completion_proc = RICompletionProc
