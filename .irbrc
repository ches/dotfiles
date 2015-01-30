# You want RubyGems available by default.
require 'rubygems'
require 'pp'
require 'irb/ext/save-history'

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"

# Do crazy syntax coloring and stuff if gems are available.
# $ gem install wirble hirb bond interactive_editor
begin
  require 'wirble'

  # Enter 'vi' or 'mate' to open the editor, save and close to evaluate,
  # enter again to recall your previous text.
  # require 'interactive_editor'

  # Bond is better completion, for IRB and more:
  # http://tagaholic.me/2009/07/22/better-irb-completion-with-bond.html
  require 'bond'
  Bond.start

  # Custom 'view formats' for object classes, and paging.
  # Gives ActiveRecord table views out of the box.
  require 'hirb'

  # Really only using wirble for color these days.
  # Skip libs because we get it all elsewhere (completion, pp, rubygems)
  Wirble.init(:skip_history => true, :skip_prompt => true, :skip_libraries => true)
  Wirble.colorize

  # Should be called after wirble
  Hirb.enable

  # Example custom Bond completion, for the #reload method below
  Bond.complete(:method=>'reload') { $" }

  # Toggle completion of inherited methods
  # http://tagaholic.me/2009/07/30/changing-readline-completions-with-key-combo.html
  # TODO: this is a bit wonky
  def toggle_object_complete
    # default mode
    if @object_complete
      Bond.recomplete(:object => 'Object', :place => :last)
      Bond.recomplete(:object => 'Object', :on => /([^.\s]+)\.([^.\s]*)$/, :place => :last)
    else
      non_inherited_methods = proc { |e|
        e.object.is_a?(Module) ? e.object.methods(false) : e.object.class.instance_methods(false)
      }
      Bond.recomplete(:object => 'Object', :place => :last, &non_inherited_methods)
      Bond.recomplete(:object => 'Object', :on => /([^.\s]+)\.([^.\s]*)$/, :place => :last, &non_inherited_methods)
    end
    @object_complete = !@object_complete
  end
rescue LoadError
  # But if not, we'll do the good stuff anyway
  require 'irb/completion'

  # Easily see all the methods local to an object's class
  # Wirble's `po` method does this
  class Object
    def local_methods
      (methods - Object.instance_methods).sort
    end
    private :local_methods
  end

  # Completion for the ri methods defined below, for irb/completion.
  #
  # Note that the completion matches only object instance methods, but the ri
  # methods do support finding instance methods from a class, e.g.
  # `String.ri_count`.
  #
  # TODO: need a Bond version of this
  RICompletionProc = proc { |input|
    bind = IRB.conf[:MAIN_CONTEXT].workspace.binding

    case input
    when /(\s*(.*)\.ri_)(.*)/
      pre = $1
      receiver = $2
      meth = $3 ? /\A#{Regexp.quote($3)}/ : /./

      begin
        candidates = eval("#{receiver}.methods", bind).map do |meth|
          case meth
          when /[A-Za-z_]/ then meth
          else %{"#{meth}"}           # needs escaping
          end
        end
        candidates = candidates.grep(meth)
        candidates.map { |s| pre + s.to_s }
      rescue Exception
        candidates = []
      end
    when /([A-Z]\w+)#(\w*)/
      klass = $1
      meth = $2 ? /\A#{Regexp.quote($2)}/ : /./
      candidates = eval("#{klass}.instance_methods(false)", bind)
      candidates = candidates.grep(meth)
      candidates.map { |s| "'#{klass}##{s}'" }
    else
      IRB::InputCompletor::CompletionProc.call(input)
    end
  }

  #Readline.basic_word_break_characters= " \t\n\"\\'`><=;|&{("
  Readline.basic_word_break_characters= " \t\n\\><=;|&"
  Readline.completion_proc = RICompletionProc
end

# Log Rails stuff like SQL/Mongo queries to $stdout if in Rails console
if defined?(Rails) && Rails.respond_to?(:logger)    # Rails 3 style
  require 'logger'
  Rails.logger = Logger.new($stdout)

  def toggle_db_logging
    if $db_logging_enabled
      ActiveRecord::Base.logger = Logger.new(nil) if defined?(ActiveRecord)
      Mongoid.logger = Logger.new(nil) if defined?(Mongoid)
      $db_logging_enabled = false
    else
      ActiveRecord::Base.logger = Rails.logger if defined?(ActiveRecord)
      Mongoid.logger = Rails.logger if defined?(Mongoid)
      $db_logging_enabled = true
    end
  end

  toggle_db_logging
# Rails < 3.0.0
elsif ENV.include?('RAILS_ENV') && !Object.const_defined?('RAILS_DEFAULT_LOGGER')
  require 'logger'
  RAILS_DEFAULT_LOGGER = Logger.new($stdout)
end

# Re-installing a gem locally as you change code gets old, no? Enter:
# http://tagaholic.me/2009/02/05/local-gem-loads-your-current-code-now.html
#
# Just set the env var in irb or in client code you're working on to switch
# between rubygems and your local edits.
#
#   ENV['LOCAL_GEMS'] = 'true'
#
if ENV['LOCAL_GEMS']
  begin
    require 'local_gem'
    require 'local_gem/override'
    load File.join(ENV['HOME'], '.localgems')
  rescue LoadError
    puts "*** local_gem doesn't appear to be installed. `gem install local_gem`"
  end
end

# Reload a matching library that has been required, if you changed it
def reload(require_regex)
  $".grep(/#{require_regex}/).each { |e| $".delete(e) && require(e) }
end

# Iterative development: watch a file and eval it when it changes.
# Ctrl-C to exit the subshell and 'return' to irb.
# Hat tip: http://po-ru.com/diary/iterative-development-in-irb/
#
# These days you should probably just use Guard :-)
def loop_execute(file)
  old_mtime = nil

  loop do
    # print("\e[sWaiting...")
    sleep(0.5) while (mtime = File.stat(file).mtime) == old_mtime
    # print("\e[u\e[K")

    begin
      r = eval(File.read(file))
      puts("=> #{r.inspect}")
    rescue IRB::Abort
      puts("Abort")
      return
    rescue Exception => e
      puts("#{e.class}: #{e.message}\n#{e.backtrace.join("\n")}")
    end

    old_mtime = mtime
  end
end

# The below adds nice ri documentation via obj.ri_* with completion,
# and supports the old Wirble way with `r 'String#count'` and completion
# as well, using above RICompletionProc. Courtesy:
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
      # This is to approximate instance and class methods in inherited order
      candidates = ancestors.zip(self.class.ancestors).flatten.compact.uniq

      # Checks both class- and instance-specific forms simply for more precise
      # output--could check 'klass.meth' only for speed, the search is equivalent.
      candidates = candidates.flat_map { |klass| ["#{klass}::#{meth}", "#{klass}##{meth}"] }
    else
      candidates = self.class.ancestors.map { |klass| "#{klass}##{meth}" }
    end

    candidates.each do |candidate|
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
      unless puts_ri_documentation_for(self, md[1])
        "Ri doesn't know about ##{meth}"
      end
    else
      super
    end
  end

  # Helpful for classes instead of instances, e.g. `String.ri_ '#count'`
  def ri_(meth)
    unless puts_ri_documentation_for(self, meth.to_s)
      "Ri doesn't know about ##{meth}"
    end
  end
end
