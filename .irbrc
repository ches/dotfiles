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
