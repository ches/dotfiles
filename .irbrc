# Require RubyGems by default.
require 'rubygems'
require 'irb/completion'

IRB.conf[:AUTO_INDENT] = true

# Gimme some documentation
# Wirble's implementation is a little nicer...
# def ri(*names)
#   system(%{ri #{names.map {|name| name.to_s}.join(" ")}})
# end

# Lifted from Leopard's default in /etc/irbrc:
# Set up permanent history.
HISTFILE = "~/.irb_history"
MAXHISTSIZE = 100
begin
  histfile = File::expand_path(HISTFILE)
  if File::exists?(histfile)
    lines = IO::readlines(histfile).collect { |line| line.chomp }
    puts "Read #{lines.nitems} saved history commands from '#{histfile}'." if $VERBOSE
    Readline::HISTORY.push(*lines)
  else
    puts "History file '#{histfile}' was empty or non-existant." if $VERBOSE
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

# For crazy syntax coloring and stuff, gem install wirble and uncomment below.
# Note that wirble does lots of the things above for you -- history, completion,
# ri shortcut, etc., so remove that or customize the init options below.
begin
  require 'pp'    # for :skip_libraries; otherwise loads pp, rubygems & irb/completion
  require 'wirble'
  Wirble.init(:skip_prompt => true, :skip_history => true, :skip_libraries => true)
  Wirble.colorize
rescue
end

# Easily see all the methods local to an object's class
class Object
  def local_methods
    (methods - Object.instance_methods).sort
  end
end

# Log SQL to STDOUT if in Rails console
if ENV.include?('RAILS_ENV') && !Object.const_defined?('RAILS_DEFAULT_LOGGER')
  require 'logger'
  RAILS_DEFAULT_LOGGER = Logger.new(STDOUT)
end
