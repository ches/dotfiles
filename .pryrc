# Quick Cheats (make a cheat...):
#
#   - use -n switch for edit or edit-method to not auto-load changed file
#   - edit-method has a -p switch to create a monkey patch temp file. Cool :-)
#   - use `binding.pry` to invoke pry at runtime, for ad hoc debugging, etc.
#   - prevent history from saving at end of session, if you want to clear it:
#        Pry.config.history.should_save = false
#   - regex commands for extension are pretty cool:
#        https://github.com/pry/pry/wiki/Command-system
#
# Some possibilities to check out for debugging with Pry:
#
#   https://github.com/mon-ouie/pry_debug
#   https://github.com/AndrewO/ruby-debug-pry

VIM = '/Applications/Development/MacVim.app/Contents/MacOS/Vim -f'
Pry.editor = proc { |file, line| "#{VIM} #{file} +#{line}" }

Pry.config.commands.import Pry::ExtendedCommands::Experimental

# Example of custom prompt mucking
# Pry.config.prompt = proc { |obj, nest_level| "#{obj}:#{obj.instance_eval('_pry_').instance_variable_get('@output_array').count}> " }
# Pry.config.prompt = proc { |obj, nest_level| "#{obj}:#{obj.instance_eval('Pry').class_eval('@current_line')}> " }

begin
  require 'hirb'
rescue LoadError
  # Missing goodies, bummer
end

if defined? Hirb
  # Dirty hack to support in-session Hirb.disable/enable
  Hirb::View.instance_eval do
    def enable_output_method
      @output_method = true
      Pry.config.print = proc do |output, value|
        Hirb::View.view_or_page_output(value) || Pry::DEFAULT_PRINT.call(output, value)
      end
    end

    def disable_output_method
      Pry.config.print = proc { |output, value| Pry::DEFAULT_PRINT.call(output, value) }
      @output_method = nil
    end
  end

  Hirb.enable
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

#
# Random custom commands
#
Pry.commands.command(/!(\d+)/, "Replay a line of history, bash-style", :listing => "!hist") do |id|
  run "history --replay #{id}"
end

# Sed-style substitution for fixes in the current multi-line input buffer
Pry.commands.command /s\/(.*?)\/(.*?)/ do |source, dest|
  eval_string.gsub!(/#{source}/) { dest }
  run 'show-input'
end

# vim:filetype=ruby

