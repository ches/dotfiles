#!/usr/bin/env ruby

# Rake task completions for bash, using a cache file to speed things up.
# http://onrails.org/articles/2006/11/17/rake-command-completion-using-rake

exit 0 unless File.file?(File.join(Dir.pwd, 'Rakefile'))

require 'rubygems'
require 'rake'
DOTCACHE = File.join(File.expand_path('~'), ".rake_task_cache" , Dir.pwd.hash.to_s)
RAKE_FILES = FileList[ __FILE__, 'Rakefile', 'lib/tasks/**/*.rake', 'vendor/plugins/*/tasks/**/*.rake']
file DOTCACHE => RAKE_FILES do
  tasks = `rake --silent --tasks`.split("\n").map { |line| line.split[1] }
  require 'fileutils'
  dirname = File.dirname(DOTCACHE)
  FileUtils.mkdir_p(dirname) unless File.exists?(dirname)
  File.open(DOTCACHE, 'w') { |f| f.puts tasks }
end

Rake::Task[DOTCACHE].invoke
tasks = File.read(DOTCACHE)

exit 0 unless /\brake\b/ =~ ENV["COMP_LINE"]

after_match = $'
task_match = (after_match.empty? || after_match =~ /\s$/) ? nil : after_match.split.last
tasks = tasks.select { |t| /^#{Regexp.escape task_match}/ =~ t } if task_match

# handle namespaces
if task_match =~ /^([-\w:]+:)/
  upto_last_colon = $1
  after_match = $'
  tasks = tasks.map { |t| (t =~ /^#{Regexp.escape upto_last_colon}([-\w:]+)$/) ? "#{$1}" : t }
end

puts tasks
exit 0
