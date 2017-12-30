/* Some nifty sbt tricks here:
 *   http://underscore.io/blog/posts/2015/11/09/sbt-commands.html
 */

// Automatically clear the console on each triggered execution run
triggeredMessage in ThisBuild := Watched.clearWhenTriggered

// Ctrl-C for forked subprocesses, debugging, etc. won't quit SBT entirely. Maybe.
cancelable in Global := true

// Some voodoo to turn on the color property for REPL sessions
initialize ~= (_ => if (ConsoleLogger.formatEnabled) sys.props("scala.color") = "true")
