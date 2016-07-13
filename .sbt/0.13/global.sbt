/*
 * Some nifty sbt tricks here:
 *   http://underscore.io/blog/posts/2015/11/09/sbt-commands.html
 *
 * Sadly latest.integration is painfully slow to resolve for all global plugins.
 * And sadly Coursier doesn't like it :-/
 */

// Use test:console for a far superior REPL experience:
// http://lihaoyi.github.io/Ammonite/#Ammonite-REPL
libraryDependencies += "com.lihaoyi" % "ammonite-repl" % "0.6.2" % "test" cross CrossVersion.full
initialCommands in (Test, console) := """ammonite.repl.Main.run()"""

// Automatically clear the console on each triggered execution run
triggeredMessage in ThisBuild := Watched.clearWhenTriggered

// Ctrl-C for forked subprocesses, debugging, etc. won't quit SBT entirely. Maybe.
cancelable in Global := true

// Show current project in prompt
// For taking it to extremes, see: https://github.com/agemooij/sbt-prompt
shellPrompt in ThisBuild := (state => s"[${Project.extract(state).currentRef.project}] > ")

// Some voodoo to turn on the color property for REPL sessions
initialize ~= (_ => if (ConsoleLogger.formatEnabled) sys.props("scala.color") = "true")
