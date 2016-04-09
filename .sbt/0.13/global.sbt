/*
 * Some nifty sbt tricks here:
 *   http://underscore.io/blog/posts/2015/11/09/sbt-commands.html
 *
 * Sadly latest.integration is painfully slow to resolve for all global plugins.
 */

// Use test:console for a far superior REPL experience:
// http://lihaoyi.github.io/Ammonite/#Ammonite-REPL
libraryDependencies += "com.lihaoyi" % "ammonite-repl" % "latest.integration" % "test" cross CrossVersion.full
initialCommands in (Test, console) := """ammonite.repl.Main.run("")"""

// Automatically clear the console on each triggered execution run
triggeredMessage in ThisBuild := Watched.clearWhenTriggered

// Ctrl-C for forked subprocesses, debugging, etc. won't quit SBT entirely
cancelable in Global := true
