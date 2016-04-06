// Use test:console for a far superior REPL experience:
// http://lihaoyi.github.io/Ammonite/#Ammonite-REPL
libraryDependencies += "com.lihaoyi" % "ammonite-repl" % "0.5.6" % "test" cross CrossVersion.full

initialCommands in (Test, console) := """ammonite.repl.Main.run("")"""

