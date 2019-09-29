/* User global config for sbt, the Scala build tool. Applies to all sbt 1.x projects.
 *
 * Some nifty sbt tricks here:
 *   http://underscore.io/blog/posts/2015/11/09/sbt-commands.html
 */

// Automatically clear the console on each triggered execution run
triggeredMessage in ThisBuild := Watched.clearWhenTriggered

// Ctrl-C for forked subprocesses, debugging, etc. won't quit SBT entirely. Maybe.
cancelable in Global := true

// Some voodoo to turn on the color property for REPL sessions
initialize ~= (_ => if (ConsoleLogger.formatEnabled) sys.props("scala.color") = "true")

/* Ammonite REPL {{{
 *
 * Use test:run or `test:runMain amm` for a superior REPL experience:
 *   http://ammonite.io/#Ammonite-REPL
 */
libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case "2.11" => "1.6.7"
    case _ ⇒ "1.7.1"
  }
  "com.lihaoyi" % "ammonite" % version % Test cross CrossVersion.full
}

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

/* // Retrive all source JARs, for the `source` command to work
(fullClasspath in Test) ++= {
  (updateClassifiers in Test).value
    .configurations
    .find(_.configuration == Test.name)
    .get
    .modules
    .flatMap(_.artifacts)
    .collect { case (a, f) if a.classifier == Some("sources") => f }
} */
// }}}

// vim:set foldmethod=marker:
