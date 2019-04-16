// vim:set ft=scala:
// See http://www.lihaoyi.com/Ammonite/#Configuration

// Not really digging the @ sign prompt.
repl.prompt() = "amm> "

interp.configureCompiler { comp =>
  // Love the double negative... enable compiler warnings
  comp.settings.nowarnings.value = false
  comp.settings.deprecation.value = true
  comp.settings.unchecked.value = true
  // For sake of cats and such
  comp.settings.YpartialUnification.value = true
}

// Load utility definitions shared with default scala REPL
try repl.load.exec(ammonite.ops.home/".config"/'scala/"replinit.scala")
catch { case _: Exception => println("=== replinit defs not loaded! ===") }
