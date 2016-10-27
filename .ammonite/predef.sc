// vim:set ft=scala:
// See http://www.lihaoyi.com/Ammonite/#Configuration

// Not really digging the @ sign prompt.
repl.prompt() = "\n> "

// Love the double negative... enable compiler warnings
repl.compiler.settings.nowarnings.value = false

// Load utility definitions shared with default scala REPL
try interp.load.exec(ammonite.ops.home/".config"/'scala/"replinit.scala")
catch { case _: Exception => println("=== replrc not loaded! ===") }
