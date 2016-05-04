/*
 * Utility code intended to be loaded into scala REPL sessions with
 * `scala -i <file>`. There is no actual config file supported by the REPL, so
 * there is no conventional path for this file. I use a wrapper script.
 */

/**
 * Quick-and-dirty profiling spot check helper.
 *
 * Hat tip: <http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala>
 */
def timeit[R](block: => R): R = {
  val now = System.nanoTime
  val result = block
  val micros = (System.nanoTime - now) / 1000
  println(s"Elapsed time: $micros µs")
  result
}
