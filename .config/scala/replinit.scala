/*
 * Utility code intended to be loaded into scala REPL sessions with
 * `scala -i <file>`. There is no actual config file supported by the REPL, so
 * there is no conventional path for this file. I use a wrapper script.
 */

/**
 * Quick-and-dirty profiling spot check helper. Have a look at ScalaMeter for
 * more principled perf tests, including its inline feature for ad hoc work.
 *
 * Hat tip: [[http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala]]
 */
object profiling {
  def timeit[R](block: => R): R = timeit(1)(block)

  def timeit[R](iterations: Int)(block: => R): R = {
    require(iterations > 0, "Must time at least one iteration")

    val start = System.nanoTime
    (1 to iterations) foreach (i => block)
    val mean = (System.nanoTime - start) / iterations

    if (iterations > 1) println(s"Mean time: ${mean / 1000} µs, $iterations iterations")
    else println(s"Elapsed time: ${mean / 1000} µs")

    // Produce result. Could fold above, sure, but that introduces collection ops
    block
  }
}

// The object is a hack to get the overloads into the same compilation unit,
// otherwise the scala REPL can't deal with them (Ammonite can). The scala REPL
// probably can if an intermediate file is used to load this one that contains
// `:paste path/to/this/file`, but the namespacing is probably a good idea anyway.
import profiling._
