import sbt._
import Keys._
import com.kalmanb.sbt.CtagsPlugin

/**
 * The command called for updating tags (could be anything run after lib sources
 * are extracted, really) can also be customized -- see:
 *
 *    https://github.com/kalmanb/sbt-ctags/blob/master/conf/CustomCtagsPlugin.scala
 *
 * TODO: Only allows setting source extraction relative to project directory --
 * try to change that to a location shared across projects and look into
 * gem-ctags and the Vim path setup that bundler.vim does.
 */

object CustomCtagsPlugin extends CtagsPlugin {
  /**
   * [Optional] - Allows you to define where sources are unzipped to.
   *
   * Setting this to the same convention that this alternative uses, to ease
   * possible migration and because target/ just makes more sense as less
   * gitignore litter to add:
   *
   *   https://github.com/ceedubs/sbt-ctags
   *
   * @return the directory to store library sources.
   *         this local to the project base directory
   */
  override def ExternalSourcesDir: String = "target/sbt-ctags-dep-srcs"
}

