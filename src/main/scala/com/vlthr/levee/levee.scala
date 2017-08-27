package com.vlthr.levee
import com.vlthr.levee.filters._
import com.vlthr.levee.core._
import com.vlthr.levee.util._
import com.vlthr.levee.parser.LeveeParser
import com.vlthr.levee.core.error._
import scala.util.{Success, Failure, Try}

object Levee {

  /** Create a new context object to register configuration and state for rendering */
  def newContext(): Context = Context.createNew()

  /** Convenience method for rendering a template file without a context */
  def render(path: String, params: Map[String, Any]): Try[String] = {
    val c =
      Context.createNew().withParams(params)
    c.renderFile(path)
  }
}
