package mustachemeta

import scala.meta._

case class Data(title: String, layout: String)

object Templating {
  def parseMustache(templateFile: String): List[String] = "title" :: "layout" :: Nil
  def compileTemplate(templateFile: String): Any = {
    val data = Data("TITLE", "LAYOUT")
    val nameUses = parseMustache(templateFile)
    nameUses.map(n=>q"data.$n")
    // val q"$expr(...$aexprssnel)" = data
  }
}

object Main extends App {
  println(Templating.compileTemplate("template.md"))
}
