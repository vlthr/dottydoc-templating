package mustachemeta

import scala.meta._
import scala.annotation.StaticAnnotation

// class Mustache(templatePath: String) extends StaticAnnotation {
//   inline def apply(annottees: Any): Any = meta {
//     annottees match {
//       case q"object $name extends { ..$earlyInits } with ..$parentCtors { $param => ..$body }" =>
//         annottees
//       case _ => abort("")
//     }
//   }
// }
