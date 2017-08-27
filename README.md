# Levee
[![Build Status](https://travis-ci.org/vlthr/levee.svg?branch=master)](https://travis-ci.org/vlthr/levee)

Levee is a rendering engine for the [Liquid](https://shopify.github.io/liquid/) templating language with a focus on providing type safety and strict error detection wherever possible.

## Using Levee
Currently, levee is available for dotty 0.2.0-RC1 and is in the process of being ported to Scala2. The library is still in its early stages and has some rough edges, but user feedback is greatly appreciated.

Add the following dependency to your `build.sbt`:
```scala
resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

libraryDependencies += "com.vlthr" %% "levee" % "0.1.0"
```

Then use as follows:
```scala
import com.vlthr.levee._

val render = Levee.newContext()
  .withConfig(includeDir = "./_include", strictConditionals = false)
  .withFilter(myCustomFilter)
  .withParams(Map("author" -> Map("name" -> "vlthr"), "title" -> "Levee README"))
  .renderFile("./index.md")

render match {
    case Success(renderedString) => println(renderedString)
    case Failure(errors) => println(errors)
}
```

Use of undefined variables will cause an error everywhere except in the conditional part of an `if` tag, e.g.
```liquid
{{ noexists }} <- Throws an error
{% if noexists %} <- Does not throw an error
{% endif %}
{% if page.noexists %}  <- Does not throw an error
{% endif %}
```
This allows you to check for the existence of a variable without causing errors. To disable this feature, enable the `strictConditionals` configuration option.

## Configuration
Levee can be configured using the `.withConfig` builder method on the LeveeContext object.

```scala
Levee.newContext()
  .withConfig(config1 = value, config2 = value)
```

The configuration options available are:

Name              | Description
------------------|-------------------------
includeDir        | The path to the directory where included snippets can be found
strictConditionals| Disallow checking for undefined variables in conditionals


## Writing custom filters
Levee allows user code to define type safe filters without needing to manually implement type checking for all of its inputs.

To define a filter that takes only one input type:
```scala
import shapeless.{ HNil, :: }
import com.vlthr.levee.filters._
import com.vlthr.levee.core._

// For the liquid filter {{ "World" | prepend: "Hello " }}
val prepend = Filter[StringValue, StringValue :: HNil, HNil]("prepend") {
  (ctx, filter, input, args, optArgs) =>
    val start = args.head.get
    success(StringValue(start + input.get))
}
```

Filter takes three type parameters:
- The type of the input (in the prepend example `"World"`, which is a `StringValue`)
- The types of the arguments, in the form of a shapeless HList (e.g. `FirstArgType :: SecondArgType :: ThirdArgType :: HNil`). If there are no arguments, use HNil to represent an empty argument list.
- The types of any optional positional parameters that may appear after the main argument list, also in the form of a shapeless HList. (e.g `Option[FirstOptArgType] :: Option[SecondOptArgType] :: HNil]`)

To extract values from the argument list, use a combination of `args.head` and `args.tail`. Then, to extract the native type from within the `Value`, use the `get` method present on each of the `Value` subtypes.

The filter must return either a successful result (`success(value)`) or a failure (`failure(errorFragment)`).

Some filters may require inputs of varying types. To do this, use the `Filter.multi` factory method:
```scala
import shapeless.{ HNil, CNil, Inl, Inr, ::, :+: }
import com.vlthr.levee.filters._
import com.vlthr.levee.core._

val reverse = Filter.multi[ListValue :+: StringValue :+: CNil, HNil, HNil]("reverse") {
  (ctx, filter, input, args, optArgs) =>
  input match {
    // Inl(_) represents the head of the type list, i.e. ListValue
    case Inl(list) => success(ListValue(list.get.reverse))
    // Inr(Inl(_)) means first taking the tail of the list,
    // then taking the head, i.e. StringValue, the second type
    case Inr(Inl(string)) => success(StringValue(string.get.reverse))
    // To prevent a compiler warning about non-exhaustive pattern matching,
    // Add an (unreachable) final case.
    case Inr(Inr(_)) => abort()
  }
}
```
`input` is then a shapeless Coproduct and can either be mapped on using a `Poly` function, or pattern matched as shown above.

The possible types of values are:
- `StringValue(v: String)`
- `BooleanValue(v: Boolean)`
- `IntValue(v: Int)`
- `ListValue(v: List[Value])`
- `MapValue(v: Map[String, Value])`
