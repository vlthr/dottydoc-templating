package com.vlthr.levee.typetraits
import com.vlthr.levee.core.Errors._
import validation.Result
import com.vlthr.levee.core._

package object TypeTraits {
  abstract trait ArgSpec { self: Extension with OptArgSpec =>
    def checkArgs(args: List[Value])(implicit ctx: Context): List[ErrorFragment]
  }

  abstract trait OptArgSpec { self: Extension =>
    def checkOptArgs(optArgs: List[Value])(implicit ctx: Context): List[ErrorFragment]
  }

  abstract trait KwArgSpec { self: Extension =>
    def checkKwArgs(kwargs: Map[String, Value])(implicit ctx: Context): List[ErrorFragment]
  }

  abstract trait InputSpec { self: Extension =>
    def checkInput(input: Value)(implicit ctx: Context): List[ErrorFragment]
  }

  abstract trait NoArgs { self: Extension =>
    def checkArgs(args: List[Value])(implicit ctx: Context): List[ErrorFragment] = {
      val correctNumberOfArgs = args.size == 0
      if (!correctNumberOfArgs) List(InvalidArgs(this, args))
      else Nil
    }
  }

  abstract trait InputType(t: ValueType) extends InputSpec { self: Extension =>
    override def checkInput(input: Value)(implicit ctx: Context): List[ErrorFragment] = {
      if (!t.matches(input)) InvalidInput(this, input) :: Nil
      else Nil
    }
  }

  abstract trait FixedOptArgs(types: ValueType*) extends OptArgSpec { self: Extension =>
    def checkOptArgs(optArgs: List[Value])(implicit ctx: Context): List[ErrorFragment] = {
      val correctArgTypes = optArgs.zip(types).forall{ case (v, expected) => expected.matches(v) }
      val correctNumberOfArgs = optArgs.size == types.size
      if (!correctArgTypes || !correctNumberOfArgs) List(InvalidArgs(this, optArgs))
      else Nil
    }
  }

  abstract trait NoOptArgs extends OptArgSpec { self: Extension =>
    def checkOptArgs(optArgs: List[Value])(implicit ctx: Context): List[ErrorFragment] = {
      val correctNumberOfArgs = optArgs.size == 0
      if (!correctNumberOfArgs) List(TooManyArgs(this, optArgs))
      else Nil
    }
  }

  abstract trait FixedArgs(types: ValueType*) extends ArgSpec { self: Extension with OptArgSpec =>
    // TODO: A macro could generate typesafe getters for each of the expected arguments.
    def checkArgs(allArgs: List[Value])(implicit ctx: Context): List[ErrorFragment] = {
      var errors: List[ErrorFragment] = Nil
      // Prune off optional arguments if applicable
      val args = if (allArgs.size > numArgs) {
        val (args, optArgs) = allArgs.splitAt(numArgs)
        errors = errors ++ checkOptArgs(optArgs)
        args
      } else allArgs
      val correctArgTypes = args.zip(types).forall{ case (v, expected) => expected.matches(v) }
      val correctNumberOfArgs = args.size == types.size
      if (!correctArgTypes || !correctNumberOfArgs) List(InvalidArgs(this, args))
      else Nil
    }
    def numArgs: Int = types.size
  }

  abstract trait OptKwArgs(types: (String, ValueType)*) extends KwArgSpec { self: Extension =>
    def checkKwArgs(kwargs: Map[String, Value])(implicit ctx: Context): List[ErrorFragment] = {
      val expectedMap: Map[String, ValueType] = Map(types: _*)
      val errors = kwargs.map { case (key, value) =>
        if (!expectedMap.contains(key)) Some(InvalidKwArg(this, key))
        else if (value.valueType != expectedMap(key)) Some(InvalidKwArgType(this, key, value, expectedMap(key)))
        else None
      }.flatten
      if (errors.size == 0) Nil
      else errors.toList
    }
  }

  abstract trait NoKwArgs extends KwArgSpec { self: Extension =>
    def checkKwArgs(kwargs: Map[String, Value])(implicit ctx: Context): List[ErrorFragment] = Nil
  }
}
