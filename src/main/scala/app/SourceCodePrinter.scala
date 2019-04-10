package app

import scala.meta._

object SourceCodePrinter {
  private def prePrintNode[T <: Tree](node: T, explanation: Option[() => Unit] = None): Unit = {
    print(s"[${node.pos.startLine + 1}:${node.pos.startColumn + 1}] ${node.productPrefix}: $node")
    if (node.toString == "") {
      print("(Empty node)")
    }

    if (explanation.isDefined) {
      println
      explanation.get()
    }
    println("\n")
  }

  def printNode[T <: Tree](node: T): Unit = {
    node match {
      case _: Source => {}
      case node: Ctor.Primary => prePrintNode(node, Some(() => {
        print("This is a primary constructor of a class. It specified in the class definition. "
        + "Arguments of the constructor are automatically declared as public readonly fields. "
        + "You can change access modifier or readonly mode by specifying corresponding keywords right here!")
      }))
      case node: Decl.Def => prePrintNode(node, Some(() => {
        print("This is a method declaration. It doesn't have implementation yet. ")
        print(s"Return type of this method is [${node.decltpe}]. ")
        print(
          if (node.paramss.isEmpty || node.paramss.length == 1 && node.paramss(0).isEmpty) {
            "It doesn't have any parameters"
          } else if (node.paramss.length == 1) {
            "Parameters will be described below"
          } else {
            (s"It has ${node.paramss.length} parameter groups. This means, that call of this function will return "
              + "another function, which will have one parameter group less. Every parameter group is enclosed in parenthesis. "
              + "This technique is called currying. Parameters will be described below")
          }
        )
      }))
      case node: Defn.Class => {
        if (node.mods.isEmpty && node.templ.inits.isEmpty) {
          prePrintNode(node)
        } else {
          prePrintNode(node, Some(() => {
            for (mod <- node.mods) {
              mod match {
                case _: Mod.Case =>
                  print("This is a case class. Instances of case classes are creating without the 'new' keyword. "
                  + "Also, objects of case classes deeply compared by value, "
                  + " e.g.: Person('Nick', 27) == Person('Nick', 27) "
                  + "even if assigned to different variables with different addresses in memory. ")
                case _ => 
              }
            }

            if (!node.templ.inits.isEmpty) {
              print(s"This class is derived from ${node.templ.inits.head}. ")
              if (node.templ.inits.length > 1) {
                print(s"It also extends functionality of ${node.templ.inits.length} traits. If traits would have a method with same name," 
                  + "the method would be taken from right most trait. ")

                print("It extends traits: " + node.templ.inits.tail)
              }
            }
          }))
        }
      }
      case node: Defn.Def => prePrintNode(node, Some(() => {
        print("This is a method. ")
        print(
          node.decltpe match {
            case Some(tpe) => s"Return type of this method is [$tpe]. "
            case None => ("Type is inferred from body of method by the type of returned value."
              + " Returned value is the result of the last expression of the body"
              + " or something, that explicitly marked by 'return' keyword. ")
          }
        )
        print(
          if (node.paramss.isEmpty || node.paramss.length == 1 && node.paramss(0).isEmpty) {
            "It doesn't have any parameters"
          } else if (node.paramss.length == 1) {
            if (node.paramss.head.length == 1 && node.paramss.head.head.decltpe.toString == "Some(Array[String])") {
              "This may be an entrypoint to the program as JVM requires such methods to be named 'main' " + 
              "and accept an array of strings as their singular argument"
            } else {
              "Parameters will be described below"
            }
          } else {
            (s"It has ${node.paramss.length} parameter groups. This means, that call of this function will return "
              + "another function, which will have one parameter group less. Every parameter group is enclosed in parenthesis. "
              + "This technique is called currying. Parameters will be described below")
          }
        )
      }))
      case node: Defn.Object => prePrintNode(node, Some(() => {
        print("This is an object. It defines a class and creates singular instance of it. ")
        if (!node.templ.inits.isEmpty) {
              print(s"This object is derived from ${node.templ.inits.head}. ")
              if (node.templ.inits.head.toString == "App") {
                print("This is the shortest way to create a runnable App. Trait App defines an antrypoint, " + 
                  "so you can start typing your code right inside of your class!")
              }
              if (node.templ.inits.length > 1) {
                print(s"It also extends functionality of ${node.templ.inits.length} traits. If traits would have a method with same name," 
                  + "the method would be taken from right most trait. ")

                print("It extends traits: " + node.templ.inits.tail)
              }
            }
      }))
      case node: Defn.Trait => prePrintNode(node, Some(() => {
        print("This is a trait. Trait are both like interfaces and abstract classes in other programming languages. " 
          + "It extends functionality of a class by adding methods declared in this trait. "
          + "One can solve rhomboid inheritance problem using traits, because it's possible to extend multiple traits at once. ")
      }))
      case node: Defn.Val => prePrintNode(node, Some(() => {
        print("This is a VALue definition similar to constants in other languages. Thus, vals cannot be reassigned. ")
        print(
          node.decltpe match {
            case Some(tpe) => s"Type [$tpe] is explicitly specified"
            case None => s"Type is inferred from right hand side expression (${node.rhs})"
          }
        )
      }))
      case node: Defn.Var => prePrintNode(node, Some(() => {
        print("This is a variable definition. Unlike vals, variables are mutable. ")
        print(
          node.decltpe match {
            case Some(tpe) => s"Type [$tpe] is explicitly specified"
            case None => s"Type is inferred from right hand side expression (${node.rhs})"
          }
        )
      }))
      case node: Term.Block => prePrintNode(node, Some(() => {
        print("This is a block. Block returns a result of last expression in it, so typically you don't have to type 'return'"
         + " keyword in functions")
      }))
      case node: Term.Function => prePrintNode(node, Some(() => {
        print("This is anonymous function. Parameters (if exist) will be printed below")
      }))
      case node: Term.Interpolate => prePrintNode(node, Some(() => {
        print("This is an interolation string. It allows to put inside of it variables prepended by '$' to evaluate it's contents. " 
          + "If you want to interpolate an expression, you'll have to enclose it in curly brackets after dollar sign '${...}'")
      }))
      case node: Term.Param => prePrintNode(node, Some(() => {
        print(s"Parameter with name ${node.name}. ")
        print(
          node.decltpe match {
            case Some(tpe) => s"It has type [$tpe]"
            case None => s"It accepts parameters of any type"
          }
        )
      }))
      case node: Type.Name if node.value == "Unit" => prePrintNode(node, Some(() => {
        print("Type [Unit] is analogue to type [void] in other programming languages")
      }))
      case node => prePrintNode(node)
    }
  }
}