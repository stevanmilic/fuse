package fuse

import scala.util.{Failure, Success}
import utest._
import org.parboiled2._

object FuseParserSpec extends TestSuite {
  import FuseParser._

  val tests = Tests {
    test("parse primitive type") {
      parse("type i32") ==> FPrimitiveType(FIdentifier("i32"))
    }
    test("parse sum type") {
      parse("type bool:\n    true\n    false") ==> FSumType(
        FIdentifier("bool"),
        None,
        Seq(
          FSumTypeValue(FIdentifier("true")),
          FSumTypeValue(FIdentifier("false"))
        )
      )
    }
    test("parse struct type") {
      parse("type Point:\n    x: i32\n    y: i32") ==> FStructType(
        FIdentifier("Point"),
        None,
        Seq(
          FStructTypeField(FParam(FIdentifier("x"), FType(FIdentifier("i32")))),
          FStructTypeField(FParam(FIdentifier("y"), FType(FIdentifier("i32"))))
        )
      )
    }
    test("parse tuple type") {
      parse("type Pair(i32, str)") ==> FTupleType(
        FIdentifier("Pair"),
        None,
        Seq(FType(FIdentifier("i32")), FType(FIdentifier("str")))
      )
    }
    test("parse generic type") {
      parse("type Option[T]:\n    None\n    Some(T)") ==> FSumType(
        FIdentifier("Option"),
        Some(Seq(FTypeParam(FIdentifier("T")))),
        Seq(
          FSumTypeValue(FIdentifier("None")),
          FSumTypeValue(
            FIdentifier("Some"),
            Some(Right(Seq(FType(FIdentifier("T")))))
          )
        )
      )
    }
    test("parse generic type with two params") {
      parse("type Map[K, V]:\n    key: K\n    value: V") ==> FStructType(
        FIdentifier("Map"),
        Some(Seq(FTypeParam(FIdentifier("K")), FTypeParam(FIdentifier("V")))),
        Seq(
          FStructTypeField(
            FParam(FIdentifier("key"), FType(FIdentifier("K")))
          ),
          FStructTypeField(
            FParam(FIdentifier("value"), FType(FIdentifier("V")))
          )
        )
      )
    }
    test("parse nested type") {
      parse("type Data[T](Option[Map[string, T]])") ==> FTupleType(
        FIdentifier("Data"),
        Some(Seq(FTypeParam(FIdentifier("T")))),
        Seq(
          FType(
            FIdentifier("Option"),
            Some(
              Seq(
                FType(
                  FIdentifier("Map"),
                  Some(
                    Seq(FType(FIdentifier("string")), FType(FIdentifier("T")))
                  )
                )
              )
            )
          )
        )
      )
    }
    test("parse generic sum type with product value") {
      parse(
        "type List[A]:\n    Cons(head: A, t: List[A])\n    Nil"
      ) ==> FSumType(
        FIdentifier("List"),
        Some(Seq(FTypeParam(FIdentifier("A")))),
        Seq(
          FSumTypeValue(
            FIdentifier("Cons"),
            Some(
              Left(
                Seq(
                  FParam(FIdentifier("head"), FType(FIdentifier("A"))),
                  FParam(
                    FIdentifier("t"),
                    FType(
                      FIdentifier("List"),
                      Some(Seq(FType(FIdentifier("A"))))
                    )
                  )
                )
              )
            )
          ),
          FSumTypeValue(FIdentifier("Nil"))
        )
      )
    }
    test("parse trait definition") {
      parse(
        "trait Shape:\n    def area() -> f32\n    def surface() -> f32"
      ) ==> FTrait(
        FIdentifier("Shape"),
        None,
        Seq(
          FFuncSig(
            false,
            FIdentifier("area"),
            None,
            None,
            FType(FIdentifier("f32"), None)
          ),
          FFuncSig(
            false,
            FIdentifier("surface"),
            None,
            None,
            FType(FIdentifier("f32"), None)
          )
        )
      )
    }
    test("parse trait with default generic types") {
      parse(
        "trait Add[V = Self, O = Self]:\n    def add(other: V) -> O"
      ) ==> FTrait(
        FIdentifier("Add"),
        Some(
          Seq(
            FTypeParam(FIdentifier("V"), Some(FType(FIdentifier("Self")))),
            FTypeParam(FIdentifier("O"), Some(FType(FIdentifier("Self"))))
          )
        ),
        Seq(
          FFuncSig(
            false,
            FIdentifier("add"),
            None,
            Some(
              Seq(
                FParam(
                  FIdentifier("other"),
                  FType(FIdentifier("V"), None)
                )
              )
            ),
            FType(FIdentifier("O"), None)
          )
        )
      )
    }
    test("parse function definition with 1 * 2") {
      parse("def one_and_two() -> i32:\n    1 * 2") ==> FFuncDef(
        FFuncSig(
          false,
          FIdentifier("one_and_two"),
          None,
          None,
          FType(FIdentifier("i32"), None)
        ),
        Seq(FMultiplication(FMemberExpr(FInt(1)), FMemberExpr(FInt(2))))
      )
    }
    test("parse function definition with additive expression") {
      parse("def sum(x: i32, y: i32) -> i32:\n    x + y") ==> FFuncDef(
        FFuncSig(
          false,
          FIdentifier("sum"),
          None,
          Some(
            Seq(
              FParam(
                FIdentifier("x"),
                FType(FIdentifier("i32"))
              ),
              FParam(
                FIdentifier("y"),
                FType(FIdentifier("i32"))
              )
            )
          ),
          FType(FIdentifier("i32"), None)
        ),
        Seq(
          FAddition(
            FMemberExpr(FExprIdentifier(FIdentifier("x"))),
            FMemberExpr(FExprIdentifier(FIdentifier("y")))
          )
        )
      )
    }
    test("parse function definition with simple call expressions") {
      parse(
        "def sum_three_and_two() -> i32:\n sum(3, 2)"
      ) ==> FFuncDef(
        FFuncSig(
          false,
          FIdentifier("sum_three_and_two"),
          None,
          None,
          FType(FIdentifier("i32"), None)
        ),
        Seq(
          FCallExpr(
            FMemberExpr(
              FExprIdentifier(FIdentifier("sum"))
            ),
            Some(Seq(FMemberExpr(FInt(3)), FMemberExpr(FInt(2))))
          )
        )
      )
    }
    test("parse function definition with call expressions") {
      parse(
        "def sum_list(l: List[i32]) -> i32:\n l.fold_left(0)(sum)"
      ) ==> FFuncDef(
        FFuncSig(
          false,
          FIdentifier("sum_list"),
          None,
          Some(
            Seq(
              FParam(
                FIdentifier("l"),
                FType(FIdentifier("List"), Some(Seq(FType(FIdentifier("i32")))))
              )
            )
          ),
          FType(FIdentifier("i32"), None)
        ),
        Seq(
          FCallExpr(
            FMemberExpr(
              FExprIdentifier(FIdentifier("l")),
              Seq(FIdentifier("fold_left"))
            ),
            Some(Seq(FMemberExpr((FInt(0))))),
            Seq(
              Left(Some(Seq(FMemberExpr(FExprIdentifier(FIdentifier("sum"))))))
            )
          )
        )
      )
    }
    test("parse trait implementation") {
      parse(
        "impl Shape for Circle:\n    def area() -> f32:\n        this.radius * this.radius * 3.14"
      ) ==> FTraitImpl(
        FIdentifier("Shape"),
        None,
        FIdentifier("Circle"),
        None,
        Seq(
          FFuncDef(
            FFuncSig(
              false,
              FIdentifier("area"),
              None,
              None,
              FType(FIdentifier("f32"), None)
            ),
            Seq(
              FMultiplication(
                FMultiplication(
                  FMemberExpr(
                    FExprIdentifier(FIdentifier("this")),
                    Seq(FIdentifier("radius"))
                  ),
                  FMemberExpr(
                    FExprIdentifier(FIdentifier("this")),
                    Seq(FIdentifier("radius"))
                  )
                ),
                FMemberExpr(FFloat(3.14.toFloat))
              )
            )
          )
        )
      )

    }
    test("fail parsing on bad indentation") {
      assertMatch(runParser("type bool:\n  true\n    false")) {
        case Failure(ParseError(Position(27, 3, 10), _, _)) =>
      }
    }
    test(
      "fail parsing on bad nested indentation having lower number of whitespaces"
    ) {
      assertMatch(
        runParser(
          "impl Shape for Circle:\n    def area() -> f32:\n this.radius * this.radius * 3.14"
        )
      ) { case Failure(ParseError(Position(79, 3, 34), _, _)) =>
      }
    }
  }

  def parse(s: String): FNode = {
    val parser = new FuseParser(s)
    parser.InputLine.run() match {
      case Success(result) => result
      case Failure(e: ParseError) =>
        sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }

  def runParser(s: String) = {
    val parser = new FuseParser(s)
    parser.InputLine.run()
  }
}
