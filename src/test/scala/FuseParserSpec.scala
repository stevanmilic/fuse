package fuse

import scala.util.{Failure, Success}
import utest._
import org.parboiled2._

object FuseParserSpec extends TestSuite {
  import FuseParser._
  import FuseLexicalParser._
  import FuseTypesParser._
  import FuseExpressionParser._

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
          FStructTypeField(
            FParam(FIdentifier("x"), FSimpleType(FIdentifier("i32")))
          ),
          FStructTypeField(
            FParam(FIdentifier("y"), FSimpleType(FIdentifier("i32")))
          )
        )
      )
    }
    test("parse tuple type") {
      parse("type Pair(i32, str)") ==> FTupleTypeDef(
        FIdentifier("Pair"),
        None,
        Seq(FSimpleType(FIdentifier("i32")), FSimpleType(FIdentifier("str")))
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
            Some(Right(Seq(FSimpleType(FIdentifier("T")))))
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
            FParam(FIdentifier("key"), FSimpleType(FIdentifier("K")))
          ),
          FStructTypeField(
            FParam(FIdentifier("value"), FSimpleType(FIdentifier("V")))
          )
        )
      )
    }
    test("parse nested type") {
      parse("type Data[T](Option[Map[string, T]])") ==> FTupleTypeDef(
        FIdentifier("Data"),
        Some(Seq(FTypeParam(FIdentifier("T")))),
        Seq(
          FSimpleType(
            FIdentifier("Option"),
            Some(
              Seq(
                FSimpleType(
                  FIdentifier("Map"),
                  Some(
                    Seq(
                      FSimpleType(FIdentifier("string")),
                      FSimpleType(FIdentifier("T"))
                    )
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
                  FParam(FIdentifier("head"), FSimpleType(FIdentifier("A"))),
                  FParam(
                    FIdentifier("t"),
                    FSimpleType(
                      FIdentifier("List"),
                      Some(Seq(FSimpleType(FIdentifier("A"))))
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
    test("parse type alias") {
      parse("type Ints = List[i32]") ==> FTypeAlias(
        FIdentifier("Ints"),
        FSimpleType(
          FIdentifier("List"),
          Some(Seq(FSimpleType(FIdentifier("i32"))))
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
          Right(
            FFuncSig(
              false,
              FIdentifier("area"),
              None,
              None,
              FSimpleType(FIdentifier("f32"), None)
            )
          ),
          Right(
            FFuncSig(
              false,
              FIdentifier("surface"),
              None,
              None,
              FSimpleType(FIdentifier("f32"), None)
            )
          )
        )
      )
    }
    test("parse trait defination with function definition") {
      parse(
        "trait Shape:\n def area() -> f32\n def double_area() -> f32:\n  this.area*2"
      ) ==> FTrait(
        FIdentifier("Shape"),
        None,
        Seq(
          Right(
            FFuncSig(
              false,
              FIdentifier("area"),
              None,
              None,
              FSimpleType(FIdentifier("f32"), None)
            )
          ),
          Left(
            FFuncDef(
              FFuncSig(
                false,
                FIdentifier("double_area"),
                None,
                None,
                FSimpleType(FIdentifier("f32"), None)
              ),
              Seq(
                FMultiplication(
                  FMemberExpr(
                    FExprIdentifier("this"),
                    Seq(FExprIdentifier("area"))
                  ),
                  FMemberExpr(FInt(2))
                )
              )
            )
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
            FTypeParam(
              FIdentifier("V"),
              Some(FSimpleType(FIdentifier("Self")))
            ),
            FTypeParam(FIdentifier("O"), Some(FSimpleType(FIdentifier("Self"))))
          )
        ),
        Seq(
          Right(
            FFuncSig(
              false,
              FIdentifier("add"),
              None,
              Some(
                Seq(
                  FParam(
                    FIdentifier("other"),
                    FSimpleType(FIdentifier("V"), None)
                  )
                )
              ),
              FSimpleType(FIdentifier("O"), None)
            )
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
          FSimpleType(FIdentifier("i32"), None)
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
                FSimpleType(FIdentifier("i32"))
              ),
              FParam(
                FIdentifier("y"),
                FSimpleType(FIdentifier("i32"))
              )
            )
          ),
          FSimpleType(FIdentifier("i32"), None)
        ),
        Seq(
          FAddition(
            FMemberExpr(FExprIdentifier("x")),
            FMemberExpr(FExprIdentifier("y"))
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
          FSimpleType(FIdentifier("i32"), None)
        ),
        Seq(
          FCallExpr(
            FMemberExpr(
              FExprIdentifier("sum")
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
                FSimpleType(
                  FIdentifier("List"),
                  Some(Seq(FSimpleType(FIdentifier("i32"))))
                )
              )
            )
          ),
          FSimpleType(FIdentifier("i32"), None)
        ),
        Seq(
          FCallExpr(
            FMemberExpr(
              FExprIdentifier("l"),
              Seq(FExprIdentifier("fold_left"))
            ),
            Some(Seq(FMemberExpr((FInt(0))))),
            Seq(
              Left(Some(Seq(FMemberExpr(FExprIdentifier("sum")))))
            )
          )
        )
      )
    }
    test("parse tail func definition with generics") {
      parse(
        "def map[A, B](l: List[A], f: A -> B) -> List[B]:\n let iter = (acc, l) => {\n  match l:\n   Nil => acc\n   Cons(h, t) => iter(Cons(f(h), acc), t)\n  }\n iter(List(), l)"
      ) ==> FFuncDef(
        FFuncSig(
          false,
          FIdentifier("map"),
          Some(Seq(FTypeParam(FIdentifier("A")), FTypeParam(FIdentifier("B")))),
          Some(
            Seq(
              FParam(
                FIdentifier("l"),
                FSimpleType(
                  FIdentifier("List"),
                  Some(Seq(FSimpleType(FIdentifier("A"))))
                )
              ),
              FParam(
                FIdentifier("f"),
                FFuncType(
                  FSimpleType(FIdentifier("A")),
                  FSimpleType(FIdentifier("B"))
                )
              )
            )
          ),
          FSimpleType(
            FIdentifier("List"),
            Some(Seq(FSimpleType(FIdentifier("B"))))
          )
        ),
        Seq(
          FLetExpr(
            FIdentifier("iter"),
            None,
            Seq(
              FLambdaExpr(
                Seq(
                  FLambdaBinding(FIdentifier("acc")),
                  FLambdaBinding(FIdentifier("l"))
                ),
                Seq(
                  FMatchExpr(
                    FMemberExpr(FExprIdentifier("l")),
                    Seq(
                      FCase(
                        Seq(FIdentifierPattern("Nil")),
                        None,
                        Seq(FMemberExpr(FExprIdentifier("acc")))
                      ),
                      FCase(
                        Seq(
                          FSumStructPattern(
                            FIdentifier("Cons"),
                            Seq(
                              FIdentifierPattern("h"),
                              FIdentifierPattern("t")
                            )
                          )
                        ),
                        None,
                        Seq(
                          FCallExpr(
                            FMemberExpr(FExprIdentifier("iter")),
                            Some(
                              Seq(
                                FCallExpr(
                                  FMemberExpr(FExprIdentifier("Cons")),
                                  Some(
                                    Seq(
                                      FCallExpr(
                                        FMemberExpr(FExprIdentifier("f")),
                                        Some(
                                          Seq(FMemberExpr(FExprIdentifier("h")))
                                        )
                                      ),
                                      FMemberExpr(FExprIdentifier("acc"))
                                    )
                                  )
                                ),
                                FMemberExpr(FExprIdentifier("t"))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          FCallExpr(
            FMemberExpr(FExprIdentifier("iter")),
            Some(
              Seq(
                FCallExpr(
                  FMemberExpr(FExprIdentifier("List")),
                  None
                ),
                FMemberExpr(FExprIdentifier("l"))
              )
            )
          )
        )
      )
    }
    test("parse type implemantion") {
      parse(
        "impl Point:\n def x_diff(other: Point) -> f32:\n  math.abs(this.x - other.x)"
      ) ==> FTypeImpl(
        FIdentifier("Point"),
        None,
        Seq(
          FFuncDef(
            FFuncSig(
              false,
              FIdentifier("x_diff"),
              None,
              Some(
                Seq(
                  FParam(
                    FIdentifier("other"),
                    FSimpleType(FIdentifier("Point"))
                  )
                )
              ),
              FSimpleType(FIdentifier("f32"))
            ),
            Seq(
              FCallExpr(
                FMemberExpr(
                  FExprIdentifier("math"),
                  Seq(FExprIdentifier("abs"))
                ),
                Some(
                  Seq(
                    FSubtraction(
                      FMemberExpr(
                        FExprIdentifier("this"),
                        Seq(FExprIdentifier("x"))
                      ),
                      FMemberExpr(
                        FExprIdentifier("other"),
                        Seq(FExprIdentifier("x"))
                      )
                    )
                  )
                )
              )
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
              FSimpleType(FIdentifier("f32"), None)
            ),
            Seq(
              FMultiplication(
                FMultiplication(
                  FMemberExpr(
                    FExprIdentifier("this"),
                    Seq(FExprIdentifier("radius"))
                  ),
                  FMemberExpr(
                    FExprIdentifier("this"),
                    Seq(FExprIdentifier("radius"))
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
