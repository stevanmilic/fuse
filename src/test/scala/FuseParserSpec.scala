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
      parse("type i32") ==> FPrimitiveTypeDef(FIdentifier("i32"))
    }
    test("parse sum type") {
      parse("type bool:\n    true\n    false") ==> FVariantTypeDef(
        FIdentifier("bool"),
        None,
        Seq(
          FVariantTypeValue(FIdentifier("true")),
          FVariantTypeValue(FIdentifier("false"))
        )
      )
    }
    test("parse struct type") {
      parse("type Point:\n    x: i32\n    y: i32") ==> FRecordTypeDef(
        FIdentifier("Point"),
        None,
        Seq(
          FRecordTypeField(
            FParam(FIdentifier("x"), FSimpleType(FIdentifier("i32")))
          ),
          FRecordTypeField(
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
      parse("type Option[T]:\n    None\n    Some(T)") ==> FVariantTypeDef(
        FIdentifier("Option"),
        Some(Seq(FTypeParam(FIdentifier("T")))),
        Seq(
          FVariantTypeValue(FIdentifier("None")),
          FVariantTypeValue(
            FIdentifier("Some"),
            Some(Right(Seq(FSimpleType(FIdentifier("T")))))
          )
        )
      )
    }
    test("parse generic type with two params") {
      parse("type Map[K, V]:\n    key: K\n    value: V") ==> FRecordTypeDef(
        FIdentifier("Map"),
        Some(Seq(FTypeParam(FIdentifier("K")), FTypeParam(FIdentifier("V")))),
        Seq(
          FRecordTypeField(
            FParam(FIdentifier("key"), FSimpleType(FIdentifier("K")))
          ),
          FRecordTypeField(
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
      ) ==> FVariantTypeDef(
        FIdentifier("List"),
        Some(Seq(FTypeParam(FIdentifier("A")))),
        Seq(
          FVariantTypeValue(
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
          FVariantTypeValue(FIdentifier("Nil"))
        )
      )
    }
    test("parse type alias") {
      parse("type Ints = List[i32]") ==> FTypeAlias(
        FIdentifier("Ints"),
        None,
        FSimpleType(
          FIdentifier("List"),
          Some(Seq(FSimpleType(FIdentifier("i32"))))
        )
      )
    }
    test("parse type alias for function0 type") {
      parse("type Function0[T] = () -> T") ==> FTypeAlias(
        FIdentifier("Function0"),
        Some(Seq(FTypeParam(FIdentifier("T")))),
        FFuncType(Seq(), FSimpleType(FIdentifier("T")))
      )
    }
    test("parse type alias for function1 type") {
      parse("type Function1[A, B] = A -> B") ==> FTypeAlias(
        FIdentifier("Function1"),
        Some(Seq(FTypeParam(FIdentifier("A")), FTypeParam(FIdentifier("B")))),
        FFuncType(
          Seq(FSimpleType(FIdentifier("A"))),
          FSimpleType(FIdentifier("B"))
        )
      )
    }
    test("parse type alias for function2 type") {
      parse("type Function2[A, B, C] = (A, B) -> C") ==> FTypeAlias(
        FIdentifier("Function2"),
        Some(
          Seq(
            FTypeParam(FIdentifier("A")),
            FTypeParam(FIdentifier("B")),
            FTypeParam(FIdentifier("C"))
          )
        ),
        FFuncType(
          Seq(FSimpleType(FIdentifier("A")), FSimpleType(FIdentifier("B"))),
          FSimpleType(FIdentifier("C"))
        )
      )
    }
    test("parse type alias for curried function") {
      parse("type CurriedInt = Int -> Int -> Int") ==> FTypeAlias(
        FIdentifier("CurriedInt"),
        None,
        FFuncType(
          Seq(FSimpleType(FIdentifier("Int"))),
          FFuncType(
            Seq(FSimpleType(FIdentifier("Int"))),
            FSimpleType(FIdentifier("Int"))
          )
        )
      )
    }
    test("parse type alias for tuple of ints") {
      parse("type IntTuple = (Int, Int)") ==> FTypeAlias(
        FIdentifier("IntTuple"),
        None,
        FTupleType(
          Seq(FSimpleType(FIdentifier("Int")), FSimpleType(FIdentifier("Int")))
        )
      )
    }
    test("parse trait definition") {
      parse(
        "trait Shape:\n    def area() -> f32\n    def surface() -> f32"
      ) ==> FTraitDef(
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
      ) ==> FTraitDef(
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
                  FInt(2)
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
      ) ==> FTraitDef(
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
        Seq(FMultiplication(FInt(1), FInt(2)))
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
            FExprIdentifier("x"),
            FExprIdentifier("y")
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
            FExprIdentifier("sum"),
            Some(Seq(FInt(3), FInt(2)))
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
            Some(Seq((FInt(0)))),
            Seq(
              Left(Some(Seq(FExprIdentifier("sum"))))
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
                  Seq(FSimpleType(FIdentifier("A"))),
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
                    FExprIdentifier("l"),
                    Seq(
                      FCase(
                        Seq(FIdentifierPattern("Nil")),
                        None,
                        Seq(FExprIdentifier("acc"))
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
                            FExprIdentifier("iter"),
                            Some(
                              Seq(
                                FCallExpr(
                                  FExprIdentifier("Cons"),
                                  Some(
                                    Seq(
                                      FCallExpr(
                                        FExprIdentifier("f"),
                                        Some(
                                          Seq(FExprIdentifier("h"))
                                        )
                                      ),
                                      FExprIdentifier("acc")
                                    )
                                  )
                                ),
                                FExprIdentifier("t")
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
            FExprIdentifier("iter"),
            Some(
              Seq(
                FCallExpr(
                  FExprIdentifier("List"),
                  None
                ),
                FExprIdentifier("l")
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
                FFloat(3.14.toFloat)
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
