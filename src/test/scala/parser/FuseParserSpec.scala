package parser

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
      parse("type i32") ==> Seq(FPrimitiveTypeDecl(FIdentifier("i32")))
    }
    test("parse variant type") {
      parse("type bool:\n    true\n    false") ==> Seq(
        FVariantTypeDecl(
          FIdentifier("bool"),
          None,
          Seq(
            FVariantTypeValue(FIdentifier("true")),
            FVariantTypeValue(FIdentifier("false"))
          )
        )
      )
    }
    test("parse record type") {
      parse("type Point:\n    x: i32\n    y: i32") ==> Seq(
        FRecordTypeDecl(
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
      )
    }
    test("parse tuple type") {
      parse("type Pair(i32, str)") ==> Seq(
        FTupleTypeDecl(
          FIdentifier("Pair"),
          None,
          Seq(FSimpleType(FIdentifier("i32")), FSimpleType(FIdentifier("str")))
        )
      )
    }
    test("parse generic type") {
      parse("type Option[T]:\n    None\n    Some(T)") ==> Seq(
        FVariantTypeDecl(
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
      )
    }
    test("parse generic type with two params") {
      parse("type Map[K, V]:\n    key: K\n    value: V") ==> Seq(
        FRecordTypeDecl(
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
      )
    }
    test("parse nested type") {
      parse("type Data[T](Option[Map[string, T]])") ==> Seq(
        FTupleTypeDecl(
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
      )
    }
    test("parse generic sum type with product value") {
      parse(
        "type List[A]:\n    Cons(head: A, t: List[A])\n    Nil"
      ) ==> Seq(
        FVariantTypeDecl(
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
      )
    }
    test("parse type alias") {
      parse("type Ints = List[i32]") ==> Seq(
        FTypeAlias(
          FIdentifier("Ints"),
          None,
          FSimpleType(
            FIdentifier("List"),
            Some(Seq(FSimpleType(FIdentifier("i32"))))
          )
        )
      )
    }
    test("parse type alias for function0 type") {
      parse("type Function0[T] = () -> T") ==> Seq(
        FTypeAlias(
          FIdentifier("Function0"),
          Some(Seq(FTypeParam(FIdentifier("T")))),
          FFuncType(Seq(), FSimpleType(FIdentifier("T")))
        )
      )
    }
    test("parse type alias for function1 type") {
      parse("type Function1[A, B] = A -> B") ==> Seq(
        FTypeAlias(
          FIdentifier("Function1"),
          Some(Seq(FTypeParam(FIdentifier("A")), FTypeParam(FIdentifier("B")))),
          FFuncType(
            Seq(FSimpleType(FIdentifier("A"))),
            FSimpleType(FIdentifier("B"))
          )
        )
      )
    }
    test("parse type alias for function2 type") {
      parse("type Function2[A, B, C] = (A, B) -> C") ==> Seq(
        FTypeAlias(
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
      )
    }
    test("parse type alias for curried function") {
      parse("type CurriedInt = Int -> Int -> Int") ==> Seq(
        FTypeAlias(
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
      )
    }
    test("parse type alias for tuple of ints") {
      parse("type IntTuple = (Int, Int)") ==> Seq(
        FTypeAlias(
          FIdentifier("IntTuple"),
          None,
          FTupleType(
            Seq(
              FSimpleType(FIdentifier("Int")),
              FSimpleType(FIdentifier("Int"))
            )
          )
        )
      )
    }
    test("parse trait definition") {
      parse(
        "trait Shape:\n    fun area() -> f32;\n    fun surface() -> f32;"
      ) ==> Seq(
        FTraitDecl(
          FIdentifier("Shape"),
          None,
          Seq(
            Right(
              FFuncSig(
                FIdentifier("area"),
                None,
                None,
                FSimpleType(FIdentifier("f32"), None)
              )
            ),
            Right(
              FFuncSig(
                FIdentifier("surface"),
                None,
                None,
                FSimpleType(FIdentifier("f32"), None)
              )
            )
          )
        )
      )
    }
    test("parse trait definition with function definition") {
      parse(
        "trait Shape:\n fun area() -> f32;\n fun double_area() -> f32\n  this.area*2"
      ) ==> Seq(
        FTraitDecl(
          FIdentifier("Shape"),
          None,
          Seq(
            Right(
              FFuncSig(
                FIdentifier("area"),
                None,
                None,
                FSimpleType(FIdentifier("f32"), None)
              )
            ),
            Left(
              FFuncDecl(
                FFuncSig(
                  FIdentifier("double_area"),
                  None,
                  None,
                  FSimpleType(FIdentifier("f32"), None)
                ),
                Seq(
                  FMultiplication(
                    FProj(
                      FVar("this"),
                      Seq(FVar("area"))
                    ),
                    FInt(2)
                  )
                )
              )
            )
          )
        )
      )
    }
    test("parse trait with default generic types") {
      parse(
        "trait Add[V = Self, O = Self]:\n    fun add(other: V) -> O;"
      ) ==> Seq(
        FTraitDecl(
          FIdentifier("Add"),
          Some(
            Seq(
              FTypeParam(
                FIdentifier("V"),
                Some(FSimpleType(FIdentifier("Self")))
              ),
              FTypeParam(
                FIdentifier("O"),
                Some(FSimpleType(FIdentifier("Self")))
              )
            )
          ),
          Seq(
            Right(
              FFuncSig(
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
      )
    }
    test("parse function definition with 1 * 2") {
      parse("fun one_and_two() -> i32\n    1 * 2") ==> Seq(
        FFuncDecl(
          FFuncSig(
            FIdentifier("one_and_two"),
            None,
            None,
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(FMultiplication(FInt(1), FInt(2)))
        )
      )
    }
    test("parse function definition with additive expression") {
      parse("fun sum(x: i32, y: i32) -> i32\n    x + y") ==> Seq(
        FFuncDecl(
          FFuncSig(
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
              FVar("x"),
              FVar("y")
            )
          )
        )
      )
    }
    test("parse function definition with simple call expressions") {
      parse(
        "fun sum_three_and_two() -> i32\n sum(3, 2)"
      ) ==> Seq(
        FFuncDecl(
          FFuncSig(
            FIdentifier("sum_three_and_two"),
            None,
            None,
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(
            FApp(
              FVar("sum"),
              None,
              Seq(Some(Seq(FInt(3), FInt(2))))
            )
          )
        )
      )
    }
    test("parse function definition with call expressions") {
      parse(
        "fun sum_list(l: List[i32]) -> i32\n l.fold_left(0)(sum)"
      ) ==> Seq(
        FFuncDecl(
          FFuncSig(
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
            FApp(
              FProj(
                FVar("l"),
                Seq(FVar("fold_left"))
              ),
              None,
              Seq(Some(Seq((FInt(0)))), Some(Seq(FVar("sum"))))
            )
          )
        )
      )
    }
    test("parse recursive function definition") {
      parse(
        "fun fib(n: i32, a: i32, b: i32) -> i32\n match n:\n  0 => b\n  _ => fib(n - 1, b, a + b)"
      ) ==> Seq(
        FFuncDecl(
          FFuncSig(
            FIdentifier("fib"),
            None,
            Some(
              Seq(
                FParam(
                  FIdentifier("n"),
                  FSimpleType(FIdentifier("i32"))
                ),
                FParam(
                  FIdentifier("a"),
                  FSimpleType(FIdentifier("i32"))
                ),
                FParam(
                  FIdentifier("b"),
                  FSimpleType(FIdentifier("i32"))
                )
              )
            ),
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(
            FMatch(
              FVar("n"),
              Seq(
                FCase(
                  Seq(FInt(0)),
                  None,
                  Seq(FVar("b"))
                ),
                FCase(
                  Seq(FWildCardPattern),
                  None,
                  Seq(
                    FApp(
                      FVar("fib"),
                      None,
                      Seq(
                        Some(
                          Seq(
                            FSubtraction(FVar("n"), FInt(1)),
                            FVar("b"),
                            FAddition(FVar("a"), FVar("b"))
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
      )
    }
    test("parse tail func definition with generics") {
      parse(
        "fun map[A, B](l: List[A], f: A -> B) -> List[B]\n let iter: List[B] = (acc: List[B], l: List[A]) => {\n  match l:\n   Nil => acc\n   Cons(h, t) => iter(Cons[B](f(h), acc), t)\n  }\n iter(Nil[B](), l)"
      ) ==> Seq(
        FFuncDecl(
          FFuncSig(
            FIdentifier("map"),
            Some(
              Seq(FTypeParam(FIdentifier("A")), FTypeParam(FIdentifier("B")))
            ),
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
              Some(
                FSimpleType(
                  FIdentifier("List"),
                  Some(Seq(FSimpleType(FIdentifier("B"))))
                )
              ),
              Seq(
                FAbs(
                  Seq(
                    FBinding(
                      FIdentifier("acc"),
                      Some(
                        FSimpleType(
                          FIdentifier("List"),
                          Some(Seq(FSimpleType(FIdentifier("B"))))
                        )
                      )
                    ),
                    FBinding(
                      FIdentifier("l"),
                      Some(
                        FSimpleType(
                          FIdentifier("List"),
                          Some(Seq(FSimpleType(FIdentifier("A"))))
                        )
                      )
                    )
                  ),
                  Seq(
                    FMatch(
                      FVar("l"),
                      Seq(
                        FCase(
                          Seq(FIdentifierPattern("Nil")),
                          None,
                          Seq(FVar("acc"))
                        ),
                        FCase(
                          Seq(
                            FVariantOrRecordPattern(
                              FIdentifier("Cons"),
                              Seq(
                                FIdentifierPattern("h"),
                                FIdentifierPattern("t")
                              )
                            )
                          ),
                          None,
                          Seq(
                            FApp(
                              FVar("iter"),
                              None,
                              Seq(
                                Some(
                                  Seq(
                                    FApp(
                                      FVar("Cons"),
                                      Some(List(FSimpleType(FIdentifier("B")))),
                                      Seq(
                                        Some(
                                          Seq(
                                            FApp(
                                              FVar("f"),
                                              None,
                                              Seq(
                                                Some(
                                                  Seq(FVar("h"))
                                                )
                                              )
                                            ),
                                            FVar("acc")
                                          )
                                        )
                                      )
                                    ),
                                    FVar("t")
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
              )
            ),
            FApp(
              FVar("iter"),
              None,
              Seq(
                Some(
                  Seq(
                    FApp(
                      FVar("Nil"),
                      Some(Seq(FSimpleType(FIdentifier("B")))),
                      Seq(None)
                    ),
                    FVar("l")
                  )
                )
              )
            )
          )
        )
      )
    }
    test("parse type implemantion") {
      parse(
        "impl Point:\n fun x_diff(other: Point) -> f32\n  math.abs(this.x - other.x)"
      ) ==> Seq(
        FTypeFuncDecls(
          FIdentifier("Point"),
          None,
          Seq(
            FFuncDecl(
              FFuncSig(
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
                FApp(
                  FProj(
                    FVar("math"),
                    Seq(FVar("abs"))
                  ),
                  None,
                  Seq(
                    Some(
                      Seq(
                        FSubtraction(
                          FProj(
                            FVar("this"),
                            Seq(FVar("x"))
                          ),
                          FProj(
                            FVar("other"),
                            Seq(FVar("x"))
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
      )
    }
    test("parse trait implementation") {
      parse(
        "impl Shape for Circle:\n    fun area() -> f32\n        this.radius * this.radius * 3.14"
      ) ==> Seq(
        FTraitInstance(
          FIdentifier("Shape"),
          None,
          FIdentifier("Circle"),
          None,
          Seq(
            FFuncDecl(
              FFuncSig(
                FIdentifier("area"),
                None,
                None,
                FSimpleType(FIdentifier("f32"), None)
              ),
              Seq(
                FMultiplication(
                  FMultiplication(
                    FProj(
                      FVar("this"),
                      Seq(FVar("radius"))
                    ),
                    FProj(
                      FVar("this"),
                      Seq(FVar("radius"))
                    )
                  ),
                  FFloat(3.14.toFloat)
                )
              )
            )
          )
        )
      )
    }
    test("parse multiple declarations") {
      parse("type bool:\n true\n false\ntype Point:\n x: i32\n y: i32") ==> Seq(
        FVariantTypeDecl(
          FIdentifier("bool"),
          None,
          Seq(
            FVariantTypeValue(FIdentifier("true")),
            FVariantTypeValue(FIdentifier("false"))
          )
        ),
        FRecordTypeDecl(
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
          "impl Shape for Circle:\n    fun area() -> f32\n this.radius * this.radius * 3.14"
        )
      ) { case Failure(ParseError(Position(78, 3, 34), _, _)) =>
      }
    }
  }

  def parse(s: String): Seq[FDecl] = {
    val parser = new FuseParser(s)
    parser.Module.run() match {
      case Success(result) => result
      case Failure(e: ParseError) =>
        sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }

  def runParser(s: String) = {
    val parser = new FuseParser(s)
    parser.Module.run()
  }
}
