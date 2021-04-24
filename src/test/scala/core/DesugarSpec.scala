package core

import cats.data.State
import core.Context._
import parser.FuseExpressionParser._
import parser.FuseLexicalParser._
import parser.FuseParser._
import parser.FuseTypesParser._
import utest._

object DesugarSpec extends TestSuite {
  val tests = Tests {
    test("desugar variant type") {
      desugar(
        FVariantTypeDecl(
          FIdentifier("bool"),
          None,
          Seq(
            FVariantTypeValue(FIdentifier("true")),
            FVariantTypeValue(FIdentifier("false"))
          )
        )
      ) ==> (List(
        ("false", NameBind),
        ("true", NameBind),
        ("bool", NameBind),
        ("@bool", NameBind)
      ),
      List(
        Bind(
          "bool",
          TypeAbbBind(
            TypeRec(
              "@bool",
              KindStar,
              TypeVariant(List(("true", TypeUnit), ("false", TypeUnit)))
            )
          )
        ),
        Bind(
          "true",
          TermAbbBind(
            TermApp(
              TermFold(TypeVar(0, 2)),
              TermTag("true", TermUnit, TypeVar(1, 2))
            )
          )
        ),
        Bind(
          "false",
          TermAbbBind(
            TermApp(
              TermFold(TypeVar(1, 3)),
              TermTag("false", TermUnit, TypeVar(2, 3))
            )
          )
        )
      ))
    }
    test("desugar variant with tuple record type") {
      desugar(
        FVariantTypeDecl(
          FIdentifier("OptionInt"),
          None,
          Seq(
            FVariantTypeValue(FIdentifier("None")),
            FVariantTypeValue(
              FIdentifier("Some"),
              Some(Right(Seq(FSimpleType(FIdentifier("i32")))))
            )
          )
        )
      ) ==> (List(
        ("Some", NameBind),
        ("#1", NameBind),
        ("None", NameBind),
        ("OptionInt", NameBind),
        ("@OptionInt", NameBind)
      ),
      List(
        Bind(
          "OptionInt",
          TypeAbbBind(
            TypeRec(
              "@OptionInt",
              KindStar,
              TypeVariant(
                List(
                  ("None", TypeUnit),
                  ("Some", TypeRecord(List(("1", TypeInt))))
                )
              )
            )
          )
        ),
        Bind(
          "None",
          TermAbbBind(
            TermApp(
              TermFold(TypeVar(0, 2)),
              TermTag("None", TermUnit, TypeVar(1, 2))
            )
          )
        ),
        Bind(
          "Some",
          TermAbbBind(
            TermAbs(
              "#1",
              TypeInt,
              TermApp(
                TermFold(TypeVar(2, 4)),
                TermTag(
                  "Some",
                  TermRecord(List(("1", TermVar(0, 4)))),
                  TypeVar(3, 4)
                )
              )
            )
          )
        )
      ))
    }
    test("desugar variant with record type") {
      desugar(
        FVariantTypeDecl(
          FIdentifier("DataPoint"),
          None,
          Seq(
            FVariantTypeValue(
              FIdentifier("2DPoint"),
              Some(
                Left(
                  Seq(
                    FParam(FIdentifier("x"), FSimpleType(FIdentifier("f32"))),
                    FParam(FIdentifier("y"), FSimpleType(FIdentifier("f32")))
                  )
                )
              )
            ),
            FVariantTypeValue(
              FIdentifier("3DPoint"),
              Some(
                Left(
                  Seq(
                    FParam(FIdentifier("x"), FSimpleType(FIdentifier("f32"))),
                    FParam(FIdentifier("y"), FSimpleType(FIdentifier("f32"))),
                    FParam(FIdentifier("z"), FSimpleType(FIdentifier("f32")))
                  )
                )
              )
            )
          )
        )
      ) ==> (List(
        ("3DPoint", NameBind),
        ("z", NameBind),
        ("y", NameBind),
        ("x", NameBind),
        ("2DPoint", NameBind),
        ("y", NameBind),
        ("x", NameBind),
        ("DataPoint", NameBind),
        ("@DataPoint", NameBind)
      ),
      List(
        Bind(
          "DataPoint",
          TypeAbbBind(
            TypeRec(
              "@DataPoint",
              KindStar,
              TypeVariant(
                List(
                  (
                    "2DPoint",
                    TypeRecord(List(("x", TypeFloat), ("y", TypeFloat)))
                  ),
                  (
                    "3DPoint",
                    TypeRecord(
                      List(("x", TypeFloat), ("y", TypeFloat), ("z", TypeFloat))
                    )
                  )
                )
              )
            )
          )
        ),
        Bind(
          "2DPoint",
          TermAbbBind(
            TermAbs(
              "x",
              TypeFloat,
              TermAbs(
                "y",
                TypeFloat,
                TermApp(
                  TermFold(TypeVar(2, 4)),
                  TermTag(
                    "2DPoint",
                    TermRecord(
                      List(("x", TermVar(1, 4)), ("y", TermVar(0, 4)))
                    ),
                    TypeVar(3, 4)
                  )
                )
              )
            )
          )
        ),
        Bind(
          "3DPoint",
          TermAbbBind(
            TermAbs(
              "x",
              TypeFloat,
              TermAbs(
                "y",
                TypeFloat,
                TermAbs(
                  "z",
                  TypeFloat,
                  TermApp(
                    TermFold(TypeVar(6, 8)),
                    TermTag(
                      "3DPoint",
                      TermRecord(
                        List(
                          ("x", TermVar(2, 8)),
                          ("y", TermVar(1, 8)),
                          ("z", TermVar(0, 8))
                        )
                      ),
                      TypeVar(7, 8)
                    )
                  )
                )
              )
            )
          )
        )
      ))
    }
    test("desugar record type") {
      desugar(
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
      ) ==> (List(
        ("%Point", NameBind),
        ("y", NameBind),
        ("x", NameBind),
        ("Point", NameBind),
        ("@Point", NameBind)
      ),
      List(
        Bind(
          "Point",
          TypeAbbBind(
            TypeRec(
              "@Point",
              KindStar,
              TypeRecord(List(("x", TypeInt), ("y", TypeInt)))
            )
          )
        ),
        Bind(
          "%Point",
          TermAbbBind(
            TermAbs(
              "x",
              TypeInt,
              TermAbs(
                "y",
                TypeInt,
                TermApp(
                  TermFold(TypeVar(2, 4)),
                  TermRecord(List(("x", TermVar(1, 4)), ("y", TermVar(0, 4))))
                )
              )
            )
          )
        )
      ))
    }
    test("desugar tuple record type") {
      desugar(
        FTupleTypeDecl(
          FIdentifier("Pair"),
          None,
          Seq(FSimpleType(FIdentifier("i32")), FSimpleType(FIdentifier("str")))
        )
      ) ==> (List(
        ("%Pair", NameBind),
        ("#2", NameBind),
        ("#1", NameBind),
        ("Pair", NameBind),
        ("@Pair", NameBind)
      ),
      List(
        Bind(
          "Pair",
          TypeAbbBind(
            TypeRec(
              "@Pair",
              KindStar,
              TypeRecord(List(("1", TypeInt), ("2", TypeString)))
            )
          )
        ),
        Bind(
          "%Pair",
          TermAbbBind(
            TermAbs(
              "#1",
              TypeInt,
              TermAbs(
                "#2",
                TypeString,
                TermApp(
                  TermFold(TypeVar(2, 4)),
                  TermRecord(List(("1", TermVar(1, 4)), ("2", TermVar(0, 4))))
                )
              )
            )
          )
        )
      ))
    }
    test("desugar recursive variant type") {
      desugar(
        FVariantTypeDecl(
          FIdentifier("ListInt"),
          None,
          Seq(
            FVariantTypeValue(FIdentifier("Nil")),
            FVariantTypeValue(
              FIdentifier("Cons"),
              Some(
                Left(
                  Seq(
                    FParam(
                      FIdentifier("head"),
                      FSimpleType(FIdentifier("i32"))
                    ),
                    FParam(
                      FIdentifier("t"),
                      FSimpleType(FIdentifier("ListInt"))
                    )
                  )
                )
              )
            )
          )
        )
      ) ==>
        (
          List(
            ("Cons", NameBind),
            ("t", NameBind),
            ("head", NameBind),
            ("Nil", NameBind),
            ("ListInt", NameBind),
            ("@ListInt", NameBind)
          ),
          List(
            Bind(
              "ListInt",
              TypeAbbBind(
                TypeRec(
                  "@ListInt",
                  KindStar,
                  TypeVariant(
                    List(
                      ("Nil", TypeUnit),
                      (
                        "Cons",
                        TypeRecord(
                          List(("head", TypeInt), ("t", TypeVar(0, 1)))
                        )
                      )
                    )
                  )
                )
              )
            ),
            Bind(
              "Nil",
              TermAbbBind(
                TermApp(
                  TermFold(TypeVar(0, 2)),
                  TermTag("Nil", TermUnit, TypeVar(1, 2))
                )
              )
            ),
            Bind(
              "Cons",
              TermAbbBind(
                TermAbs(
                  "head",
                  TypeInt,
                  TermAbs(
                    "t",
                    TypeVar(2, 4),
                    TermApp(
                      TermFold(TypeVar(3, 5)),
                      TermTag(
                        "Cons",
                        TermRecord(
                          List(("head", TermVar(1, 5)), ("t", TermVar(0, 5)))
                        ),
                        TypeVar(4, 5)
                      )
                    )
                  )
                )
              )
            )
          )
        )
    }
    test("desugar recursive tuple record type") {
      desugar(
        FTupleTypeDecl(
          FIdentifier("Pair2"),
          None,
          Seq(
            FSimpleType(FIdentifier("i32")),
            FSimpleType(FIdentifier("Pair2"))
          )
        )
      ) ==> (List(
        ("%Pair2", NameBind),
        ("#2", NameBind),
        ("#1", NameBind),
        ("Pair2", NameBind),
        ("@Pair2", NameBind)
      ),
      List(
        Bind(
          "Pair2",
          TypeAbbBind(
            TypeRec(
              "@Pair2",
              KindStar,
              TypeRecord(List(("1", TypeInt), ("2", TypeVar(0, 1))))
            )
          )
        ),
        Bind(
          "%Pair2",
          TermAbbBind(
            TermAbs(
              "#1",
              TypeInt,
              TermAbs(
                "#2",
                TypeVar(1, 3),
                TermApp(
                  TermFold(TypeVar(2, 4)),
                  TermRecord(List(("1", TermVar(1, 4)), ("2", TermVar(0, 4))))
                )
              )
            )
          )
        )
      ))
    }
    test("desugar type function abbreviation") {
      desugar(
        FTypeAlias(
          FIdentifier("IntToString"),
          None,
          FFuncType(
            Seq(FSimpleType(FIdentifier("i32"))),
            FSimpleType(FIdentifier("str"))
          )
        )
      ) ==> (List(("IntToString", NameBind)),
      List(
        Bind(
          "IntToString",
          TypeAbbBind(TypeArrow(TypeInt, TypeString))
        )
      ))
    }
    test("desugar parametric record type") {
      desugar(
        FRecordTypeDecl(
          FIdentifier("Point"),
          Some(Seq(FTypeParam(FIdentifier("T")))),
          Seq(
            FRecordTypeField(
              FParam(FIdentifier("x"), FSimpleType(FIdentifier("T")))
            ),
            FRecordTypeField(
              FParam(FIdentifier("y"), FSimpleType(FIdentifier("T")))
            )
          )
        )
      ) ==> (
        List(
          ("%Point", NameBind),
          ("y", NameBind),
          ("x", NameBind),
          ("T", NameBind),
          ("Point", NameBind),
          ("@Point", NameBind),
          ("T", NameBind)
        ),
        List(
          Bind(
            "Point",
            TypeAbbBind(
              TypeAbs(
                "T",
                TypeRec(
                  "@Point",
                  KindArrow(KindStar, KindStar),
                  TypeRecord(List(("x", TypeVar(1, 2)), ("y", TypeVar(1, 2))))
                )
              )
            )
          ),
          Bind(
            "%Point",
            TermAbbBind(
              TermTAbs(
                "T",
                TermAbs(
                  "x",
                  TypeVar(0, 4),
                  TermAbs(
                    "y",
                    TypeVar(1, 5),
                    TermApp(
                      TermFold(TypeApp(TypeVar(3, 6), TypeVar(2, 6))),
                      TermRecord(
                        List(
                          ("x", TermVar(1, 6)),
                          ("y", TermVar(0, 6))
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
    test("desugar parametric variant type") {
      desugar(
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
      ) ==> (List(
        ("Some", NameBind),
        ("#1", NameBind),
        ("T", NameBind),
        ("None", NameBind),
        ("T", NameBind),
        ("Option", NameBind),
        ("@Option", NameBind),
        ("T", NameBind)
      ),
      List(
        Bind(
          "Option",
          TypeAbbBind(
            TypeAbs(
              "T",
              TypeRec(
                "@Option",
                KindArrow(KindStar, KindStar),
                TypeVariant(
                  List(
                    ("None", TypeUnit),
                    ("Some", TypeRecord(List(("1", TypeVar(1, 2)))))
                  )
                )
              )
            )
          )
        ),
        Bind(
          "None",
          TermAbbBind(
            TermTAbs(
              "T",
              TermApp(
                TermFold(TypeApp(TypeVar(1, 4), TypeVar(0, 4))),
                TermTag("None", TermUnit, TypeVar(2, 4))
              )
            )
          )
        ),
        Bind(
          "Some",
          TermAbbBind(
            TermTAbs(
              "T",
              TermAbs(
                "#1",
                TypeVar(0, 6),
                TermApp(
                  TermFold(TypeApp(TypeVar(4, 7), TypeVar(1, 7))),
                  TermTag(
                    "Some",
                    TermRecord(List(("1", TermVar(0, 7)))),
                    TypeVar(5, 7)
                  )
                )
              )
            )
          )
        )
      ))
    }
    test("desugar algebraic data type -> recursive + parametric") {
      val test =
        desugar(
          FVariantTypeDecl(
            FIdentifier("List"),
            Some(Seq(FTypeParam(FIdentifier("A")))),
            Seq(
              FVariantTypeValue(FIdentifier("Nil")),
              FVariantTypeValue(
                FIdentifier("Cons"),
                Some(
                  Right(
                    Seq(
                      FSimpleType(FIdentifier("A")),
                      FSimpleType(
                        FIdentifier("List"),
                        Some(Seq(FSimpleType(FIdentifier("A"))))
                      )
                    )
                  )
                )
              )
            )
          )
        ) ==> (List(
          ("Cons", NameBind),
          ("#2", NameBind),
          ("#1", NameBind),
          ("A", NameBind),
          ("Nil", NameBind),
          ("A", NameBind),
          ("List", NameBind),
          ("@List", NameBind),
          ("A", NameBind)
        ),
        List(
          Bind(
            "List",
            TypeAbbBind(
              TypeAbs(
                "A",
                TypeRec(
                  "@List",
                  KindArrow(KindStar, KindStar),
                  TypeVariant(
                    List(
                      ("Nil", TypeUnit),
                      (
                        "Cons",
                        TypeRecord(
                          List(
                            ("1", TypeVar(1, 2)),
                            ("2", TypeApp(TypeVar(0, 2), TypeVar(1, 2)))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          Bind(
            "Nil",
            TermAbbBind(
              TermTAbs(
                "A",
                TermApp(
                  TermFold(TypeApp(TypeVar(1, 4), TypeVar(0, 4))),
                  TermTag(
                    "Nil",
                    TermUnit,
                    TypeVar(2, 4)
                  )
                )
              )
            )
          ),
          Bind(
            "Cons",
            TermAbbBind(
              TermTAbs(
                "A",
                TermAbs(
                  "#1",
                  TypeVar(0, 6),
                  TermAbs(
                    "#2",
                    TypeApp(TypeVar(4, 7), TypeVar(1, 7)),
                    TermApp(
                      TermFold(TypeApp(TypeVar(5, 8), TypeVar(2, 8))),
                      TermTag(
                        "Cons",
                        TermRecord(
                          List(("1", TermVar(1, 8)), ("2", TermVar(0, 8)))
                        ),
                        TypeVar(6, 8)
                      )
                    )
                  )
                )
              )
            )
          )
        ))
    }
    test("desugar type function0 abbreviation") {
      desugar(
        FTypeAlias(
          FIdentifier("Function0"),
          Some(Seq(FTypeParam(FIdentifier("T")))),
          FFuncType(Seq(), FSimpleType(FIdentifier("T")))
        )
      ) ==> (List(("Function0", NameBind), ("T", NameBind)),
      List(
        Bind(
          "Function0",
          TypeAbbBind(TypeAbs("T", TypeArrow(TypeUnit, TypeVar(0, 1))))
        )
      ))
    }
    test("desugar type function1 abbreviation") {
      desugar(
        FTypeAlias(
          FIdentifier("Function1"),
          Some(
            Seq(FTypeParam(FIdentifier("A")), FTypeParam(FIdentifier("B")))
          ),
          FFuncType(
            Seq(FSimpleType(FIdentifier("A"))),
            FSimpleType(FIdentifier("B"))
          )
        )
      ) ==> (List(("Function1", NameBind), ("B", NameBind), ("A", NameBind)),
      List(
        Bind(
          "Function1",
          TypeAbbBind(
            TypeAbs("A", TypeAbs("B", TypeArrow(TypeVar(1, 2), TypeVar(0, 2))))
          )
        )
      ))
    }
    test("desugar type function2 abbreviation") {
      desugar(
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
      ) ==> (
        List(
          ("Function2", NameBind),
          ("C", NameBind),
          ("B", NameBind),
          ("A", NameBind)
        ),
        List(
          Bind(
            "Function2",
            TypeAbbBind(
              TypeAbs(
                "A",
                TypeAbs(
                  "B",
                  TypeAbs(
                    "C",
                    TypeArrow(
                      TypeVar(2, 3),
                      TypeArrow(TypeVar(1, 3), TypeVar(0, 3))
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
    test("desugar type curried function abbreviation") {
      desugar(
        FTypeAlias(
          FIdentifier("CurriedInt"),
          None,
          FFuncType(
            Seq(FSimpleType(FIdentifier("i32"))),
            FFuncType(
              Seq(FSimpleType(FIdentifier("i32"))),
              FSimpleType(FIdentifier("i32"))
            )
          )
        )
      ) ==> (List(("CurriedInt", NameBind)), List(
        Bind(
          "CurriedInt",
          TypeAbbBind(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
        )
      ))
    }
    test("desugar algebraic data type with three type parameters") {
      desugar(
        FTupleTypeDecl(
          FIdentifier("DataPoint"),
          Some(
            Seq(
              FTypeParam(FIdentifier("A")),
              FTypeParam(FIdentifier("B")),
              FTypeParam(FIdentifier("C"))
            )
          ),
          Seq(
            FSimpleType(
              FIdentifier("DataPoint"),
              Some(
                Seq(
                  FSimpleType(FIdentifier("C")),
                  FSimpleType(FIdentifier("B")),
                  FSimpleType(FIdentifier("A"))
                )
              )
            )
          )
        )
      ) ==> (
        List(
          ("%DataPoint", NameBind),
          ("#1", NameBind),
          ("C", NameBind),
          ("B", NameBind),
          ("A", NameBind),
          ("DataPoint", NameBind),
          ("@DataPoint", NameBind),
          ("C", NameBind),
          ("B", NameBind),
          ("A", NameBind)
        ),
        List(
          Bind(
            "DataPoint",
            TypeAbbBind(
              TypeAbs(
                "A",
                TypeAbs(
                  "B",
                  TypeAbs(
                    "C",
                    TypeRec(
                      "@DataPoint",
                      KindArrow(
                        KindStar,
                        KindArrow(KindStar, KindArrow(KindStar, KindStar))
                      ),
                      TypeRecord(
                        List(
                          (
                            "1",
                            TypeApp(
                              TypeApp(
                                TypeApp(TypeVar(0, 4), TypeVar(1, 4)),
                                TypeVar(2, 4)
                              ),
                              TypeVar(3, 4)
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
          Bind(
            "%DataPoint",
            TermAbbBind(
              TermTAbs(
                "A",
                TermTAbs(
                  "B",
                  TermTAbs(
                    "C",
                    TermAbs(
                      "#1",
                      TypeApp(
                        TypeApp(
                          TypeApp(TypeVar(3, 8), TypeVar(0, 8)),
                          TypeVar(1, 8)
                        ),
                        TypeVar(2, 8)
                      ),
                      TermApp(
                        TermFold(
                          TypeApp(
                            TypeApp(
                              TypeApp(TypeVar(4, 9), TypeVar(3, 9)),
                              TypeVar(2, 9)
                            ),
                            TypeVar(1, 9)
                          )
                        ),
                        TermRecord(List(("1", TermVar(0, 9))))
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
    test("desugar function with 1 * 2") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("one_and_two"),
            None,
            None,
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(FMultiplication(FInt(1), FInt(2)))
        ),
        List(("&multiply", NameBind)) // Built-in function.
      ) ==> (List(("one_and_two", NameBind), ("&multiply", NameBind)),
      List(
        Bind(
          "one_and_two",
          TermAbbBind(
            TermAbs(
              "_",
              TypeUnit,
              TermApp(TermApp(TermVar(0, 1), TermInt(1)), TermInt(2)),
              Some(TypeInt)
            )
          )
        )
      ))
    }
    test("desugar function with additive expression") {
      desugar(
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
        ),
        List(("&add", NameBind)) // Built-in function.
      ) ==> (List(
        ("sum", NameBind),
        ("y", NameBind),
        ("x", NameBind),
        ("^sum", NameBind),
        ("&add", NameBind)
      ),
      List(
        Bind(
          "sum",
          TermAbbBind(
            TermFix(
              TermAbs(
                "^sum",
                TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)),
                TermAbs(
                  "x",
                  TypeInt,
                  TermAbs(
                    "y",
                    TypeInt,
                    TermApp(
                      TermApp(TermVar(3, 4), TermVar(1, 4)),
                      TermVar(0, 4)
                    ),
                    Some(TypeInt)
                  )
                )
              )
            )
          )
        )
      ))
    }
    test("desugar function with simple call expressions") {
      desugar(
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
              Seq(Some(Seq(FInt(3), FInt(2))))
            )
          )
        ),
        List(("sum", NameBind))
      ) ==> (List(("sum_three_and_two", NameBind), ("sum", NameBind)),
      List(
        Bind(
          "sum_three_and_two",
          TermAbbBind(
            TermAbs(
              "_",
              TypeUnit,
              TermApp(TermApp(TermVar(0, 1), TermInt(3)), TermInt(2)),
              Some(TypeInt)
            )
          )
        )
      ))
    }
    test("desugar function with record data constructor") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("zero_point"),
            None,
            None,
            FSimpleType(FIdentifier("Point"), None)
          ),
          Seq(
            FApp(
              FVar("Point"),
              Seq(Some(Seq(FInt(0), FInt(0))))
            )
          )
        ),
        List(("Point", NameBind))
      ) ==> (List(("zero_point", NameBind), ("Point", NameBind)),
      List(
        Bind(
          "zero_point",
          TermAbbBind(
            TermAbs(
              "_",
              TypeUnit,
              TermApp(TermApp(TermVar(0, 1), TermInt(0)), TermInt(0)),
              Some(TypeVar(0, 1))
            )
          )
        )
      ))
    }
    test("desugar function with variant data constructor") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("some_value"),
            None,
            Some(
              Seq(
                FParam(
                  FIdentifier("x"),
                  FSimpleType(FIdentifier("i32"))
                )
              )
            ),
            FSimpleType(FIdentifier("OptionInt"), None)
          ),
          Seq(
            FApp(
              FVar("Some"),
              Seq(Some(Seq(FVar("x"))))
            )
          )
        ),
        List(
          ("Some", NameBind),
          ("OptionInt", NameBind)
        )
      ) ==> (List(
        ("some_value", NameBind),
        ("x", NameBind),
        ("^some_value", NameBind),
        ("Some", NameBind),
        ("OptionInt", NameBind)
      ),
      List(
        Bind(
          "some_value",
          TermAbbBind(
            TermFix(
              TermAbs(
                "^some_value",
                TypeArrow(TypeInt, TypeVar(1, 2)),
                TermAbs(
                  "x",
                  TypeInt,
                  TermApp(TermVar(2, 4), TermVar(0, 4)),
                  Some(TypeVar(3, 4))
                )
              )
            )
          )
        )
      ))
    }
    test("desugar function with type parameters") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("some_t_value"),
            Some(
              Seq(FTypeParam(FIdentifier("T")))
            ),
            Some(
              Seq(
                FParam(
                  FIdentifier("x"),
                  FSimpleType(FIdentifier("T"))
                )
              )
            ),
            FSimpleType(
              FIdentifier("Option"),
              Some(Seq(FSimpleType(FIdentifier("T"))))
            )
          ),
          Seq(
            FApp(
              FVar("Some"),
              Seq(Some(Seq(FVar("x"))))
            )
          )
        ),
        List(
          ("Some", NameBind),
          ("Option", NameBind)
        )
      ) ==> (List(
        ("some_t_value", NameBind),
        ("x", NameBind),
        ("^some_t_value", NameBind),
        ("T", NameBind),
        ("Some", NameBind),
        ("Option", NameBind)
      ),
      List(
        Bind(
          "some_t_value",
          TermAbbBind(
            TermTAbs(
              "T",
              TermFix(
                TermAbs(
                  "^some_t_value",
                  TypeArrow(
                    TypeVar(0, 3),
                    TypeApp(TypeVar(2, 3), TypeVar(0, 3))
                  ),
                  TermAbs(
                    "x",
                    TypeVar(1, 4),
                    TermApp(TermVar(3, 5), TermVar(0, 5)),
                    Some(TypeApp(TypeVar(4, 5), TypeVar(2, 5)))
                  )
                )
              )
            )
          )
        )
      ))
    }
    test("desugar function with match expresion with literals") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("zero_to_one"),
            None,
            Some(
              Seq(
                FParam(
                  FIdentifier("x"),
                  FSimpleType(FIdentifier("i32"))
                )
              )
            ),
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(
            FMatch(
              FVar("x"),
              Seq(
                FCase(
                  Seq(FInt(0)),
                  None,
                  Seq(FInt(1))
                ),
                FCase(
                  Seq(FWildCardPattern),
                  None,
                  Seq(FVar("x"))
                )
              )
            )
          )
        )
      ) ==> (List(
        ("zero_to_one", NameBind),
        ("x", NameBind),
        ("^zero_to_one", NameBind)
      ),
      List(
        Bind(
          "zero_to_one",
          TermAbbBind(
            TermFix(
              TermAbs(
                "^zero_to_one",
                TypeArrow(TypeInt, TypeInt),
                TermAbs(
                  "x",
                  TypeInt,
                  TermMatch(
                    TermVar(0, 2),
                    List(
                      (TermInt(0), TermInt(1)),
                      (PatternDefault, TermVar(0, 2))
                    )
                  ),
                  Some(TypeInt)
                )
              )
            )
          )
        )
      ))
    }
    test("desugar function with match expresion with adt unfolding") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("some_plus_one"),
            Some(
              Seq(FTypeParam(FIdentifier("T")))
            ),
            Some(
              Seq(
                FParam(
                  FIdentifier("x"),
                  FSimpleType(
                    FIdentifier("Option"),
                    Some(Seq(FSimpleType(FIdentifier("T"))))
                  )
                )
              )
            ),
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(
            FMatch(
              FVar("x"),
              Seq(
                FCase(
                  Seq(
                    FVariantOrRecordPattern(
                      FIdentifier("Some"),
                      Seq(FIdentifierPattern("v"))
                    )
                  ),
                  None,
                  Seq(FAddition(FVar("v"), FInt(1)))
                ),
                FCase(
                  Seq(FIdentifierPattern("None")),
                  None,
                  Seq(FInt(0))
                )
              )
            )
          )
        ),
        List(
          ("Some", NameBind),
          ("Option", NameBind),
          ("&add", NameBind)
        ) // Built-in function.
      ) ==> (List(
        ("some_plus_one", NameBind),
        ("v", NameBind),
        ("x", NameBind),
        ("^some_plus_one", NameBind),
        ("T", NameBind),
        ("Some", NameBind),
        ("Option", NameBind),
        ("&add", NameBind)
      ),
      List(
        Bind(
          "some_plus_one",
          TermAbbBind(
            TermTAbs(
              "T",
              TermFix(
                TermAbs(
                  "^some_plus_one",
                  TypeArrow(
                    TypeApp(TypeVar(2, 4), TypeVar(0, 4)),
                    TypeInt
                  ),
                  TermAbs(
                    "x",
                    TypeApp(TypeVar(3, 5), TypeVar(1, 5)),
                    TermMatch(
                      TermVar(0, 6),
                      List(
                        (
                          PatternNode("Some", List("v")),
                          TermApp(
                            TermApp(TermVar(6, 7), TermVar(0, 7)),
                            TermInt(1)
                          )
                        ),
                        (PatternNode("None", List()), TermInt(0))
                      )
                    ),
                    Some(TypeInt)
                  )
                )
              )
            )
          )
        )
      ))
    }
    test("desugar recursive function") {
      desugar(
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
                      Seq(
                        Some(Seq(FSubtraction(FVar("n"), FInt(1)))),
                        Some(Seq(FAddition(FVar("a"), FVar("b"))))
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        List(("&sub", NameBind), ("&add", NameBind)) // Built-in function.
      ) ==> (List(
        ("fib", NameBind),
        ("b", NameBind),
        ("a", NameBind),
        ("n", NameBind),
        ("^fib", NameBind),
        ("&sub", NameBind),
        ("&add", NameBind)
      ),
      List(
        Bind(
          "fib",
          TermAbbBind(
            TermFix(
              TermAbs(
                "^fib",
                TypeArrow(
                  TypeInt,
                  TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt))
                ),
                TermAbs(
                  "n",
                  TypeInt,
                  TermAbs(
                    "a",
                    TypeInt,
                    TermAbs(
                      "b",
                      TypeInt,
                      TermMatch(
                        TermVar(2, 6),
                        List(
                          (TermInt(0), TermVar(0, 6)),
                          (
                            PatternDefault,
                            TermApp(
                              TermApp(
                                TermVar(3, 6),
                                TermApp(
                                  TermApp(TermVar(4, 6), TermVar(2, 6)),
                                  TermInt(1)
                                )
                              ),
                              TermApp(
                                TermApp(TermVar(5, 6), TermVar(1, 6)),
                                TermVar(0, 6)
                              )
                            )
                          )
                        )
                      ),
                      Some(TypeInt)
                    )
                  )
                )
              )
            )
          )
        )
      ))
    }
    test("desugar function with let expression") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("one_and_two_plus_one"),
            None,
            None,
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(
            FLetExpr(
              FIdentifier("x"),
              None,
              Seq(FMultiplication(FInt(1), FInt(2)))
            ),
            FAddition(FVar("x"), FInt(1))
          )
        ),
        List(("&multiply", NameBind), ("&add", NameBind)) // Built-in function.
      ) ==> (List(
        ("one_and_two_plus_one", NameBind),
        ("x", NameBind),
        ("&multiply", NameBind),
        ("&add", NameBind)
      ),
      List(
        Bind(
          "one_and_two_plus_one",
          TermAbbBind(
            TermAbs(
              "_",
              TypeUnit,
              TermLet(
                "x",
                TermApp(TermApp(TermVar(0, 2), TermInt(1)), TermInt(2)),
                TermApp(TermApp(TermVar(2, 3), TermVar(0, 3)), TermInt(1))
              ),
              Some(TypeInt)
            )
          )
        )
      ))
    }
    test("desugar function with sequence of let expressions") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("compute_z"),
            None,
            None,
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(
            FLetExpr(
              FIdentifier("x"),
              None,
              Seq(FInt(5))
            ),
            FLetExpr(
              FIdentifier("y"),
              None,
              Seq(FAddition(FVar("x"), FInt(1)))
            ),
            FMultiplication(FVar("x"), FVar("y"))
          )
        ),
        List(("&multiply", NameBind), ("&add", NameBind)) // Built-in function.
      ) ==> (List(
        ("compute_z", NameBind),
        ("y", NameBind),
        ("x", NameBind),
        ("&multiply", NameBind),
        ("&add", NameBind)
      ),
      List(
        Bind(
          "compute_z",
          TermAbbBind(
            TermAbs(
              "_",
              TypeUnit,
              TermLet(
                "x",
                TermInt(5),
                TermLet(
                  "y",
                  TermApp(TermApp(TermVar(2, 3), TermVar(0, 3)), TermInt(1)),
                  TermApp(TermApp(TermVar(2, 4), TermVar(1, 4)), TermVar(0, 4))
                )
              ),
              Some(TypeInt)
            )
          )
        )
      ))
    }
    test("desugar function with let expression with lambda expression") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("plus_one"),
            None,
            None,
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(
            FLetExpr(
              FIdentifier("f"),
              None,
              Seq(
                FAbs(
                  Seq(
                    FBinding(
                      FIdentifier("x"),
                      Some(FSimpleType(FIdentifier("i32")))
                    )
                  ),
                  Seq(
                    FAddition(FVar("x"), FInt(1))
                  )
                )
              )
            ),
            FApp(FVar("f"), Seq(Some(Seq(FInt(5)))))
          )
        ),
        List(("&add", NameBind)) // Built-in function.
      ) ==> (List(
        ("plus_one", NameBind),
        ("f", NameBind),
        ("x", NameBind),
        ("&add", NameBind)
      ),
      List(
        Bind(
          "plus_one",
          TermAbbBind(
            TermAbs(
              "_",
              TypeUnit,
              TermLet(
                "f",
                TermClosure(
                  "x",
                  Some(TypeInt),
                  TermApp(TermApp(TermVar(1, 2), TermVar(0, 2)), TermInt(1))
                ),
                TermApp(TermVar(0, 3), TermInt(5))
              ),
              Some(TypeInt)
            )
          )
        )
      ))
    }
    test(
      "desuagar function with let expression wtih lambda expression using multiple params"
    ) {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("compute_three"),
            None,
            None,
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(
            FLetExpr(
              FIdentifier("g"),
              None,
              Seq(
                FAbs(
                  Seq(
                    FBinding(
                      FIdentifier("x"),
                      Some(FSimpleType(FIdentifier("i32")))
                    ),
                    FBinding(
                      FIdentifier("y"),
                      Some(FSimpleType(FIdentifier("i32")))
                    ),
                    FBinding(
                      FIdentifier("z"),
                      Some(FSimpleType(FIdentifier("i32")))
                    )
                  ),
                  Seq(
                    FAddition(FMultiplication(FVar("x"), FVar("y")), FVar("z"))
                  )
                )
              )
            ),
            FApp(FVar("g"), Seq(Some(Seq(FInt(1), FInt(2), FInt(3)))))
          )
        ),
        List(("&multiply", NameBind), ("&add", NameBind)) // Built-in function.
      ) ==> (List(
        ("compute_three", NameBind),
        ("g", NameBind),
        ("z", NameBind),
        ("y", NameBind),
        ("x", NameBind),
        ("&multiply", NameBind),
        ("&add", NameBind)
      ),
      List(
        Bind(
          "compute_three",
          TermAbbBind(
            TermAbs(
              "_",
              TypeUnit,
              TermLet(
                "g",
                TermClosure(
                  "x",
                  Some(TypeInt),
                  TermClosure(
                    "y",
                    Some(TypeInt),
                    TermClosure(
                      "z",
                      Some(TypeInt),
                      TermApp(
                        TermApp(
                          TermVar(4, 5),
                          TermApp(
                            TermApp(TermVar(3, 5), TermVar(2, 5)),
                            TermVar(1, 5)
                          )
                        ),
                        TermVar(0, 5)
                      )
                    )
                  )
                ),
                TermApp(
                  TermApp(TermApp(TermVar(0, 6), TermInt(1)), TermInt(2)),
                  TermInt(3)
                )
              ),
              Some(TypeInt)
            )
          )
        )
      ))
    }
    test("desuagar function with call expression wtih inline lambda argument") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("option_and_one"),
            None,
            Some(
              Seq(
                FParam(
                  FIdentifier("x"),
                  FSimpleType(FIdentifier("OptionInt"))
                )
              )
            ),
            FSimpleType(FIdentifier("OptionInt"), None)
          ),
          Seq(
            FApp(
              FVar("map"),
              Seq(
                Some(
                  Seq(
                    FVar("x"),
                    FAbs(
                      Seq(
                        FBinding(
                          FIdentifier("a"),
                          Some(FSimpleType(FIdentifier("i32")))
                        )
                      ),
                      Seq(
                        FAddition(
                          FVar("a"),
                          FInt(1)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        List(
          ("map", NameBind),
          ("OptionInt", NameBind),
          ("&add", NameBind)
        )
      ) ==> (List(
        ("option_and_one", NameBind),
        ("a", NameBind),
        ("x", NameBind),
        ("^option_and_one", NameBind),
        ("map", NameBind),
        ("OptionInt", NameBind),
        ("&add", NameBind)
      ),
      List(
        Bind(
          "option_and_one",
          TermAbbBind(
            TermFix(
              TermAbs(
                "^option_and_one",
                TypeArrow(TypeVar(1, 3), TypeVar(1, 3)),
                TermAbs(
                  "x",
                  TypeVar(2, 4),
                  TermApp(
                    TermApp(
                      TermVar(2, 5),
                      TermVar(0, 5)
                    ),
                    TermClosure(
                      "a",
                      Some(TypeInt),
                      TermApp(TermApp(TermVar(5, 6), TermVar(0, 6)), TermInt(1))
                    )
                  ),
                  Some(TypeVar(4, 6))
                )
              )
            )
          )
        )
      ))
    }
    test("desugar type func decls") {}
    // TODO: Learn how to represent traits (type classes) in the lambda calculus.
    // test("desugar trait decl") {}
    // test("desugar trait instance decl") {}
  }

  def desugar(d: FDecl, ctx: Context = Context.empty): (Context, List[Bind]) = {
    Desugar.bind(d).run(ctx).value
  }
}
