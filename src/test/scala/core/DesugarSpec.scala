package core

import cats.data.State
import core.Context._
import parser.Expressions._
import parser.Identifiers._
import parser.Types._
import parser.FuseParser._
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
        ("bool", NameBind)
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
              TermFold(TypeVar(0, 1)),
              TermTag("true", TermUnit, TypeVar(0, 1))
            )
          )
        ),
        Bind(
          "false",
          TermAbbBind(
            TermApp(
              TermFold(TypeVar(1, 2)),
              TermTag("false", TermUnit, TypeVar(1, 2))
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
        ("None", NameBind),
        ("OptionInt", NameBind)
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
              TermFold(TypeVar(0, 1)),
              TermTag("None", TermUnit, TypeVar(0, 1))
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
                TermFold(TypeVar(2, 3)),
                TermTag(
                  "Some",
                  TermRecord(List(("1", TermVar(0, 3)))),
                  TypeVar(2, 3)
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
        ("2DPoint", NameBind),
        ("DataPoint", NameBind)
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
                  TermFold(TypeVar(2, 3)),
                  TermTag(
                    "2DPoint",
                    TermRecord(
                      List(("x", TermVar(1, 3)), ("y", TermVar(0, 3)))
                    ),
                    TypeVar(2, 3)
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
                    TermFold(TypeVar(4, 5)),
                    TermTag(
                      "3DPoint",
                      TermRecord(
                        List(
                          ("x", TermVar(2, 5)),
                          ("y", TermVar(1, 5)),
                          ("z", TermVar(0, 5))
                        )
                      ),
                      TypeVar(4, 5)
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
        ("Point", NameBind)
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
                  TermFold(TypeVar(2, 3)),
                  TermRecord(List(("x", TermVar(1, 3)), ("y", TermVar(0, 3))))
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
        ("Pair", NameBind)
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
                  TermFold(TypeVar(2, 3)),
                  TermRecord(List(("1", TermVar(1, 3)), ("2", TermVar(0, 3))))
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
            ("Nil", NameBind),
            ("ListInt", NameBind)
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
                  TermFold(TypeVar(0, 1)),
                  TermTag("Nil", TermUnit, TypeVar(0, 1))
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
                    TypeVar(2, 3),
                    TermApp(
                      TermFold(TypeVar(3, 4)),
                      TermTag(
                        "Cons",
                        TermRecord(
                          List(("head", TermVar(1, 4)), ("t", TermVar(0, 4)))
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
        ("Pair2", NameBind)
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
                TypeVar(1, 2),
                TermApp(
                  TermFold(TypeVar(2, 3)),
                  TermRecord(List(("1", TermVar(1, 3)), ("2", TermVar(0, 3))))
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
          ("Point", NameBind)
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
                  TypeVar(0, 2),
                  TermAbs(
                    "y",
                    TypeVar(1, 3),
                    TermApp(
                      TermFold(TypeApp(TypeVar(3, 4), TypeVar(2, 4))),
                      TermRecord(
                        List(
                          ("x", TermVar(1, 4)),
                          ("y", TermVar(0, 4))
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
        ("None", NameBind),
        ("Option", NameBind)
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
                TermFold(TypeApp(TypeVar(1, 2), TypeVar(0, 2))),
                TermTag("None", TermUnit, TypeApp(TypeVar(1, 2), TypeVar(0, 2)))
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
                TypeVar(0, 3),
                TermApp(
                  TermFold(TypeApp(TypeVar(3, 4), TypeVar(1, 4))),
                  TermTag(
                    "Some",
                    TermRecord(List(("1", TermVar(0, 4)))),
                    TypeApp(TypeVar(3, 4), TypeVar(1, 4))
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
          ("Nil", NameBind),
          ("List", NameBind)
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
                  TermFold(TypeApp(TypeVar(1, 2), TypeVar(0, 2))),
                  TermTag(
                    "Nil",
                    TermUnit,
                    TypeApp(TypeVar(1, 2), TypeVar(0, 2))
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
                  TypeVar(0, 3),
                  TermAbs(
                    "#2",
                    TypeApp(TypeVar(3, 4), TypeVar(1, 4)),
                    TermApp(
                      TermFold(TypeApp(TypeVar(4, 5), TypeVar(2, 5))),
                      TermTag(
                        "Cons",
                        TermRecord(
                          List(("1", TermVar(1, 5)), ("2", TermVar(0, 5)))
                        ),
                        TypeApp(TypeVar(4, 5), TypeVar(2, 5))
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
      ) ==> (List(("Function0", NameBind)),
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
      ) ==> (List(("Function1", NameBind)),
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
        List(("Function2", NameBind)),
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
          ("DataPoint", NameBind)
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
                          TypeApp(TypeVar(3, 4), TypeVar(0, 4)),
                          TypeVar(1, 4)
                        ),
                        TypeVar(2, 4)
                      ),
                      TermApp(
                        TermFold(
                          TypeApp(
                            TypeApp(
                              TypeApp(TypeVar(4, 5), TypeVar(3, 5)),
                              TypeVar(2, 5)
                            ),
                            TypeVar(1, 5)
                          )
                        ),
                        TermRecord(List(("1", TermVar(0, 5))))
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
              TermApp(TermApp(TermVar(1, 2), TermInt(1)), TermInt(2)),
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
              None,
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
              TermApp(TermApp(TermVar(1, 2), TermInt(3)), TermInt(2)),
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
              None,
              Seq(Some(Seq(FInt(0), FInt(0))))
            )
          )
        ),
        List(("%Point", NameBind), ("Point", NameBind))
      ) ==> (List(
        ("zero_point", NameBind),
        ("%Point", NameBind),
        ("Point", NameBind),
      ),
      List(
        Bind(
          "zero_point",
          TermAbbBind(
            TermAbs(
              "_",
              TypeUnit,
              TermApp(TermApp(TermVar(1, 3), TermInt(0)), TermInt(0)),
              Some(TypeVar(2, 3))
            )
          )
        )
      ))
    }
    test("desugar function with record projection") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("x_point_sum"),
            None,
            Some(
              Seq(
                FParam(
                  FIdentifier("p1"),
                  FSimpleType(FIdentifier("Point"), None)
                ),
                FParam(
                  FIdentifier("p2"),
                  FSimpleType(FIdentifier("Point"), None)
                )
              )
            ),
            FSimpleType(FIdentifier("i32"), None)
          ),
          Seq(
            FAddition(
              FProj(FVar("p1"), Seq(FVar("x"))),
              FProj(FVar("p2"), Seq(FVar("x")))
            )
          )
        ),
        List(("Point", NameBind), ("&add", NameBind))
      ) ==> (
        List(
          ("x_point_sum", NameBind),
          ("Point", NameBind),
          ("&add", NameBind)
        ),
        List(
          Bind(
            "x_point_sum",
            TermAbbBind(
              TermFix(
                TermAbs(
                  "^x_point_sum",
                  TypeArrow(TypeVar(0, 2), TypeArrow(TypeVar(0, 2), TypeInt)),
                  TermAbs(
                    "p1",
                    TypeVar(1, 3),
                    TermAbs(
                      "p2",
                      TypeVar(2, 4),
                      TermApp(
                        TermApp(TermVar(4, 5), TermProj(TermVar(1, 5), "x")),
                        TermProj(TermVar(0, 5), "x")
                      ),
                      Some(TypeInt)
                    )
                  )
                )
              )
            )
          )
        )
      )
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
              None,
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
        ),
        List(("&add", NameBind), ("&sub", NameBind))
      ) ==> (List(
        ("fib", NameBind),
        ("&add", NameBind),
        ("&sub", NameBind)
      ), List(
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
                                TermApp(
                                  TermVar(3, 6),
                                  TermApp(
                                    TermApp(TermVar(5, 6), TermVar(2, 6)),
                                    TermInt(1)
                                  )
                                ),
                                TermVar(0, 6)
                              ),
                              TermApp(
                                TermApp(TermVar(4, 6), TermVar(1, 6)),
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
              None,
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
      ) ==> (List(("zero_to_one", NameBind)),
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
            FIdentifier("get_or_else"),
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
                ),
                FParam(
                  FIdentifier("default"),
                  FSimpleType((FIdentifier("T")))
                )
              )
            ),
            FSimpleType(FIdentifier("T"), None)
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
                  Seq(FVar("v"))
                ),
                FCase(
                  Seq(FIdentifierPattern("None")),
                  None,
                  Seq(FVar("default"))
                )
              )
            )
          )
        ),
        List(
          ("Some", NameBind),
          ("Option", NameBind)
        ) // Built-in function.
      ) ==> (List(
        ("get_or_else", NameBind),
        ("Some", NameBind),
        ("Option", NameBind)
      ),
      List(
        Bind(
          "get_or_else",
          TermAbbBind(
            TermTAbs(
              "T",
              TermFix(
                TermAbs(
                  "^get_or_else",
                  TypeArrow(
                    TypeApp(TypeVar(2, 3), TypeVar(0, 3)),
                    TypeArrow(TypeVar(0, 3), TypeVar(0, 3))
                  ),
                  TermAbs(
                    "x",
                    TypeApp(TypeVar(3, 4), TypeVar(1, 4)),
                    TermAbs(
                      "default",
                      TypeVar(2, 5),
                      TermMatch(
                        TermVar(1, 6),
                        List(
                          (
                            PatternNode("Some", List("v")),
                            TermVar(0, 7)
                          ),
                          (PatternNode("None", List()), TermVar(0, 6))
                        )
                      ),
                      Some(TypeVar(3, 6))
                    )
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
                      None,
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
                TermApp(TermApp(TermVar(1, 3), TermInt(1)), TermInt(2)),
                TermApp(TermApp(TermVar(3, 4), TermVar(0, 4)), TermInt(1))
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
                  TermApp(TermApp(TermVar(3, 4), TermVar(0, 4)), TermInt(1)),
                  TermApp(TermApp(TermVar(3, 5), TermVar(1, 5)), TermVar(0, 5))
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
                  None,
                  Seq(
                    FAddition(FVar("x"), FInt(1))
                  )
                )
              )
            ),
            FApp(FVar("f"), None, Seq(Some(Seq(FInt(5)))))
          )
        ),
        List(("&add", NameBind)) // Built-in function.
      ) ==> (List(
        ("plus_one", NameBind),
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
                  TermApp(TermApp(TermVar(2, 3), TermVar(0, 3)), TermInt(1))
                ),
                TermApp(TermVar(0, 4), TermInt(5))
              ),
              Some(TypeInt)
            )
          )
        )
      ))
    }
    test(
      "desugar function with let expression with lambda expression using multiple params"
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
                  None,
                  Seq(
                    FAddition(FMultiplication(FVar("x"), FVar("y")), FVar("z"))
                  )
                )
              )
            ),
            FApp(FVar("g"), None, Seq(Some(Seq(FInt(1), FInt(2), FInt(3)))))
          )
        ),
        List(("&multiply", NameBind), ("&add", NameBind)) // Built-in function.
      ) ==> (List(
        ("compute_three", NameBind),
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
                          TermVar(5, 6),
                          TermApp(
                            TermApp(TermVar(4, 6), TermVar(2, 6)),
                            TermVar(1, 6)
                          )
                        ),
                        TermVar(0, 6)
                      )
                    )
                  )
                ),
                TermApp(
                  TermApp(TermApp(TermVar(0, 7), TermInt(1)), TermInt(2)),
                  TermInt(3)
                )
              ),
              Some(TypeInt)
            )
          )
        )
      ))
    }
    test("desugar function with call expression with inline lambda argument") {
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
              None,
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
                      None,
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
    test("desugar function with recursive lambda function") {
      desugar(
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
                  Some(
                    FSimpleType(
                      FIdentifier("List"),
                      Some(Seq(FSimpleType(FIdentifier("B"))))
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
        ),
        List(
          ("Cons", NameBind),
          ("Nil", NameBind),
          ("List", NameBind)
        )
      ) ==> (List(
        ("map", NameBind),
        ("Cons", NameBind),
        ("Nil", NameBind),
        ("List", NameBind)
      ),
      List(
        Bind(
          "map",
          TermAbbBind(
            TermTAbs(
              "A",
              TermTAbs(
                "B",
                TermFix(
                  TermAbs(
                    "^map",
                    TypeArrow(
                      TypeApp(TypeVar(4, 5), TypeVar(1, 5)),
                      TypeArrow(
                        TypeArrow(TypeVar(1, 5), TypeVar(0, 5)),
                        TypeApp(TypeVar(4, 5), TypeVar(0, 5))
                      )
                    ),
                    TermAbs(
                      "l",
                      TypeApp(TypeVar(5, 6), TypeVar(2, 6)),
                      TermAbs(
                        "f",
                        TypeArrow(TypeVar(3, 7), TypeVar(2, 7)),
                        TermLet(
                          "iter",
                          TermFix(
                            TermAbs(
                              "^iter",
                              TypeArrow(
                                TypeApp(TypeVar(7, 8), TypeVar(3, 8)),
                                TypeArrow(
                                  TypeApp(TypeVar(7, 8), TypeVar(4, 8)),
                                  TypeApp(TypeVar(7, 8), TypeVar(3, 8))
                                )
                              ),
                              TermClosure(
                                "acc",
                                Some(TypeApp(TypeVar(8, 9), TypeVar(4, 9))),
                                TermClosure(
                                  "l",
                                  Some(
                                    TypeApp(TypeVar(9, 10), TypeVar(6, 10))
                                  ),
                                  TermMatch(
                                    TermVar(0, 11),
                                    List(
                                      (
                                        PatternNode("Nil", List()),
                                        TermVar(1, 11)
                                      ),
                                      (
                                        PatternNode("Cons", List("h", "t")),
                                        TermApp(
                                          TermApp(
                                            TermVar(4, 13),
                                            TermApp(
                                              TermApp(
                                                TermTApp(
                                                  TermVar(10, 13),
                                                  TypeVar(8, 13)
                                                ),
                                                TermApp(
                                                  TermVar(5, 13),
                                                  TermVar(1, 13)
                                                )
                                              ),
                                              TermVar(3, 13)
                                            )
                                          ),
                                          TermVar(0, 13)
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          TermApp(
                            TermApp(
                              TermVar(0, 9),
                              TermTApp(TermVar(7, 9), TypeVar(4, 9))
                            ),
                            TermVar(2, 9)
                          )
                        ),
                        Some(TypeApp(TypeVar(8, 9), TypeVar(4, 9)))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ))
    }
    test("desugar type func decls") {
      desugar(
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
                      FSimpleType(FIdentifier("Point"), None)
                    )
                  )
                ),
                FSimpleType(FIdentifier("f32"), None)
              ),
              Seq(
                FSubtraction(
                  FProj(FVar("this"), Seq(FVar("x"))),
                  FProj(FVar("other"), Seq(FVar("x")))
                )
              )
            ),
            FFuncDecl(
              FFuncSig(
                FIdentifier("orElse"),
                Some(Seq(FTypeParam(FIdentifier("T"), None))),
                Some(
                  Seq(
                    FParam(
                      FIdentifier("t"),
                      FSimpleType(FIdentifier("T"), None)
                    )
                  )
                ),
                FSimpleType(
                  FIdentifier("Option"),
                  Some(Seq(FSimpleType(FIdentifier("T"), None)))
                )
              ),
              Seq(
                FMatch(
                  FEquality(FProj(FVar("this"), Seq(FVar("x"))), FInt(0)),
                  Seq(
                    FCase(Seq(FBool(true)), None, List(FVar("t"))),
                    FCase(Seq(FBool(false)), None, List(FVar("None")))
                  )
                )
              )
            )
          )
        ),
        List(
          ("Point", NameBind),
          ("Some", NameBind),
          ("None", NameBind),
          ("Option", NameBind),
          ("&sub", NameBind),
          ("&eq", NameBind)
        )
      ) ==>
        (List(
          ("!orElse#Point", NameBind),
          ("!x_diff#Point", NameBind),
          ("Point", NameBind),
          ("Some", NameBind),
          ("None", NameBind),
          ("Option", NameBind),
          ("&sub", NameBind),
          ("&eq", NameBind)
        ), List(
          Bind(
            "!x_diff#Point",
            TermAbbBind(
              TermFix(
                TermAbs(
                  "^!x_diff#Point",
                  TypeArrow(TypeVar(0, 6), TypeArrow(TypeVar(0, 6), TypeFloat)),
                  TermAbs(
                    "this",
                    TypeVar(1, 7),
                    TermAbs(
                      "other",
                      TypeVar(2, 8),
                      TermApp(
                        TermApp(TermVar(7, 9), TermProj(TermVar(1, 9), "x")),
                        TermProj(TermVar(0, 9), "x")
                      ),
                      Some(TypeFloat)
                    )
                  )
                )
              )
            )
          ),
          Bind(
            "!orElse#Point",
            TermAbbBind(
              TermTAbs(
                "T",
                TermFix(
                  TermAbs(
                    "^!orElse#Point",
                    TypeArrow(
                      TypeVar(2, 8),
                      TypeArrow(
                        TypeVar(0, 8),
                        TypeApp(TypeVar(5, 8), TypeVar(0, 8))
                      )
                    ),
                    TermAbs(
                      "this",
                      TypeVar(3, 9),
                      TermAbs(
                        "t",
                        TypeVar(2, 10),
                        TermMatch(
                          TermApp(
                            TermApp(
                              TermVar(10, 11),
                              TermProj(TermVar(1, 11), "x")
                            ),
                            TermInt(0)
                          ),
                          List(
                            (TermTrue, TermVar(0, 11)),
                            (TermFalse, TermVar(7, 11))
                          )
                        ),
                        Some(TypeApp(TypeVar(8, 11), TypeVar(3, 11)))
                      )
                    )
                  )
                )
              )
            )
          )
        ))
    }
    test("desugar type func decls with type parameters") {
      desugar(
        FTypeFuncDecls(
          FIdentifier("Option"),
          Some(Seq(FTypeParam(FIdentifier("A"), None))),
          Seq(
            FFuncDecl(
              FFuncSig(
                FIdentifier("map"),
                Some(Seq(FTypeParam(FIdentifier("B"), None))),
                Some(
                  Seq(
                    FParam(
                      FIdentifier("f"),
                      FFuncType(
                        List(FSimpleType(FIdentifier("A"), None)),
                        FSimpleType(FIdentifier("B"), None)
                      )
                    )
                  )
                ),
                FSimpleType(
                  FIdentifier("Option"),
                  Some(Seq(FSimpleType(FIdentifier("B"), None)))
                )
              ),
              Seq(
                FMatch(
                  FVar("this"),
                  Seq(
                    FCase(
                      Seq(
                        FVariantOrRecordPattern(
                          FIdentifier("Some"),
                          Seq(FIdentifierPattern("a", None))
                        )
                      ),
                      None,
                      List(
                        FApp(
                          FVar("Some"),
                          Some(Seq(FSimpleType(FIdentifier("B"), None))),
                          Seq(
                            Some(
                              Seq(
                                FApp(FVar("f"), None, Seq(Some(Seq(FVar("a")))))
                              )
                            )
                          )
                        )
                      )
                    ),
                    FCase(
                      Seq(FWildCardPattern),
                      None,
                      List(
                        FApp(
                          FVar("None"),
                          Some(Seq(FSimpleType(FIdentifier("B"), None))),
                          Seq(None)
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
          ("Some", NameBind),
          ("None", NameBind),
          ("Option", NameBind)
        ) // Built-in function.
      ) ==> (List(
        ("!map#Option", NameBind),
        ("Some", NameBind),
        ("None", NameBind),
        ("Option", NameBind)
      ), List(
        Bind(
          "!map#Option",
          TermAbbBind(
            TermTAbs(
              "A",
              TermTAbs(
                "B",
                TermFix(
                  TermAbs(
                    "^!map#Option",
                    TypeArrow(
                      TypeApp(TypeVar(4, 5), TypeVar(1, 5)),
                      TypeArrow(
                        TypeArrow(TypeVar(1, 5), TypeVar(0, 5)),
                        TypeApp(TypeVar(4, 5), TypeVar(0, 5))
                      )
                    ),
                    TermAbs(
                      "this",
                      TypeApp(TypeVar(5, 6), TypeVar(2, 6)),
                      TermAbs(
                        "f",
                        TypeArrow(TypeVar(3, 7), TypeVar(2, 7)),
                        TermMatch(
                          TermVar(1, 8),
                          List(
                            (
                              PatternNode("Some", List("a")),
                              TermApp(
                                TermTApp(TermVar(6, 9), TypeVar(4, 9)),
                                TermApp(TermVar(1, 9), TermVar(0, 9))
                              )
                            ),
                            (
                              PatternDefault,
                              TermTApp(TermVar(6, 8), TypeVar(3, 8))
                            )
                          )
                        ),
                        Some(TypeApp(TypeVar(7, 8), TypeVar(3, 8)))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ))
    }
    test("desugar function with method expression") {
      desugar(
        FFuncDecl(
          FFuncSig(
            FIdentifier("incr"),
            None,
            Some(
              Seq(
                FParam(
                  FIdentifier("v"),
                  FSimpleType(
                    FIdentifier("Option"),
                    Some(Seq(FSimpleType(FIdentifier("i32"))))
                  )
                )
              )
            ),
            FSimpleType(
              FIdentifier("Option"),
              Some(Seq(FSimpleType(FIdentifier("i32"))))
            )
          ),
          Seq(
            FMethodApp(
              FProj(FVar("v"), Vector(FVar("map"))),
              Some(
                Seq(
                  FSimpleType(FIdentifier("i32"), None),
                  FSimpleType(FIdentifier("i32"), None)
                )
              ),
              Vector(
                Some(
                  Vector(
                    FAbs(
                      List(
                        FBinding(
                          FIdentifier("a"),
                          Some(FSimpleType(FIdentifier("i32"), None))
                        )
                      ),
                      None,
                      List(FAddition(FVar("a"), FInt(1)))
                    )
                  )
                )
              )
            )
          )
        ),
        List(
          ("&add", NameBind),
          ("!map#Option", NameBind),
          ("Some", NameBind),
          ("None", NameBind),
          ("Option", NameBind)
        )
      ) ==> (List(
        ("incr", NameBind),
        ("&add", NameBind),
        ("!map#Option", NameBind),
        ("Some", NameBind),
        ("None", NameBind),
        ("Option", NameBind)
      ), List(
        Bind(
          "incr",
          TermAbbBind(
            TermFix(
              TermAbs(
                "^incr",
                TypeArrow(
                  TypeApp(TypeVar(4, 5), TypeInt),
                  TypeApp(TypeVar(4, 5), TypeInt)
                ),
                TermAbs(
                  "v",
                  TypeApp(TypeVar(5, 6), TypeInt),
                  TermApp(
                    TermApp(
                      TermTApp(
                        TermTApp(TermMethodProj(TermVar(0, 7), "map"), TypeInt),
                        TypeInt
                      ),
                      TermVar(0, 7)
                    ),
                    TermClosure(
                      "a",
                      Some(TypeInt),
                      TermApp(TermApp(TermVar(3, 8), TermVar(0, 8)), TermInt(1))
                    )
                  ),
                  Some(TypeApp(TypeVar(7, 8), TypeInt))
                )
              )
            )
          )
        )
      ))
    }
    // TODO: Learn how to represent traits (type classes) in the lambda calculus.
    // test("desugar trait decl") {}
    // test("desugar trait instance decl") {}
  }

  def desugar(d: FDecl, ctx: Context = Context.emptyContext): (Context, List[Bind]) = {
    Desugar.bind(d).run(ctx).value
  }
}
