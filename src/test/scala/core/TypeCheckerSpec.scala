package core

import utest._
import cats.data.State
import core.Context._

object TypeCheckerSpec extends TestSuite {
  val tests = Tests {

    test("type check function with 1 * 2") {
      typeCheck(
        List(
          Bind(
            "&multiply",
            TermAbbBind(
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
            )
          ),
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
        )
      ) ==> Right(
        List(
          ("&multiply", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))),
          ("one_and_two", Right(TypeArrow(TypeUnit, TypeInt)))
        )
      )
    }
    test("type check function with additive expression") {
      typeCheck(
        List(
          Bind(
            "&add",
            TermAbbBind(
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
            )
          ),
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
        )
      ) ==> Right(
        List(
          ("&add", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))),
          ("sum", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt))))
        )
      )
    }
    test("type check recursive function") {
      typeCheck(
        List(
          Bind(
            "&sub",
            TermAbbBind(
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
            )
          ),
          Bind(
            "&add",
            TermAbbBind(
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
            )
          ),
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
        )
      ) ==> Right(
        List(
          ("&sub", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))),
          ("&add", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))),
          (
            "fib",
            Right(
              TypeArrow(
                TypeInt,
                TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt))
              )
            )
          )
        )
      )
    }
    test("type check function with record data constructor") {
      typeCheck(
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
          ),
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
        )
      ) ==> Right(
        List(
          ("Point", Left(KindStar)),
          (
            "%Point",
            Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeVar(0, 1))))
          ),
          ("zero_point", Right(TypeArrow(TypeUnit, TypeVar(1, 2))))
        )
      )
    }
    test("type check function with variant data constructor") {
      typeCheck(
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
        )
      ) ==> Right(
        List(
          ("bool", Left(KindStar)),
          ("true", Right(TypeVar(0, 1))),
          ("false", Right(TypeVar(1, 2)))
        )
      )
    }
    test("type check function with variant data constructor on record type") {
      typeCheck(
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
                        List(
                          ("x", TypeFloat),
                          ("y", TypeFloat),
                          ("z", TypeFloat)
                        )
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
        )
      ) ==> Right(
        List(
          ("DataPoint", Left(KindStar)),
          (
            "2DPoint",
            Right(TypeArrow(TypeFloat, TypeArrow(TypeFloat, TypeVar(0, 1))))
          ),
          (
            "3DPoint",
            Right(
              TypeArrow(
                TypeFloat,
                TypeArrow(TypeFloat, TypeArrow(TypeFloat, TypeVar(1, 2)))
              )
            )
          )
        )
      )
    }
    test("type check function with parametric record data constructor") {
      typeCheck(
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
      ) ==> Right(
        List(
          ("Point", Left(KindArrow(KindStar, KindStar))),
          (
            "%Point",
            Right(
              TypeAll(
                "T",
                KindStar,
                TypeArrow(
                  TypeVar(0, 2),
                  TypeArrow(
                    TypeVar(0, 2),
                    TypeApp(TypeVar(1, 2), TypeVar(0, 2))
                  )
                )
              )
            )
          )
        )
      )
    }
    test("type check function with parametric variant data constructor") {
      typeCheck(
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
                  TermTag(
                    "None",
                    TermUnit,
                    TypeApp(TypeVar(1, 2), TypeVar(0, 2))
                  )
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
        )
      ) ==> Right(
        List(
          ("Option", Left(KindArrow(KindStar, KindStar))),
          (
            "None",
            Right(TypeAll("T", KindStar, TypeApp(TypeVar(1, 2), TypeVar(0, 2))))
          ),
          (
            "Some",
            Right(
              TypeAll(
                "T",
                KindStar,
                TypeArrow(TypeVar(0, 3), TypeApp(TypeVar(2, 3), TypeVar(0, 3)))
              )
            )
          )
        )
      )
    }
    test("type check algebraic data type") {
      typeCheck(
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
        )
      ) ==> Right(
        List(
          ("List", Left(KindArrow(KindStar, KindStar))),
          (
            "Nil",
            Right(TypeAll("A", KindStar, TypeApp(TypeVar(1, 2), TypeVar(0, 2))))
          ),
          (
            "Cons",
            Right(
              TypeAll(
                "A",
                KindStar,
                TypeArrow(
                  TypeVar(0, 3),
                  TypeArrow(
                    TypeApp(TypeVar(2, 3), TypeVar(0, 3)),
                    TypeApp(TypeVar(2, 3), TypeVar(0, 3))
                  )
                )
              )
            )
          )
        )
      )
    }
    test("type check function with variant data constructor") {
      typeCheck(
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
          ),
          Bind(
            "some_value",
            TermAbbBind(
              TermFix(
                TermAbs(
                  "^some_value",
                  TypeArrow(TypeInt, TypeVar(2, 3)),
                  TermAbs(
                    "x",
                    TypeInt,
                    TermApp(TermVar(2, 5), TermVar(0, 5)),
                    Some(TypeVar(4, 5))
                  )
                )
              )
            )
          )
        )
      ) ==> Right(
        List(
          ("OptionInt", Left(KindStar)),
          ("None", Right(TypeVar(0, 1))),
          ("Some", Right(TypeArrow(TypeInt, TypeVar(1, 2)))),
          ("some_value", Right(TypeArrow(TypeInt, TypeVar(2, 3))))
        )
      )
    }
    test("type check function with match expression with literals") {
      typeCheck(
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
        )
      ) ==> Right(List(("zero_to_one", Right(TypeArrow(TypeInt, TypeInt)))))
    }
    test("type check function with match expression with with adt unfolding") {
      typeCheck(
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
            "Some",
            TermAbbBind(
              TermTAbs(
                "T",
                TermAbs(
                  "#1",
                  TypeVar(0, 2),
                  TermApp(
                    TermFold(TypeApp(TypeVar(2, 3), TypeVar(1, 3))),
                    TermTag(
                      "Some",
                      TermRecord(List(("1", TermVar(0, 3)))),
                      TypeApp(TypeVar(2, 3), TypeVar(1, 3))
                    )
                  )
                )
              )
            )
          ),
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
        )
      ) ==> Right(
        List(
          ("Option", Left(KindArrow(KindStar, KindStar))),
          (
            "Some",
            Right(
              TypeAll(
                "T",
                KindStar,
                TypeArrow(TypeVar(0, 2), TypeApp(TypeVar(1, 2), TypeVar(0, 2)))
              )
            )
          ),
          (
            "get_or_else",
            Right(
              TypeAll(
                "T",
                KindStar,
                TypeArrow(
                  TypeApp(TypeVar(2, 3), TypeVar(0, 3)),
                  TypeArrow(TypeVar(0, 3), TypeVar(0, 3))
                )
              )
            )
          )
        )
      )
    }
    test(
      "type check recursive function with match expression with adt structure"
    ) {
      typeCheck(
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
          ),
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
        )
      ) ==> Right(
        List(
          ("List", Left(KindArrow(KindStar, KindStar))),
          (
            "Nil",
            Right(TypeAll("A", KindStar, TypeApp(TypeVar(1, 2), TypeVar(0, 2))))
          ),
          (
            "Cons",
            Right(
              TypeAll(
                "A",
                KindStar,
                TypeArrow(
                  TypeVar(0, 3),
                  TypeArrow(
                    TypeApp(TypeVar(2, 3), TypeVar(0, 3)),
                    TypeApp(TypeVar(2, 3), TypeVar(0, 3))
                  )
                )
              )
            )
          ),
          (
            "map",
            Right(
              TypeAll(
                "A",
                KindStar,
                TypeAll(
                  "B",
                  KindStar,
                  TypeArrow(
                    TypeApp(TypeVar(4, 5), TypeVar(1, 5)),
                    TypeArrow(
                      TypeArrow(TypeVar(1, 5), TypeVar(0, 5)),
                      TypeApp(TypeVar(4, 5), TypeVar(0, 5))
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
    test("type check function with let expression") {
      typeCheck(
        List(
          Bind(
            "&add",
            TermAbbBind(
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
            )
          ),
          Bind(
            "&multiply",
            TermAbbBind(
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
            )
          ),
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
        )
      ) ==> Right(
        List(
          ("&add", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))),
          ("&multiply", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))),
          ("one_and_two_plus_one", Right(TypeArrow(TypeUnit, TypeInt)))
        )
      )

    }
    test("type check function with sequence of let expressions") {
      typeCheck(
        List(
          Bind(
            "&add",
            TermAbbBind(
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
            )
          ),
          Bind(
            "&multiply",
            TermAbbBind(
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
            )
          ),
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
                    TermApp(
                      TermApp(TermVar(3, 5), TermVar(1, 5)),
                      TermVar(0, 5)
                    )
                  )
                ),
                Some(TypeInt)
              )
            )
          )
        )
      ) ==> Right(
        List(
          ("&add", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))),
          ("&multiply", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))),
          ("compute_z", Right(TypeArrow(TypeUnit, TypeInt)))
        )
      )
    }
    test("type check function with let expression with lambda expression") {
      typeCheck(
        List(
          Bind(
            "&add",
            TermAbbBind(
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
            )
          ),
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
        )
      ) ==> Right(
        List(
          ("&add", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))),
          ("plus_one", Right(TypeArrow(TypeUnit, TypeInt)))
        )
      )
    }
    test(
      "type check function with call expression with inline lambda argument"
    ) {
      typeCheck(
        List(
          Bind(
            "&add",
            TermAbbBind(
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
            )
          ),
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
            "map",
            TermAbbBind(
              TermBuiltin(
                TypeArrow(
                  TypeVar(0, 2),
                  TypeArrow(TypeArrow(TypeInt, TypeInt), TypeVar(0, 2))
                )
              )
            )
          ),
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
                        TermApp(
                          TermApp(TermVar(5, 6), TermVar(0, 6)),
                          TermInt(1)
                        )
                      )
                    ),
                    Some(TypeVar(4, 6))
                  )
                )
              )
            )
          )
        )
      ) ==> Right(
        List(
          ("&add", Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))),
          ("OptionInt", Left(KindStar)),
          (
            "map",
            Right(
              TypeArrow(
                TypeVar(0, 2),
                TypeArrow(TypeArrow(TypeInt, TypeInt), TypeVar(0, 2))
              )
            )
          ),
          ("option_and_one", Right(TypeArrow(TypeVar(1, 3), TypeVar(1, 3))))
        )
      )
    }
  }

  def typeCheck(
      binds: List[Bind]
  ): Either[Error, List[(String, Either[Kind, Type])]] = {
    TypeChecker
      .check(State.pure(binds))
      .map(
        _.map(v =>
          v.b match {
            case TypeVarBind(k)           => (v.i, Left(k))
            case VarBind(ty)              => (v.i, Right(ty))
            case TypeAbbBind(_, Some(k))  => (v.i, Left(k))
            case TermAbbBind(_, Some(ty)) => (v.i, Right(ty))
          }
        )
      )
      .value
      .runA(empty)
      .value
  }
}
