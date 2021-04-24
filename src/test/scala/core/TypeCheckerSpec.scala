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
      ) ==> Right(List())
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
      ) ==> Right(List())
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