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
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt))),
            )
          ),
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
              TermBuiltin(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt))),
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
                    TermFold(TypeVar(2, 4)),
                    TermRecord(List(("x", TermVar(1, 4)), ("y", TermVar(0, 4))))
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
                TermApp(TermApp(TermVar(0, 1), TermInt(0)), TermInt(0)),
                Some(TypeVar(0, 1))
              )
            )
          )
        )
      ) ==> Right(
        List(
          ("Point", Left(KindStar)),
          (
            "%Point",
            Right(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeVar(0, 2))))
          ),
          ("zero_point", Right(TypeArrow(TypeUnit, TypeVar(0, 2))))
        )
      )
    }
    test("type check algebraic datat type") {
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
      )) ==> Right(List())
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
