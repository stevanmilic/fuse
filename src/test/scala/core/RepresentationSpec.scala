package core

import cats.data.State
import core.Context.*
import utest.*

import Representation.*

object RepresentationSpec extends TestSuite {
  val tests = Tests {
    test("representation of type arrow") {
      typeRepresentation(
        TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt))
      ) ==> Right("i32 -> i32 -> i32")
    }
    test("representation of type all") {
      typeRepresentation(
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
        ),
        List(("Nil", NameBind), ("List", NameBind))
      ) ==> Right("[A::*] A -> List[A] -> List[A]")
    }
    test("representation of type abstraction") {
      typeRepresentation(
        TypeAbs(
          "T",
          TypeRec(
            "@Point",
            KindArrow(KindStar, KindStar),
            TypeRecord(List(("x", TypeVar(1, 2)), ("y", TypeVar(1, 2))))
          )
        )
      ) ==> Right("λ T. {x: T, y: T}")
    }
    test("representation of type variant") {
      typeRepresentation(
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
      ) ==> Right("λ A. Nil() | Cons{1: A, 2: List[A]}")
    }
    test("representation of kinds") {
      kindToString(
        KindArrow(KindStar, KindArrow(KindStar, KindStar))
      ) ==> "* => * => *"
    }
  }

  def typeRepresentation(t: Type, c: Context = emptyContext): Either[Error, String] =
    typeToString(t, true).value.runA(c).value
}
