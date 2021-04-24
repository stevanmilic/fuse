package core

import cats.data.State
import core.Context._
import parser.FuseParser._
import parser.FuseLexicalParser._
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
      ) ==> (List(("bool", NameBind)),
      Bind(
        "bool",
        TypeAbbBind(TypeVariant(List(("true", TypeUnit), ("false", TypeUnit))))
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
      ) ==> (List(("OptionInt", NameBind)),
      Bind(
        "OptionInt",
        TypeAbbBind(
          TypeVariant(
            List(
              ("None", TypeUnit),
              ("Some", TypeRecord(List(("1", TypeInt))))
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
      ) ==> (List(("DataPoint", NameBind)),
      Bind(
        "DataPoint",
        TypeAbbBind(
          TypeVariant(
            List(
              ("2DPoint", TypeRecord(List(("x", TypeFloat), ("y", TypeFloat)))),
              (
                "3DPoint",
                TypeRecord(
                  List(("x", TypeFloat), ("y", TypeFloat), ("z", TypeFloat))
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
      ) ==> (List(("Point", NameBind)),
      Bind(
        "Point",
        TypeAbbBind(TypeRecord(List(("x", TypeInt), ("y", TypeInt))))
      ))
    }
    test("desugar tuple record type") {
      desugar(
        FTupleTypeDecl(
          FIdentifier("Pair"),
          None,
          Seq(FSimpleType(FIdentifier("i32")), FSimpleType(FIdentifier("str")))
        )
      ) ==> (List(("Pair", NameBind)),
      Bind(
        "Pair",
        TypeAbbBind(TypeRecord(List(("1", TypeInt), ("2", TypeString))))
      ))
    }
    test("desugar recursive variant type") {
      desugar(
        FVariantTypeDecl(
          FIdentifier("ListInt"),
          None,
          Seq(
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
            ),
            FVariantTypeValue(FIdentifier("Nil"))
          )
        )
      ) ==>
        (List(("ListInt", NameBind), ("@ListInt", NameBind)),
        Bind(
          "ListInt",
          TypeAbbBind(
            TypeRec(
              "@ListInt",
              KindStar,
              TypeVariant(
                List(
                  (
                    "Cons",
                    TypeRecord(List(("head", TypeInt), ("t", TypeVar(0, 1))))
                  ),
                  ("Nil", TypeUnit)
                )
              )
            )
          )
        ))
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
      ) ==> (List(("Pair2", NameBind), ("@Pair2", NameBind)),
      Bind(
        "Pair2",
        TypeAbbBind(
          TypeRec(
            "@Pair2",
            KindStar,
            TypeRecord(List(("1", TypeInt), ("2", TypeVar(0, 1))))
          )
        )
      ))
    }
    test("is identifier in types") {
      test("for simple type") {
        Desugar.isIdentifierInTypes(
          "List",
          Seq(FSimpleType(FIdentifier("List"), None))
        ) ==> true
      }
      test("for func type") {
        Desugar.isIdentifierInTypes(
          "TreeInt",
          Seq(
            FFuncType(
              Seq(FSimpleType(FIdentifier("i32"))),
              FSimpleType(
                FIdentifier("List"),
                Some(Seq(FSimpleType(FIdentifier("TreeInt"))))
              )
            )
          )
        ) ==> true
      }
      test("for different simple type") {
        Desugar.isIdentifierInTypes(
          "List",
          Seq(FSimpleType(FIdentifier("i32")))
        ) ==> false
      }
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
      Bind(
        "IntToString",
        TypeAbbBind(TypeArrow(TypeInt, TypeString))
      ))
    }
    test("desugar type recursive function abbreviation") {
      desugar(
        FTypeAlias(
          FIdentifier("Hungry"),
          None,
          FFuncType(
            Seq(FSimpleType(FIdentifier("i32"))),
            FSimpleType(FIdentifier("Hungry"))
          )
        )
      ) ==> (List(("Hungry", NameBind), ("@Hungry", NameBind)),
      Bind(
        "Hungry",
        TypeAbbBind(
          TypeRec("@Hungry", KindStar, TypeArrow(TypeInt, TypeVar(0, 1)))
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
        List(("Point", NameBind), ("T", NameBind)),
        Bind(
          "Point",
          TypeAbbBind(
            TypeAbs(
              "T",
              TypeRecord(List(("x", TypeVar(0, 1)), ("y", TypeVar(0, 1))))
            )
          )
        ),
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
      ) ==> (List(("Option", NameBind), ("T", NameBind)),
      Bind(
        "Option",
        TypeAbbBind(
          TypeAbs(
            "T",
            TypeVariant(
              List(
                ("None", TypeUnit),
                ("Some", TypeRecord(List(("1", TypeVar(0, 1)))))
              )
            )
          )
        )
      ))
    }
    test("desugar algebraic data type -> recursive + parametric") {
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
      ) ==> (List(("List", NameBind), ("@List", NameBind), ("A", NameBind)),
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
      Bind(
        "Function0",
        TypeAbbBind(TypeAbs("T", TypeArrow(TypeUnit, TypeVar(0, 1))))
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
      Bind(
        "Function1",
        TypeAbbBind(
          TypeAbs("A", TypeAbs("B", TypeArrow(TypeVar(1, 2), TypeVar(0, 2))))
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
      ) ==> (List(("CurriedInt", NameBind)), Bind(
        "CurriedInt",
        TypeAbbBind(TypeArrow(TypeInt, TypeArrow(TypeInt, TypeInt)))
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
          ("DataPoint", NameBind),
          ("@DataPoint", NameBind),
          ("C", NameBind),
          ("B", NameBind),
          ("A", NameBind)
        ),
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
      )
    }
    test("desugar func decl") {}
    test("desugar type func decls") {}
    // TODO: Learn how to represent traits (type classes) in the lambda calculus.
    // test("desugar trait decl") {}
    // test("desugar trait instance decl") {}
  }

  def desugar(d: FDecl): (Context, Bind) = {
    Desugar.bind(d).run(Context.empty).value
  }
}
