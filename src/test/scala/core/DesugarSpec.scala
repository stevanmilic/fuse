package core

import core.Context._
import parser.FuseParser._
import parser.FuseLexicalParser._
import parser.FuseTypesParser._
import utest._

object DesugarSpec extends TestSuite {
  val tests = Tests {
    test("desugar variant type") {
      Desugar.bind(
        FVariantTypeDecl(
          FIdentifier("bool"),
          None,
          Seq(
            FVariantTypeValue(FIdentifier("true")),
            FVariantTypeValue(FIdentifier("false"))
          )
        ),
        Context.empty
      ) ==> (Bind(
        "bool",
        TypeAbbBind(TypeVariant(List(("true", TypeUnit), ("false", TypeUnit))))
      ), List(("bool", NameBind)))
    }
    test("desugar variant with tuple record type") {
      Desugar.bind(
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
        ),
        Context.empty
      ) ==> (Bind(
        "OptionInt",
        TypeAbbBind(
          TypeVariant(
            List(
              ("None", TypeUnit),
              ("Some", TypeRecord(List(("1", TypeInt))))
            )
          )
        )
      ), List(("OptionInt", NameBind)))
    }
    test("desugar variant with record type") {
      Desugar.bind(
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
        ),
        Context.empty
      ) ==> (Bind(
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
      ), List(("DataPoint", NameBind)))
    }
    test("desugar record type") {
      Desugar.bind(
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
        ),
        Context.empty
      ) ==> (Bind(
        "Point",
        TypeAbbBind(TypeRecord(List(("x", TypeInt), ("y", TypeInt))))
      ), List(("Point", NameBind)))
    }
    test("desugar tuple record type") {
      Desugar.bind(
        FTupleTypeDecl(
          FIdentifier("Pair"),
          None,
          Seq(FSimpleType(FIdentifier("i32")), FSimpleType(FIdentifier("str")))
        ),
        Context.empty
      ) ==> (Bind(
        "Pair",
        TypeAbbBind(TypeRecord(List(("1", TypeInt), ("2", TypeString))))
      ), List(("Pair", NameBind)))
    }
    test("desugar recursive variant type") {
      Desugar
        .bind(
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
          ),
          Context.empty
        ) ==>
        (Bind(
          "ListInt",
          TypeAbbBind(
            TypeRec(
              "P",
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
        ),
        List(("ListInt", NameBind), ("P", NameBind)))
    }
    test("desugar recursive tuple record type") {
      Desugar.bind(
        FTupleTypeDecl(
          FIdentifier("Pair2"),
          None,
          Seq(
            FSimpleType(FIdentifier("i32")),
            FSimpleType(FIdentifier("Pair2"))
          )
        ),
        Context.empty
      ) ==> (Bind(
        "Pair2",
        TypeAbbBind(
          TypeRec("P", TypeRecord(List(("1", TypeInt), ("2", TypeVar(0, 1)))))
        )
      ),
      List(("Pair2", NameBind), ("P", NameBind)))
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
      Desugar.bind(
        FTypeAlias(
          FIdentifier("IntToString"),
          None,
          FFuncType(
            Seq(FSimpleType(FIdentifier("i32"))),
            FSimpleType(FIdentifier("str"))
          )
        ),
        Context.empty
      ) ==> (Bind(
        "IntToString",
        TypeAbbBind(TypeArrow(TypeInt, TypeString))
      ), List(("IntToString", NameBind)))
    }
    test("desugar type recursive function abbreviation") {
      Desugar.bind(
        FTypeAlias(
          FIdentifier("Hungry"),
          None,
          FFuncType(
            Seq(FSimpleType(FIdentifier("i32"))),
            FSimpleType(FIdentifier("Hungry"))
          )
        ),
        Context.empty
      ) ==> (Bind(
        "Hungry",
        TypeAbbBind(
          TypeRec("P", TypeArrow(TypeInt, TypeVar(0, 1)))
        )
      ), List(("Hungry", NameBind), ("P", NameBind)))
    }
    test("desugar parametric variant type") {}
    test("desugar record variant type") {}
    test("desugar algebraic data type -> recursive + parametric") {}
    test("desugar func decl") {}
    test("desugar type func decls") {}
    // TODO: Learn how to represent traits (type classes) in the lambda calculus.
    // test("desugar trait decl") {}
    // test("desugar trait instance decl") {}
  }
}
