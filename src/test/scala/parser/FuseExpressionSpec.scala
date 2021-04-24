package parser

import scala.util.{Failure, Success}
import utest._
import org.parboiled2._

object FuseExpressionSpec extends TestSuite {
  import FuseLexicalParser._
  import FuseTypesParser._
  import FuseExpressionParser._

  val tests = Tests {
    test("parse additive expression with identifiers") {
      parse("x + y") ==> FAddition(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse substraction expression") {
      parse("x - y") ==> FSubtraction(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse division expression") {
      parse("x / y") ==> FDivision(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse modulo expression") {
      parse("x % y") ==> FModulo(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse multiplication expression") {
      parse("3 * 2") ==> FMultiplication(
        FInt(3),
        FInt(2)
      )
    }
    test("parse equality expression") {
      parse("x == y") ==> FEquality(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse not equal expression") {
      parse("x != y") ==> FNotEquality(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse less than expression") {
      parse("x < y") ==> FLessThan(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse less than equal expression") {
      parse("x <= y") ==> FLessThanEqual(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse greater than expression") {
      parse("x > y") ==> FGreaterThan(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse greater than equal expression") {
      parse("x >= y") ==> FGreaterThanEqual(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse logical and expression") {
      parse("x && y") ==> FAnd(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse logical or operation") {
      parse("x || y") ==> FOr(
        FVar("x"),
        FVar("y")
      )
    }
    test("parse addition and multiplication") {
      parse("3 * 2 + 1") ==> FAddition(
        FMultiplication(
          FInt(3),
          FInt(2)
        ),
        FInt(1)
      )
    }
    test("parse simple call expressions") {
      parse("sum(3, 2)") ==> FApp(
        FVar("sum"),
        Seq(Some(Seq(FInt(3), FInt(2))))
      )
    }
    test("parse dot accessor in multiplication expression") {
      parse("this.radius * this.radius * 3.14") ==> FMultiplication(
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
    }
    test("parse call expressions with  multiple arguments") {
      parse("l.fold_left(0)(sum)") ==> FApp(
        FProj(
          FVar("l"),
          Seq(FVar("fold_left"))
        ),
        Seq(
          Some(Seq((FInt(0)))),
          Some(Seq(FVar("sum")))
        )
      )
    }
    test("parse call expression with lambda argument") {
      parse("l.map(a => a + 1)") ==> FApp(
        FProj(
          FVar("l"),
          Seq(FVar("map"))
        ),
        Seq(
          Some(
            Seq(
              FAbs(
                Seq(FBinding(FIdentifier("a"))),
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
    }
    test("parse call expression with lambda argument with inline block") {
      parse("l.map(a => {\n a + 1\n })") ==> FApp(
        FProj(
          FVar("l"),
          Seq(FVar("map"))
        ),
        Seq(
          Some(
            Seq(
              FAbs(
                Seq(FBinding(FIdentifier("a"))),
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
    }
    test("parse let expression") {
      parse("let x = 5 + 3") ==> FLetExpr(
        FIdentifier("x"),
        None,
        Seq(FAddition(FInt(5), FInt(3)))
      )
    }
    test("parse let expression with block inline expression") {
      parse("let z = {\n let t = x => x + 1\n t(5) + t(6)\n }") ==> FLetExpr(
        FIdentifier("z"),
        None,
        Seq(
          FLetExpr(
            FIdentifier("t"),
            None,
            Seq(
              FAbs(
                Seq(FBinding(FIdentifier("x"))),
                Seq(
                  FAddition(
                    FVar("x"),
                    FInt(1)
                  )
                )
              )
            )
          ),
          FAddition(
            FApp(
              FVar("t"),
              Seq(Some(Seq(FInt(5))))
            ),
            FApp(
              FVar("t"),
              Seq(Some(Seq(FInt(6))))
            )
          )
        )
      )
    }
    test("parse let expression with annotations") {
      parse("let b: i32 = 5") ==> FLetExpr(
        FIdentifier("b"),
        Some(FSimpleType(FIdentifier("i32"))),
        Seq(FInt(5))
      )
    }
    test("parse lambda expression") {
      parse("(a, b) => a + b") ==> FAbs(
        Seq(
          FBinding(FIdentifier("a")),
          FBinding(FIdentifier("b"))
        ),
        Seq(
          FAddition(
            FVar("a"),
            FVar("b")
          )
        )
      )
    }
    test("parse lambda expression with block inline expression") {
      parse("(f, g, v) => {\n let fv = f(v)\n g(fv)\n }") ==> FAbs(
        Seq(
          FBinding(FIdentifier("f")),
          FBinding(FIdentifier("g")),
          FBinding(FIdentifier("v"))
        ),
        Seq(
          FLetExpr(
            FIdentifier("fv"),
            None,
            Seq(
              FApp(
                FVar("f"),
                Seq(Some(Seq(FVar("v"))))
              )
            )
          ),
          FApp(
            FVar("g"),
            Seq(Some(Seq(FVar("fv"))))
          )
        )
      )
    }
    test("parse nested lambda expression") {
      parse("(a) => (b) => a + b") ==> FAbs(
        Seq(FBinding(FIdentifier("a"))),
        Seq(
          FAbs(
            Seq(FBinding(FIdentifier("b"))),
            Seq(
              FAddition(
                FVar("a"),
                FVar("b")
              )
            )
          )
        )
      )
    }
    test("parse lambda expression with annotations") {
      parse("(a: i32) => a + 1") ==> FAbs(
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
    }
    test("parse lambda expression with single param and no paranthesis") {
      parse("a => a + 1") ==> FAbs(
        Seq(FBinding(FIdentifier("a"))),
        Seq(
          FAddition(
            FVar("a"),
            FInt(1)
          )
        )
      )
    }
    test("parse let with lambda expression") {
      parse("let f = (a) => a + 1") ==> FLetExpr(
        FIdentifier("f"),
        None,
        Seq(
          FAbs(
            Seq(FBinding(FIdentifier("a"))),
            Seq(
              FAddition(FVar("a"), FInt(1))
            )
          )
        )
      )
    }
    test("parse match expression with deconstructing sum and struct types") {
      parse("match m:\n Some(v) => v\n None => b") ==> FMatch(
        FVar("m"),
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
            Seq(FVar("b"))
          )
        )
      )
    }
    test("parse match expression with literals") {
      parse("match x:\n 1 => a\n 2 => b\n _ => c") ==> FMatch(
        FVar("x"),
        Seq(
          FCase(
            Seq(FInt(1)),
            None,
            Seq(FVar("a"))
          ),
          FCase(Seq(FInt(2)), None, Seq(FVar("b"))),
          FCase(
            Seq(FWildCardPattern),
            None,
            Seq(FVar("c"))
          )
        )
      )
    }
    test("parse match expression with deconstructing tuples") {
      parse("match t:\n (x, y) => x + y") ==> FMatch(
        FVar("t"),
        Seq(
          FCase(
            Seq(
              FTuplePattern(
                Seq(FIdentifierPattern("x"), FIdentifierPattern("y"))
              )
            ),
            None,
            Seq(
              FAddition(
                FVar("x"),
                FVar("y")
              )
            )
          )
        )
      )
    }
    test("parse match expression with if guard") {
      parse("match m:\n Some(x) if x > 0 => x") ==> FMatch(
        FVar("m"),
        Seq(
          FCase(
            Seq(
              FVariantOrRecordPattern(
                FIdentifier("Some"),
                Seq(FIdentifierPattern("x"))
              )
            ),
            Some(
              FGreaterThan(
                FVar("x"),
                FInt(0)
              )
            ),
            Seq(FVar("x"))
          )
        )
      )
    }
    test(
      "parse match expression with multiple conditional patterns in a case clause"
    ) {
      parse("match S(1, 2):\n S(1, _) | S(_, 2) => 3") ==> FMatch(
        FApp(
          FVar("S"),
          Seq(Some(Seq(FInt(1), FInt(2))))
        ),
        Seq(
          FCase(
            Seq(
              FVariantOrRecordPattern(
                FIdentifier("S"),
                Seq(FInt(1), FWildCardPattern)
              ),
              FVariantOrRecordPattern(
                FIdentifier("S"),
                Seq(FWildCardPattern, FInt(2))
              )
            ),
            None,
            Seq(FInt(3))
          )
        )
      )
    }
    test("parse match expression with binding") {
      parse("match e:\n f @ Left(_) => f\n Right(v) => v") ==> FMatch(
        FVar("e"),
        Seq(
          FCase(
            Seq(
              FIdentifierPattern(
                "f",
                Some(
                  FVariantOrRecordPattern(
                    FIdentifier("Left"),
                    Seq(FWildCardPattern)
                  )
                )
              )
            ),
            None,
            Seq(FVar("f"))
          ),
          FCase(
            Seq(
              FVariantOrRecordPattern(
                FIdentifier("Right"),
                Seq(FIdentifierPattern("v"))
              )
            ),
            None,
            Seq(FVar("v"))
          )
        )
      )
    }

  }

  def parse(s: String): FExpr = {
    val parser = new FuseExpressionParser(s)
    parser.Expr.run() match {
      case Success(result) => result
      case Failure(e: ParseError) =>
        sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }

  def runParser(s: String) = {
    val parser = new FuseExpressionParser(s)
    parser.Expr.run()
  }
}
