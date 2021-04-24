package fuse

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
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse substraction expression") {
      parse("x - y") ==> FSubtraction(
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse division expression") {
      parse("x / y") ==> FDivision(
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse modulo expression") {
      parse("x % y") ==> FModulo(
        FExprIdentifier("x"),
        FExprIdentifier("y")
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
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse not equal expression") {
      parse("x != y") ==> FNotEquality(
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse less than expression") {
      parse("x < y") ==> FLessThan(
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse less than equal expression") {
      parse("x <= y") ==> FLessThanEqual(
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse greater than expression") {
      parse("x > y") ==> FGreaterThan(
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse greater than equal expression") {
      parse("x >= y") ==> FGreaterThanEqual(
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse logical and expression") {
      parse("x && y") ==> FAnd(
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse logical or operation") {
      parse("x || y") ==> FOr(
        FExprIdentifier("x"),
        FExprIdentifier("y")
      )
    }
    test("parse simple call expressions") {
      parse("sum(3, 2)") ==> FCallExpr(
        FExprIdentifier("sum"),
        Some(Seq(FInt(3), FInt(2)))
      )
    }
    test("parse dot accessor in multiplication expression") {
      parse("this.radius * this.radius * 3.14") ==> FMultiplication(
        FMultiplication(
          FMemberExpr(
            FExprIdentifier("this"),
            Seq(FExprIdentifier("radius"))
          ),
          FMemberExpr(
            FExprIdentifier("this"),
            Seq(FExprIdentifier("radius"))
          )
        ),
        FFloat(3.14.toFloat)
      )
    }
    test("parse call expressions with dot accessor and multiple arguments") {
      parse("l.fold_left(0)(sum)") ==> FCallExpr(
        FMemberExpr(
          FExprIdentifier("l"),
          Seq(FExprIdentifier("fold_left"))
        ),
        Some(Seq((FInt(0)))),
        Seq(
          Left(Some(Seq(FExprIdentifier("sum"))))
        )
      )
    }
    test("parse call expression with lambda argument") {
      parse("l.map(a => a + 1)") ==> FCallExpr(
        FMemberExpr(
          FExprIdentifier("l"),
          Seq(FExprIdentifier("map"))
        ),
        Some(
          Seq(
            FLambdaExpr(
              Seq(FLambdaBinding(FIdentifier("a"))),
              Seq(
                FAddition(
                  FExprIdentifier("a"),
                  FInt(1)
                )
              )
            )
          )
        )
      )
    }
    test("parse call expression with lambda argument with inline block") {
      parse("l.map(a => {\n a + 1\n })") ==> FCallExpr(
        FMemberExpr(
          FExprIdentifier("l"),
          Seq(FExprIdentifier("map"))
        ),
        Some(
          Seq(
            FLambdaExpr(
              Seq(FLambdaBinding(FIdentifier("a"))),
              Seq(
                FAddition(
                  FExprIdentifier("a"),
                  FInt(1)
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
              FLambdaExpr(
                Seq(FLambdaBinding(FIdentifier("x"))),
                Seq(
                  FAddition(
                    FExprIdentifier("x"),
                    FInt(1)
                  )
                )
              )
            )
          ),
          FAddition(
            FCallExpr(
              FExprIdentifier("t"),
              Some(Seq(FInt(5)))
            ),
            FCallExpr(
              FExprIdentifier("t"),
              Some(Seq(FInt(6)))
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
      parse("(a, b) => a + b") ==> FLambdaExpr(
        Seq(
          FLambdaBinding(FIdentifier("a")),
          FLambdaBinding(FIdentifier("b"))
        ),
        Seq(
          FAddition(
            FExprIdentifier("a"),
            FExprIdentifier("b")
          )
        )
      )
    }
    test("parse lambda expression with block inline expression") {
      parse("(f, g, v) => {\n let fv = f(v)\n g(fv)\n }") ==> FLambdaExpr(
        Seq(
          FLambdaBinding(FIdentifier("f")),
          FLambdaBinding(FIdentifier("g")),
          FLambdaBinding(FIdentifier("v"))
        ),
        Seq(
          FLetExpr(
            FIdentifier("fv"),
            None,
            Seq(
              FCallExpr(
                FExprIdentifier("f"),
                Some(Seq(FExprIdentifier("v")))
              )
            )
          ),
          FCallExpr(
            FExprIdentifier("g"),
            Some(Seq(FExprIdentifier("fv")))
          )
        )
      )
    }
    test("parse nested lambda expression") {
      parse("(a) => (b) => a + b") ==> FLambdaExpr(
        Seq(FLambdaBinding(FIdentifier("a"))),
        Seq(
          FLambdaExpr(
            Seq(FLambdaBinding(FIdentifier("b"))),
            Seq(
              FAddition(
                FExprIdentifier("a"),
                FExprIdentifier("b")
              )
            )
          )
        )
      )
    }
    test("parse lambda expression with annotations") {
      parse("(a: i32) => a + 1") ==> FLambdaExpr(
        Seq(
          FLambdaBinding(
            FIdentifier("a"),
            Some(FSimpleType(FIdentifier("i32")))
          )
        ),
        Seq(
          FAddition(
            FExprIdentifier("a"),
            FInt(1)
          )
        )
      )
    }
    test("parse lambda expression with single param and no paranthesis") {
      parse("a => a + 1") ==> FLambdaExpr(
        Seq(FLambdaBinding(FIdentifier("a"))),
        Seq(
          FAddition(
            FExprIdentifier("a"),
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
          FLambdaExpr(
            Seq(FLambdaBinding(FIdentifier("a"))),
            Seq(
              FAddition(FExprIdentifier("a"), FInt(1))
            )
          )
        )
      )
    }
    test("parse match expression with deconstructing sum and struct types") {
      parse("match m:\n Some(v) => v\n None => b") ==> FMatchExpr(
        FExprIdentifier("m"),
        Seq(
          FCase(
            Seq(
              FSumStructPattern(
                FIdentifier("Some"),
                Seq(FIdentifierPattern("v"))
              )
            ),
            None,
            Seq(FExprIdentifier("v"))
          ),
          FCase(
            Seq(FIdentifierPattern("None")),
            None,
            Seq(FExprIdentifier("b"))
          )
        )
      )
    }
    test("parse match expression with literals") {
      parse("match x:\n 1 => a\n 2 => b\n _ => c") ==> FMatchExpr(
        FExprIdentifier("x"),
        Seq(
          FCase(
            Seq(FInt(1)),
            None,
            Seq(FExprIdentifier("a"))
          ),
          FCase(Seq(FInt(2)), None, Seq(FExprIdentifier("b"))),
          FCase(
            Seq(FWildCardPattern),
            None,
            Seq(FExprIdentifier("c"))
          )
        )
      )
    }
    test("parse match expression with deconstructing tuples") {
      parse("match t:\n (x, y) => x + y") ==> FMatchExpr(
        FExprIdentifier("t"),
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
                FExprIdentifier("x"),
                FExprIdentifier("y")
              )
            )
          )
        )
      )
    }
    test("parse match expression with if guard") {
      parse("match m:\n Some(x) if x > 0 => x") ==> FMatchExpr(
        FExprIdentifier("m"),
        Seq(
          FCase(
            Seq(
              FSumStructPattern(
                FIdentifier("Some"),
                Seq(FIdentifierPattern("x"))
              )
            ),
            Some(
              FGreaterThan(
                FExprIdentifier("x"),
                FInt(0)
              )
            ),
            Seq(FExprIdentifier("x"))
          )
        )
      )
    }
    test(
      "parse match expression with multiple conditional patterns in a case clause"
    ) {
      parse("match S(1, 2):\n S(1, _) | S(_, 2) => 3") ==> FMatchExpr(
        FCallExpr(
          FExprIdentifier("S"),
          Some(Seq(FInt(1), FInt(2)))
        ),
        Seq(
          FCase(
            Seq(
              FSumStructPattern(
                FIdentifier("S"),
                Seq(FInt(1), FWildCardPattern)
              ),
              FSumStructPattern(
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
      parse("match e:\n f @ Left(_) => f\n Right(v) => v") ==> FMatchExpr(
        FExprIdentifier("e"),
        Seq(
          FCase(
            Seq(
              FIdentifierPattern(
                "f",
                Some(
                  FSumStructPattern(
                    FIdentifier("Left"),
                    Seq(FWildCardPattern)
                  )
                )
              )
            ),
            None,
            Seq(FExprIdentifier("f"))
          ),
          FCase(
            Seq(
              FSumStructPattern(
                FIdentifier("Right"),
                Seq(FIdentifierPattern("v"))
              )
            ),
            None,
            Seq(FExprIdentifier("v"))
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
