package com.github.arashbm.calculator

import org.scalatra.test.specs2._
import scala.util.{Try,Success,Failure}

class InfixQuerySpec extends MutableScalatraSpec {
  "InfixQuery" should {
    "tokenize" in {
      "correct input" in {
        var tokens = new InfixQuery("123 + 2/(3.2 + 1.2)").tokenize
        tokens must beSuccessfulTry
        tokens must beLike {
          case Success(List(
            ValueToken(123.0),
            OperatorToken('+', _, _, _, _),
            ValueToken(2.0),
            OperatorToken('/', _, _, _, _),
            ParenthesisToken(true),
            ValueToken(3.2),
            OperatorToken('+', _, _, _, _),
            ValueToken(1.2),
            ParenthesisToken(false)
          )) => ok
        }
      }

      "incorrect input" in {
        var tokens = new InfixQuery("123 + 2/(3.2 + 1.2 2.3)").tokenize
        tokens must beFailedTry
      }
    }

    "shuntingYard" in {
      "simple valid infix expression" in {
        var tokens = List(
          ValueToken(123.0),
          OperatorToken(op = '+', precedence = 2, leftAssociative = true,
            arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_))),
          ValueToken(123.0)
        )
        var postfix = new InfixQuery("blah").shuntingYard(tokens)
        postfix must beSuccessfulTry
        postfix must beLike {
          case Success(List(
            ValueToken(123.0),
            ValueToken(123.0),
            OperatorToken('+', _, _, _, _)
          )) => ok
        }
      }

      "complicated valid infix expression" in {

        // 123 + 2/(3.2 + 1.2)
        var tokens = List(
            ValueToken(123.0),
            OperatorToken(op = '+', precedence = 2, leftAssociative = true,
              arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_))),
            ValueToken(2.0),
            OperatorToken(op = '/', precedence = 3, leftAssociative = true,
              arity = 2, action = ((a: List[Double]) => a.reduceLeft(_/_))),
            ParenthesisToken(true),
            ValueToken(3.2),
            OperatorToken(op = '+', precedence = 2, leftAssociative = true,
              arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_))),
            ValueToken(1.2),
            ParenthesisToken(false)
          )
        var postfix = new InfixQuery("blah").shuntingYard(tokens)
        postfix must beSuccessfulTry

        // 123 2 3.2 1.2 + / +
        postfix must beLike {
          case Success(List(
            ValueToken(123.0),
            ValueToken(2.0),
            ValueToken(3.2),
            ValueToken(1.2),
            OperatorToken('+', _, _, _, _),
            OperatorToken('/', _, _, _, _),
            OperatorToken('+', _, _, _, _)
          )) => ok
        }
      }

      "invalid infix expression" in {
        var tokens = List(
          ValueToken(123.0),
          OperatorToken(op = '+', precedence = 2, leftAssociative = true,
            arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_))),
          ValueToken(123.0),
          ValueToken(123.0)
        )
        var postfix = new InfixQuery("blah").shuntingYard(tokens)
        postfix must beFailedTry
      }
    }

    "solvePostfix" in {
      "simple valid postfix expression" in {
        var tokens = List(
          ValueToken(2.1),
          ValueToken(3.2),
          OperatorToken(op = '+', precedence = 2, leftAssociative = true,
            arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_)))
        )
        var result = new InfixQuery("blah").solvePostfix(tokens)
        result must beSuccessfulTry.withValue(beCloseTo(5.3 within 2.significantFigures))
      }

      "complicated valid postfix expression" in {
        var tokens = List(
            ValueToken(123.0),
            ValueToken(2.0),
            ValueToken(3.2),
            ValueToken(1.2),
            OperatorToken(op = '+', precedence = 2, leftAssociative = true,
              arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_))),
            OperatorToken(op = '/', precedence = 3, leftAssociative = true,
              arity = 2, action = ((a: List[Double]) => a.reduceLeft(_/_))),
            OperatorToken(op = '+', precedence = 2, leftAssociative = true,
              arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_)))
        )
        var result = new InfixQuery("blah").solvePostfix(tokens)
        result must beSuccessfulTry.withValue(
          beCloseTo(123.45 within 2.significantFigures))
      }


      "invalid postfix expression" in {
        "too many args" in {
          var tokens = List(
            ValueToken(2.1),
            ValueToken(3.2),
            ValueToken(3.2),
            OperatorToken(op = '+', precedence = 2, leftAssociative = true,
              arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_)))
            )
          var result = new InfixQuery("blah").solvePostfix(tokens)
          result must beFailedTry.withThrowable[ParseFailure]("too many values in user input")
        }
        "too few args" in {
          var tokens = List(
            ValueToken(3.2),
            OperatorToken(op = '+', precedence = 2, leftAssociative = true,
              arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_)))
            )
          var result = new InfixQuery("blah").solvePostfix(tokens)
          result must beFailedTry.withThrowable[ParseFailure]("not enough arguments for operator .")
        }
      }
    }
  }
}
