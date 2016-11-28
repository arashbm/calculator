package com.github.arashbm.calculator

import org.scalatra.test.specs2._
import scala.util.{Try,Success,Failure}

class InfixQuerySpec extends MutableScalatraSpec {
  "InfixQuery" should {
    "tokenize" in {
      "correct input" in {
        var tokens = new InfixQuery("123 + 2/(3.2 + 1.2)").tokenize
        tokens must beSuccessfulTry
        todo // check actual values
      }

      "incorrect input" in {
        var tokens = new InfixQuery("123 + 2/(3.2 + 1.2 2.3)").tokenize
        tokens must beFailedTry
      }
    }

    "shuntingYard" in {
      "valid infix expression" in {
        var tokens = List(
          ValueToken(123.0),
          OperatorToken(op = '+', precedence = 2, leftAssociative = true,
            arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_))),
          ValueToken(123.0)
        )
        var postfix = new InfixQuery("blah").shuntingYard(tokens)
        postfix must beSuccessfulTry
        todo // check actual values
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

    "solveRPN" in {
      "valid RPN expression" in {
        var tokens = List(
          ValueToken(2.1),
          ValueToken(3.2),
          OperatorToken(op = '+', precedence = 2, leftAssociative = true,
            arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_)))
        )
        var result = new InfixQuery("blah").solveRPN(tokens)
        result must beSuccessfulTry.withValue(beCloseTo(5.3 within 2.significantFigures))
      }

      "invalid RPN expression" in {
        "too many args" in {
          var tokens = List(
            ValueToken(2.1),
            ValueToken(3.2),
            ValueToken(3.2),
            OperatorToken(op = '+', precedence = 2, leftAssociative = true,
              arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_)))
            )
          var result = new InfixQuery("blah").solveRPN(tokens)
          result must beFailedTry.withThrowable[ParseFailure]("too many values in user input")
        }
        "too few args" in {
          var tokens = List(
            ValueToken(3.2),
            OperatorToken(op = '+', precedence = 2, leftAssociative = true,
              arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_)))
            )
          var result = new InfixQuery("blah").solveRPN(tokens)
          result must beFailedTry.withThrowable[ParseFailure]("not enough arguments for operator .")
        }
      }
    }
  }
}
