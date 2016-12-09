package com.github.arashbm.calculator

import org.scalatra.test.specs2._
import scala.util.{Try,Success,Failure}

class PostfixExpressionSpec extends MutableScalatraSpec {
  "PostfixExpression" should {


    "solvePostfix" in {
      "simple valid postfix expression" in {
        val tokens = List(
          ValueToken(2.1),
          ValueToken(3.2),
          PlusOp
        )
        val result = new PostfixExpression(tokens).solve
        result must beSuccessfulTry.withValue(beCloseTo(5.3 within 2.significantFigures))
      }

      "complicated valid postfix expression" in {
        val tokens = List(
            ValueToken(123.0),
            ValueToken(2.0),
            ValueToken(3.2),
            ValueToken(1.2),
            PlusOp,
            DivisionOp,
            PlusOp
        )
        val result = new PostfixExpression(tokens).solve
        result must beSuccessfulTry.withValue(
          beCloseTo(123.45 within 2.significantFigures))
      }


      "invalid postfix expression" in {
        "too many args" in {
          val tokens = List(
            ValueToken(2.1),
            ValueToken(3.2),
            ValueToken(3.2),
            PlusOp
            )
          val result = new PostfixExpression(tokens).solve
          result must beFailedTry.withThrowable[ParseFailure]("too many values in user input")
        }

        "too few args" in {
          val tokens = List(
            ValueToken(3.2),
            PlusOp
            )
          val result = new PostfixExpression(tokens).solve
          result must beFailedTry.withThrowable[ParseFailure]("not enough arguments for operator '.'")
        }

        "empty input" in {
          val tokens = List()
          val result = new PostfixExpression(tokens).solve
          result must beFailedTry.withThrowable[ParseFailure]("empty user input")
        }
      }
    }

  }
}
