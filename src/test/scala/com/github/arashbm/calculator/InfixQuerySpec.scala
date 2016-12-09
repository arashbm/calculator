package com.github.arashbm.calculator

import org.scalatra.test.specs2._
import scala.util.{Try,Success,Failure}

class InfixQuerySpec extends MutableScalatraSpec {
  "InfixQuery" should {
    "tokenize" in {
      "correct input" in {
        var tokens = new InfixQuery("123 + 2/(3.2 + 1.2)").tokenize
        tokens must beSuccessfulTry.like {
          case List(ValueToken(123.0),
            PlusOp,
            ValueToken(2.0),
            DivisionOp,
            ParenthesisToken(true),
            ValueToken(3.2),
            PlusOp,
            ValueToken(1.2),
            ParenthesisToken(false)) => ok
        }
      }

      "incorrect input: bad number" in {
        var tokens = new InfixQuery("123 + 1.2.3").tokenize
        tokens must beFailedTry
      }

      "incorrect input: unknown operator" in {
        var tokens = new InfixQuery("123 + 1.23 _ 1").tokenize
        tokens must beFailedTry
      }
    }

    "shuntingYard" in {
      "simple valid infix expression" in {
        val tokens = List(
          ValueToken(123.0),
          PlusOp,
          ValueToken(123.0)
        )
        val postfix = new InfixQuery("blah").shuntingYard(tokens)
        postfix must beSuccessfulTry.like {
          case pf: PostfixExpression if pf.tokens == List(ValueToken(123.0),
            ValueToken(123.0),
            PlusOp) => ok
        }
      }

      "complicated valid infix expression" in {

        // 123 + 2/(3.2 + 1.2)
        val tokens = List(
            ValueToken(123.0),
            PlusOp,
            ValueToken(2.0),
            DivisionOp,
            ParenthesisToken(true),
            ValueToken(3.2),
            PlusOp,
            ValueToken(1.2),
            ParenthesisToken(false)
          )
        val postfix = new InfixQuery("blah").shuntingYard(tokens)
        postfix must beSuccessfulTry.like {
          case pf: PostfixExpression if pf.tokens == List(ValueToken(123.0),
            ValueToken(2.0),
            ValueToken(3.2),
            ValueToken(1.2),
            PlusOp,
            DivisionOp,
            PlusOp) => ok
        }
      }

      "invalid left paranthesis in infix expression" in {
        val tokens = List(
          ValueToken(123.0),
          ParenthesisToken(true),
          ValueToken(123.0),
          ParenthesisToken(true),
          ValueToken(123.0)
        )
        val postfix = new InfixQuery("blah").shuntingYard(tokens)
        postfix must beFailedTry
      }

      "invalid right paranthesis in infix expression" in {
        val tokens = List(
          ValueToken(123.0),
          ParenthesisToken(false),
          ValueToken(123.0)
        )
        val postfix = new InfixQuery("blah").shuntingYard(tokens)
        postfix must beFailedTry
      }
    }
  }
}
