package com.github.arashbm.calculator

import org.scalatra._
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.json._
import org.slf4j.{Logger, LoggerFactory}
import scala.util.{Try,Success,Failure}



class Calculator extends CalculatorStack with JacksonJsonSupport {
  protected implicit lazy val jsonFormats: Formats = DefaultFormats

  val logger =  LoggerFactory.getLogger(getClass)

  before() {
    contentType = formats("json")
  }

  get("/calculus") {
    var query = new String(new sun.misc.BASE64Decoder().decodeBuffer(params("query")))
    logger.info("calculator query: " + query)
    var infix = new InfixQuery(query)
    infix.solve match {
      case Success(result) =>
        Map("error" -> false, "result" -> result)
      case Failure(error: ParseFailure) =>
        halt(status = 400, body = Map("error" -> true, "message" -> error.getMessage))
      case Failure(error) =>
        halt(status = 500, body = Map("error" -> true, "message" -> error.getMessage))
    }
  }

}

class InfixQuery(query: String) {
  def solve: Try[Double] = {
    for {
      tokens <- tokenize
      rpn <- shuntingYard(tokens)
      result <- solveRPN(rpn)
    } yield result
  }

  def tokenize: Try[List[CalcToken]] = {
    // query.foldLeft(List[Char]())((l, c) => l ++ List(c))
    Success(List(
      ValueToken(123.0),
      OperatorToken(op = '+', precedence = 2, leftAssociative = true,
        arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_))),
      ValueToken(123.0)
      ))
  }

  def shuntingYard(tokens: List[CalcToken]): Try[List[CalcToken]] = {
    var opStack = List[CalcToken]()
    var valQueue = new scala.collection.mutable.Queue[CalcToken]
    Success(
      List(
        ValueToken(123.0),
        ValueToken(123.0),
        OperatorToken(op = '+', precedence = 2, leftAssociative = true,
          arity = 2, action = ((a: List[Double]) => a.reduceLeft(_+_)))
        )
    )
  }

  def solveRPN(tokens: List[CalcToken]): Try[Double] = {
    solveRPN(stack = List[ValueToken](), tokens = tokens)
  }

  def solveRPN(stack: List[ValueToken], tokens: List[CalcToken]): Try[Double] = tokens match {
    case List() => {
      if (stack.length == 1)
        Success(stack.head.value)
      else
        Failure(new ParseFailure("too many values in user input"))
    }
    case (token:ValueToken) :: (rest:List[CalcToken]) => solveRPN(token :: stack , rest)
    case (token:OperatorToken) :: (rest:List[CalcToken]) => {
      if (stack.length < token.arity) {
        Failure(new ParseFailure("not enough arguments for operator " + token.op))
      } else {
        val (head, tail) = stack.splitAt(token.arity)
        var calcStep = ValueToken(value = token.action(head.map(_.value)))
        solveRPN(calcStep :: tail, rest)
      }
    }
  }
}

case class ParseFailure(message: String) extends Throwable(message)

sealed abstract class CalcToken
case class ValueToken(value: Double) extends CalcToken
case class OperatorToken(op: Char,precedence: Int, leftAssociative: Boolean,
  arity: Int, action: (List[Double]) => Double) extends CalcToken
