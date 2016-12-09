package com.github.arashbm.calculator

import scala.util.{Try,Success,Failure}
import scala.util.matching.Regex

case class ParseFailure(message: String) extends Throwable(message)

class InfixQuery(val query: String) {
  def solve: Try[Double] = for {
    tokens <-  tokenize
    postfix <- shuntingYard(tokens)
    result <-  postfix.solve
  } yield result

  def tokenize: Try[List[CalcToken]] = {
    // adding an end of query character makes everything much easier
    tokenize(query.toList :+ ' ', None, List[CalcToken]())
  }

  private def tokenize(query: List[Char], current_number: Option[List[Char]],
    acc: List[CalcToken]): Try[List[CalcToken]] = {
    query match {
      case List() => Success(acc.reverse)
      case letter :: rest  => {
        val number = "[\\d.]".r
        val whitespace = "\\s".r
        (letter, current_number) match {
          case (number(), None) => tokenize(rest, Some(List(letter)), acc)
          case (number(), Some(number)) =>
            tokenize(rest, Some(number :+ letter), acc)
          case (_, Some(number)) => {
            Try(number.mkString.toDouble) match {
              case Success(double) =>
                tokenize(query, None, ValueToken(double) :: acc)
              case Failure(_) =>
                Failure(ParseFailure("bad number token: " + number.mkString))
            }
          }
          case (whitespace(), None) => tokenize(rest, None, acc)
          case ('+', None) => tokenize(rest, None, PlusOp :: acc)
          case ('-', None) => tokenize(rest, None, MinusOp :: acc)
          case ('/', None) => tokenize(rest, None, DivisionOp :: acc)
          case ('*', None) => tokenize(rest, None, TimesOp :: acc)
          case ('(', None) =>
            tokenize(rest, None, ParenthesisToken(true) :: acc)
          case (')', None) =>
            tokenize(rest, None, ParenthesisToken(false) :: acc)
          case (char, None) =>
            Failure(ParseFailure(s"unknown token: '${char.toString}'"))
        }
      }
    }
  }

  def shuntingYard(tokens: List[CalcToken]): Try[PostfixExpression] =
    shuntingYard(tokens = tokens,
      valQueue = List[CalcToken](),
      opStack = List[CalcToken]())

  // This actually produces reversed expression (since prepending is O(1) and
  // appending is O(n)) and the results are reversed in last step.
  private def shuntingYard(tokens: List[CalcToken], valQueue: List[CalcToken],
    opStack: List[CalcToken]) : Try[PostfixExpression] = tokens match {
      case List() => opStack match {
        case List() =>  Success(new PostfixExpression(valQueue.reverse))
        case ParenthesisToken(_) :: _ =>
          Failure(new ParseFailure("mismatched parentheses in infix expression"))
        case op :: rest => shuntingYard(List(), op :: valQueue, rest)
      }
      case (token:ValueToken) :: (rest:List[CalcToken]) =>
        shuntingYard(tokens = rest, valQueue =  token :: valQueue, opStack = opStack)
      case (o1:OperatorToken) :: (rest:List[CalcToken]) => {
        opStack match {
          case (o2:OperatorToken)::osRest if shuntingYardPrecedence(o1, o2) =>
            shuntingYard(tokens, o2 :: valQueue, osRest)
          case _ => shuntingYard(rest, valQueue, o1 :: opStack)
        }
      }
      case (p: ParenthesisToken) :: (rest:List[CalcToken]) if p.left =>
        shuntingYard(rest, valQueue, p :: opStack)
      case (p: ParenthesisToken) :: (rest:List[CalcToken]) if !p.left => {
        opStack match {
          case List() =>
            Failure(new ParseFailure("mismatched parentheses in infix expression"))
          case ParenthesisToken(true) :: osRest =>
            shuntingYard(rest, valQueue, osRest)
          case op :: osRest =>
            shuntingYard(p :: rest, op :: valQueue, osRest)
        }
      }
  }

  // determines whether we should take o2 out of opStack before putting o1 in
  private def shuntingYardPrecedence(o1: OperatorToken, o2: OperatorToken): Boolean =
    (o1.leftAssociative && (o1.precedence <= o2.precedence)) ||
      (!o1.leftAssociative && (o1.precedence < o2.precedence))

}

