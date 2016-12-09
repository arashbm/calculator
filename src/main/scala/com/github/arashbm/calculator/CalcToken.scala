package com.github.arashbm.calculator

sealed abstract class CalcToken
case class ValueToken(value: Double) extends CalcToken
case class ParenthesisToken(left: Boolean) extends CalcToken
case class OperatorToken(op: Char, precedence: Int,
  leftAssociative: Boolean,
  arity: Int, action: (List[Double]) => Double) extends CalcToken

object PlusOp extends OperatorToken(
  op = '+', precedence = 2, leftAssociative = true, arity = 2,
  action = ((a: List[Double]) => a.reverse.reduceLeft(_+_)))
object MinusOp extends OperatorToken(
  op = '-', precedence = 2, leftAssociative = true, arity = 2,
  action = ((a: List[Double]) => a.reverse.reduceLeft(_-_)))
object TimesOp extends OperatorToken(
  op = '*', precedence = 3, leftAssociative = true, arity = 2,
  action = ((a: List[Double]) => a.reverse.reduceLeft(_*_)))
object DivisionOp extends OperatorToken(
  op = '/', precedence = 3, leftAssociative = true, arity = 2,
  action = ((a: List[Double]) => a.reverse.reduceLeft(_/_)))
