package com.github.arashbm.calculator

import scala.util.{Try,Success,Failure}

class PostfixExpression(val tokens: List[CalcToken]) {
  def solve(): Try[Double] =
    solve(stack = List[ValueToken](), tokens = tokens)

  private def solve(stack: List[ValueToken],
    tokens: List[CalcToken]): Try[Double] = tokens match {
      case List() if (stack.length == 1) => Success(stack.head.value)
      case List() if (stack.length == 0) =>
        Failure(new ParseFailure("empty user input"))
      case List() => Failure(new ParseFailure("too many values in user input"))
      case (token:ValueToken) :: (rest:List[CalcToken]) =>
        solve(token :: stack , rest)
      case (_:ParenthesisToken) :: (_:List[CalcToken]) =>
        Failure(new ParseFailure("postfix notation cannot contain parenthesis"))
      case (token:OperatorToken) :: (rest:List[CalcToken]) if (stack.length < token.arity) =>
        Failure(new ParseFailure(s"not enough arguments for operator '${token.op}'"))
      case (token:OperatorToken) :: (rest:List[CalcToken]) => {
        val (head, tail) = stack.splitAt(token.arity)
        var calcStep = ValueToken(token.action(head.map(_.value)))
        solve(calcStep :: tail, rest)
      }
  }
}
