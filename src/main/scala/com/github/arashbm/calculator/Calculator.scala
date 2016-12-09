package com.github.arashbm.calculator

import org.scalatra._
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.json._
import org.slf4j.{Logger, LoggerFactory}
import scala.util.{Try,Success,Failure}
import scala.collection.mutable.Queue



class Calculator extends CalculatorStack with JacksonJsonSupport {
  protected implicit lazy val jsonFormats: Formats = DefaultFormats

  val logger =  LoggerFactory.getLogger(getClass)

  before() {
    contentType = formats("json")
  }

  get("/calculus") {
    var query = new String(new sun.misc.BASE64Decoder().decodeBuffer(params("query")))
    logger.info(s"calculator query: '${query}'")
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

