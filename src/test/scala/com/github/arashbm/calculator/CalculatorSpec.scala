package com.github.arashbm.calculator

import org.scalatra.test.specs2._
import scala.util.parsing.json._

// For more on Specs2, see http://etorreborre.github.com/specs2/guide/org.specs2.guide.QuickStart.html
class CalculatorSpec extends MutableScalatraSpec {
  addServlet(classOf[Calculator], "/*")
  "GET /calculus on Calculator" should {
    "return status 400 when no query given" in {
      get("/calculus") {
        status must_== 400
        todo // check the actual body
      }
    }

    "return status 400 when given bad query" in {
      val query = new sun.misc.BASE64Encoder().encodeBuffer("2 blah 3").trim
      get(s"/calculus?query=${query}") {
        status must_== 400
        todo // check the actual body
      }
    }

    "return status 200 when given correct query" in {
      val query = new sun.misc.BASE64Encoder().encodeBuffer("2 * (23/(3*3))- 23 * (2*3)").trim
      get(s"/calculus?query=${query}") {
        status must_== 200
        todo // check the actual body
      }
    }
  }
}
