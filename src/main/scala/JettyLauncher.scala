import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ DefaultServlet, ServletContextHandler }
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener

object JettyLauncher {
  def main(args: Array[String]) {
    val port = if(System.getProperty("http.port") != null) System.getProperty("http.port").toInt else 8080

    val server = new Server(port)
    val context = new WebAppContext()
    context.setInitParameter("org.eclipse.jetty.servlet.Default.dirAllowed", "false");
    context.setInitParameter(org.scalatra.EnvironmentKey, "production")

    context.setContextPath("/")
    context.setResourceBase("src/main/webapp")

    context.getErrorHandler.setShowStacks(false)

    context.setEventListeners(Array(new ScalatraListener))

    server.setHandler(context)

    server.start
    server.join
  }
}
