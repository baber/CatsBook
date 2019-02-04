package chapter8

import org.scalatest.{FlatSpec, Matchers}
import chapter4.instances.ApplicativeInstances._

class UptimeServiceSpec extends FlatSpec with Matchers {

  "uptime service" should "return total uptime" in {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    expected shouldEqual actual
  }

}
