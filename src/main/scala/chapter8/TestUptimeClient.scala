package chapter8

import cats.Id

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
}