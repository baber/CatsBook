package misc.freemonad

import cats.Functor

sealed trait KVS[A]

case class Put[A](key: String, value: String, a: A) extends KVS[A]

case class Get[A](key: String, h: String ⇒ A) extends KVS[A]

case class Delete[A](key: String, a: A) extends KVS[A]


object KVS {

  def runKVS[A](kvs: Free[KVS, A], table: Map[String, String]) : Map[String, String] = {
    kvs match {
      case More(Put(k, v, a))     ⇒ runKVS(a, table + (k → v))
      case More(Delete(k, a))     ⇒ runKVS(a, table - k)
      case More(Get(k,f))         ⇒ runKVS(f(table(k)), table)
      case Done(a)                ⇒ table
    }
  }

  def modify[A](key: String, f: String ⇒ String) = {
    for {
      v     ← get(key)
      _     ← put(key, v)
    } yield ()
  }

  def put(k: String, v: String): Free[KVS, Unit] = More(Put(k, v, Done()))

  def delete(k: String): Free[KVS, Unit] = More(Delete(k, Done()))

  def get(k: String): Free[KVS, String] = More(Get(k, v ⇒ Done(v)))

  implicit val functor: Functor[KVS] = new Functor[KVS] {
    override def map[A, B](fa: KVS[A])(f: A ⇒ B) = {
      fa match {
        case Get(k,h)       ⇒ Get(k,h andThen f)
        case Delete(k, a)   ⇒ Delete(k, f(a))
        case Put(k,v,a)     ⇒ Put(k, v, f(a))
      }
    }
  }


  def main (args: Array[String] ): Unit = {
    val description = for {
      x     ← put("key1", "value1")
      y     ← put("key2", "value2")
      z     ← put("key3", "value3")
      a     ← delete("key2")
      b     ← get("key1")
    } yield b


    val myMap = runKVS(description , Map())
    println(myMap)
  }
}

