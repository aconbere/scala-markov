import scala.io.Source

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import org.junit.runner.RunWith

import com.conbere.markov._

@RunWith(classOf[JUnitRunner])
class MarkovSuite extends FunSuite {
  val planetNames = Source.fromFile("./src/test/resources/planets.txt").getLines.toList

  trait TestM {
    val m1 = new MarkovChain[Char]('\2', '\3')
    val m2 = m1.insert("ab".toList)
  }

  test("a large insert") {
    new TestM {
      planetNames.foldLeft(m1)((acc, n) => acc.insert(n.toLowerCase.toList)).generate(10)
    }
  }

  test("insert") {
    new TestM {
      assert(m2.contains(('\2', 'a')))
      assert(m2.contains(('a', 'b')))
    }
  }

  test("seed") {
    new TestM {
      val s = m2.seed
      assert(s == ('\2', 'a'))
    }
  }

  test("generate") {
    new TestM {
      assert(m2.seed == ('\2', 'a'))
      assert(m2.generate(10) == List('a', 'b'))
    }
  }
}
