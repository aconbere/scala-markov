import scala.io.Source

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import org.junit.runner.RunWith

import com.conbere.markov._

@RunWith(classOf[JUnitRunner])
class MarkovSuite extends FunSuite {
  val planetNames = Source.fromFile("./src/test/resources/planets.txt").getLines.toList

  trait TestStateStorage {
    val s1 = new StateStorage[Char]()
    val s2 = s1.add(('a', 'b'), 'c').add(('b', 'c'), 'd')
    val s3 = s1.insert("abcd".toList)
  }

  trait TestM {
    val m1 = new MarkovChain[Char]('\2', '\3')
    val m2 = m1.insert("ab".toList)
  }

  def testLargeInsert[T](m:MarkovChain[Char]) = {
    planetNames.foldLeft(m)((acc, n) => acc.insert(n.toLowerCase.toList)).generate(10)
  }

  def testGenerate[T](m:MarkovChain[Char]) = {
    val x = m.generate(10)
    assert(x == Some(List('a', 'b')))
  }

  test("state add") {
    new TestStateStorage {
      assert(s2.keys == Set(('a', 'b'), ('b', 'c')))
    }
  }

  test("state insert") {
    new TestStateStorage {
      assert(s3.keys == Set(('a', 'b'), ('b', 'c')))
    }
  }

  test("a large insert") {
    new TestM {
      testLargeInsert(m1)
    }
  }

  test("generate") {
    new TestM {
      testGenerate(m2)
    }
  }
}
