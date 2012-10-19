import scala.io.Source

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import org.junit.runner.RunWith

import com.conbere.markov._

@RunWith(classOf[JUnitRunner])
class MarkovSuite extends FunSuite {
  val planetNames = Source.fromFile("./src/test/resources/planets.txt").getLines.toList

  trait TestM {
    val m1 = new MarkovChainList[Char]('\2', '\3', 2)
    val m2 = m1.insert("ab".toList)
    val mm1 = new MarkovChainMap[Char]('\2', '\3', 2)
    val mm2 = m1.insert("ab".toList)
  }

  def testLargeInsert[T](m:MarkovChain[Char,T]) = {
    planetNames.foldLeft(m)((acc, n) => acc.insert(n.toLowerCase.toList)).generate(10)
  }

  def testInsert[T](m:MarkovChain[Char,T]) = {
    assert(m.contains(List('\2', 'a')))
    assert(m.contains(List('a', 'b')))
  }

  def testSeed[T](m:MarkovChain[Char,T]) = {
    val s = m.seed
    assert(s == List('\2', 'a'))
  }

  def testGenerate[T](m:MarkovChain[Char,T]) = {
    assert(m.seed == List('\2', 'a'))
    assert(m.generate(10) == List('a', 'b'))
  }

  test("a large insert") {
    new TestM {
      testLargeInsert(m1)
      testLargeInsert(mm1)
    }
  }

  test("insert") {
    new TestM {
      testInsert(m2)
      testInsert(mm2)
    }
  }

  test("seed") {
    new TestM {
      testSeed(m2)
      testSeed(mm2)
    }
  }

  test("generate") {
    new TestM {
      testGenerate(m2)
      testGenerate(mm2)
    }
  }
}
