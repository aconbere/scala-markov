package com.conbere.markov

import scala.util.Random
import scala.io.Source

trait MarkovC[C,T] {
  val chain:Map[(C,C), T]
  val start:C
  val stop:C

  def add(k:(C,C), v:C):MarkovC[C,T]

  def contains(k:(C,C)) = chain.contains(k)

  def random[K](l:List[K]) = l(new Random().nextInt(l.length))

  def choice(x:T):C

  def seed = {
    random(chain.keys.filter { case (s, _) => s == start }.toList)
  }

  def next(seed:(C,C)):Option[C] = {
    for (l <- chain.get(seed)) yield {
      choice(l)
    }
  }

  def insert(word:List[C]) = {
    ((start :: word) :+ stop).sliding(3).foldLeft(this) {
      case (acc, List(x1, x2, x3)) =>
        acc.add((x1, x2), x3)
      case (acc, _) =>
        acc
      }
  }

  def generate(maxLength:Int) = {
    def inner(count:Int, acc:List[C], previous:(C,C)):List[C] = {
      if (count >= 0) {
        next(previous) match {
          case Some(`stop`) =>
            acc
          case Some(n) =>
            inner(count - 1, acc :+ n, (previous._2, n))
          case _ =>
            acc
        }
      } else {
        acc
      }
    }
    val s = seed
    inner(maxLength, List[C](s._2), s)
  }
}

class MC[C](val start:C, val stop:C, val chain: Map[(C,C), List[C]])
extends MarkovC[C, List[C]] {
  def this(start:C, stop:C) =
    this(start, stop, Map[(C,C), List[C]]())

  def add(k: (C,C), v:C) = {
    val value = chain.getOrElse(k, List()) :+ v
    new MC[C](start, stop, chain + (k -> value))
  }

  def choice(l:List[C]):C = random(l)
}

class MMC[C](val start:C, val stop:C, val chain: Map[(C,C), Map[C,Int]])
extends MarkovC[C, Map[C, Int]] {
  def this(start:C, stop:C) =
    this(start, stop, Map[(C,C), Map[C,Int]]())

  def add(k: (C,C), v:C) = {
    val m = chain.getOrElse(k, Map[C,Int]())
    val newM = m + (v -> (m.getOrElse(v, 0) + 1))
    new MMC[C](start, stop, chain + (k -> newM))
  }

  def choice(m:Map[C,Int]): C = {
    def inner(c:Int, l:List[(C,Int)]):C = {
      if (c <= 0)
        l.head._1
      else
        inner(c - l.head._2, l.tail)
    }
    inner(new Random().nextInt(m.values.sum), m.toList)
  }
}



class MarkovChain[C](val start:C, val stop:C, val chain: Map[(C,C), List[C]]) {

  def this(start:C, stop:C) = this(start, stop, Map[(C,C), List[C]]())

  def add(k:(C,C), v:List[C]) = {
    val value = chain.getOrElse(k, List()) ++ v
    new MarkovChain[C](start, stop, chain + (k -> value))
  }

  def contains(k:(C,C)) = chain.contains(k)

  def insert(word:List[C]): MarkovChain[C] = {
    ((start :: word) :+ stop).sliding(3).foldLeft(this) {
      case (acc, List(x1, x2, x3)) =>
        acc.add((x1, x2), List(x3))
      case (acc, _) =>
        acc
      }
  }

  def merge(that: MarkovChain[C]) = {
    that.chain.foldLeft(this) {
      case (acc, (k,v)) => acc.add(k,v)
    }
  }

  def next(seed:(C,C)) = {
    for (l <- chain.get(seed)) yield {
      l(new Random().nextInt(l.length))
    }
  }

  def seed = {
    val ks = chain.keys.toList.filter { case (s, _) => s == start }
    ks(new Random().nextInt(ks.length))
  }

  def generate(maxLength:Int) = {
    def inner(count:Int, acc:List[C], previous:(C,C)):List[C] = {
      if (count >= 0) {
        next(previous) match {
          case Some(`stop`) =>
            acc
          case Some(n) =>
            inner(count - 1, acc :+ n, (previous._2, n))
          case _ =>
            acc
        }
      } else {
        acc
      }
    }
    val s = seed
    inner(maxLength, List[C](s._2), s)
  }
}

object Markov {
  def fromFile(filePath: String) = {
    Source.fromFile(filePath)
          .getLines()
          .foldLeft(new MarkovChain[Char]('\2','\3'))((acc, w) =>
             acc.insert(w.toLowerCase.toList)
             )
  }
}
