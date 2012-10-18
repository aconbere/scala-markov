package com.conbere.archspace

import scala.util.Random
import scala.io.Source

class MarkovChain[C](val start:C, val stop:C, val chain: Map[(C,C), List[C]]) {

  def this(start:C, stop:C) = this(start, stop, Map[(C,C), List[C]]())

  def add(k:(C,C), v:List[C]) = {
    val value = chain.getOrElse(k, List()) ++ v
    new MarkovChain[C](start, stop, chain + (k -> value))
  }

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
  def build() = {
    Source.fromFile("./names/planets.txt")
          .getLines()
          .foldLeft(new MarkovChain[Char]('\2','\3'))((acc, w) =>
             acc.insert(w.toLowerCase.toList)
             )
  }

  def out(m:MarkovChain[Char]) = {
    m.chain.foreach {
      case (k,v) =>
        println(k)
        println(v)
    }
  }

  def test(m:MarkovChain[Char]) = {
    println(m.generate(10).mkString)
    println(m.generate(10).mkString)
    println(m.generate(10).mkString)
    println(m.generate(10).mkString)
    println(m.generate(10).mkString)
    println(m.generate(10).mkString)
  }
}
