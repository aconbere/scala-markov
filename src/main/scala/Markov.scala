package com.conbere.markov

import scala.util.Random
import scala.io.Source

package object M {
  def random[K](l: List[K]) = l(new Random().nextInt(l.length))
}

trait Frequency[C] {
  def add(v: C): Frequency[C]
  def choice(): Option[C]
  def contains(v: C): Boolean
}

class DirectFrequency[C](val frequencies: List[C])
extends Frequency[C] {
  def this() = this(List[C]())
  def add(v: C) = new DirectFrequency(frequencies :+ v)
  def contains(v: C) = frequencies.contains(v)
  def choice() = Some(M.random(frequencies))
}

class RouletteFrequency[C](val frequencies: Map[C,Int])
extends Frequency[C] {
  def this() = this(Map[C,Int]())

  def add(v: C): RouletteFrequency[C] =
    new RouletteFrequency(frequencies + (v -> (frequencies.getOrElse(v,0) + 1)))

  def contains(v: C) = frequencies.contains(v)

  def choice() = {
    def inner(l: List[(C,Int)], n: Int): Option[C]= {
      if (l.isEmpty) {
        None
      } else {
        l.head match {
          case head @ (c, i) =>
            if(l.length < 1 || (n - i) <= 0)
              Some(c)
            else
              inner(l.tail, n - i)
        }
      }
    }
    inner(frequencies.toList,
          new Random()
            .nextInt(frequencies.values.sum))
  }
}

trait EdgeFrequency[T, C] {
  val frequencies: Map[T,RouletteFrequency[C]]

  def add(k: T, v: C): EdgeFrequency[T,C]

  def contains(k: T) = frequencies.contains(k)
  def keys = frequencies.keys
  def get(k: T): Option[RouletteFrequency[C]] =
    frequencies.get(k)
}

class ListEdgeFrequency[C] (val frequencies: Map[List[C], RouletteFrequency[C]])
extends EdgeFrequency[List[C], C] {
  def this() = this(Map[List[C], RouletteFrequency[C]]())

  def add(k:List[C], v:C) =
    new ListEdgeFrequency[C](frequencies +
      (k -> frequencies.getOrElse(k, new RouletteFrequency()).add(v)))
}

class TupleEdgeFrequency[C] (val frequencies: Map[(C,C), RouletteFrequency[C]])
extends EdgeFrequency[(C,C), C] {
  def this() = this(Map[(C,C), RouletteFrequency[C]]())

  def add(k:(C,C), v:C) =
    new TupleEdgeFrequency[C](frequencies +
      (k -> frequencies.getOrElse(k, new RouletteFrequency()).add(v)))
}

trait Chainable[T, C] {
  val edges: EdgeFrequency[T, C]

  val start: C
  val stop: C
  val chainLength: Int

  def insert(word: T)
  def seed: T
  def generate(maxLength: Int): List[C]

  def next(seed: T): Option[C] = {
    for (l <- edges.get(seed);
         n <- l.choice)
    yield {
       n
    }
  }
}


class MarkovChain[C](val start: C,
                     val stop: C,
                     val chainLength: Int,
                     val edges: EdgeFrequency[List[C],C])
extends Chainable[List[C], C] {
  def seed = M.random(edges.keys.filter(x => x.head == start).toList)

  def insert(word: List[C]) = {
    val w = (start :: word) :+ stop

    w.sliding(chainLength + 1)
     .foldLeft(this)((acc, l) =>
       if (l.length == chainLength + 1)
         new MarkovChain[C](start,
                            stop,
                            chainLength,
                            edges.add(l.slice(0, chainLength), l.last))
       else
         acc
     )
  }

  def generate(maxLength: Int): List[C] = {
    def inner(count: Int, acc: List[C], previous: List[C]): List[C] = {
      if (count >= 0) {
        next(previous) match {
          case Some(`stop`) =>
            acc
          case Some(n) =>
            inner(count - 1, acc :+ n, previous.tail :+ n)
          case _ =>
            acc
        }
      } else {
        acc
      }
    }
    val s = seed
    inner(maxLength, s.tail, s)
  }
}
