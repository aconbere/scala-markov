package com.conbere.markov

import scala.util.Random
import scala.io.Source

package object M {
  def random[K](l: List[K]): Option[K] = {
    l.length match {
      case 0 =>
        None
      case _ =>
        Some(l(new Random().nextInt(l.length)))
    }
  }
}

class Frequency[C](val frequencies: Map[C,Int]) {
  def this() = this(Map[C,Int]())

  def add(v: C): Frequency[C] = {
    val old = frequencies.getOrElse(v,0)
    new Frequency(frequencies + (v -> (old + 1)))
  }

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

  override def toString = frequencies.toString
}

class StateStorage[C] (val frequencies: Map[(C,C), Frequency[C]]) {
  def this() = this(Map[(C,C), Frequency[C]]())

  def add(k:(C,C), v:C):StateStorage[C] = {
    val old = frequencies.getOrElse(k, new Frequency[C]())
    new StateStorage[C](frequencies + (k -> old.add(v)))
  }

  def insert(w: List[C]) = {
    w.sliding(3)
     .foldLeft(this)((acc, l) =>
       l match {
         case List(x1, x2, x3) =>
           acc.add((x1, x2), x3)
         case _ =>
           acc
       }
     )
  }

  def keys = frequencies.keys

  def next(seed: (C,C)): Option[C] = {
    for (l <- frequencies.get(seed);
         n <- l.choice)
    yield {
       n
    }
  }

  override def toString = frequencies.toString
}

class MarkovChain[C](val start: C,
                     val stop: C,
                     val edges: StateStorage[C]) {

  def this(start: C, stop: C) =
    this(start, stop, new StateStorage[C]())

  def insert(w: List[C]) = {
    new MarkovChain(start, stop, edges.insert((start :: w) :+ stop))
  }

  def randomKey =
    M.random(edges.keys.filter { case (x1, x2) => x1 == start }.toList)

  def generate(maxLength: Int): Option[List[C]] = {
    def inner(count: Int, acc: List[C], previous: (C,C)): List[C] = {
      if (count > 0) {
        edges.next(previous) match {
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

    for (k <- randomKey) yield {
      k match {
        case s @ (x1, x2) =>
          inner(maxLength, List(x2), s)
      }
    }
  }
}
