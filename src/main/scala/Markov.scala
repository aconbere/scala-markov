package org.conbere.markov

import scala.util.Random
import scala.io.Source

class Frequency[C](val transitions: Map[C,Int]) {
  def this() = this(Map[C,Int]())

  def add(v: C): Frequency[C] = {
    val old = transitions.getOrElse(v,0)
    new Frequency(transitions + (v -> (old + 1)))
  }

  def contains(v: C) = transitions.contains(v)

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
    inner(transitions.toList,
          new Random()
            .nextInt(transitions.values.sum))
  }

  override def toString = transitions.toString
}

class StateStorage[C] (val transitions: Map[(C,C), Frequency[C]], val startTransitions: Frequency[C], val startState: C) {

  def add(k:(C,C), v:C):StateStorage[C] = {
    val old = transitions.getOrElse(k, new Frequency[C]())
    new StateStorage[C](transitions + (k -> old.add(v)), startTransitions, startState)
  }

  def addStart(k :C):StateStorage[C] = {
    new StateStorage[C](transitions, startTransitions.add(k), startState)
  }

  def insert(w: List[C]) = {
    w.sliding(3)
     .foldLeft(this)((acc, l) =>
       l match {
         case List(`startState`, x2, x3) =>
           acc.add((startState, x2), x3).addStart(x2)
         case List(x1, x2, x3) =>
           acc.add((x1, x2), x3)
         case _ =>
           acc
       }
     )
  }

  def keys = transitions.keys

  def start: Option[C] = startTransitions.choice

  def next(seed: (C,C)): Option[C] = {
    for (l <- transitions.get(seed);
         n <- l.choice)
    yield {
       n
    }
  }

  override def toString = transitions.toString
}

class MarkovChain[C](val start: C,
                     val stop: C,
                     val edges: StateStorage[C]) {

  def this(start: C, stop: C) =
    this(start, stop, new StateStorage[C](Map[(C,C), Frequency[C]](), new Frequency[C](), start))

  def insert(w: List[C]) = {
    new MarkovChain(start, stop, edges.insert((start :: w) :+ stop))
  }

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

    for (k <- edges.start) yield {
      inner(maxLength, List(k), (start, k))
    }
  }
}
