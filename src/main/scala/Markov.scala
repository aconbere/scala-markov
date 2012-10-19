package com.conbere.markov

import scala.util.Random
import scala.io.Source

trait MarkovChain[C,T] {
  val chain: Map[List[C], T]
  val start: C
  val stop: C
  val chainLength: Int

  def add(k: List[C], v: C): MarkovChain[C,T]
  def choice(x: T): Option[C]

  def contains(k: List[C]) = chain.contains(k)
  def random[K](l: List[K]) = l(new Random().nextInt(l.length))

  def seed = random(chain.keys.filter(x => x.head == start).toList)

  def next(seed: List[C]): Option[C] = {
    for (l <- chain.get(seed);
         n <- choice(l))
    yield {
       n
    }
  }

  def insert(word: List[C]) = {
    val w = (start :: word) :+ stop

    w.sliding(chainLength + 1)
     .foldLeft(this)((acc, l) =>
       if (l.length == chainLength + 1)
         acc.add(l.slice(0, chainLength), l.last)
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

class MarkovChainList[C](val start: C,
                         val stop: C,
                         val chainLength: Int,
                         val chain: Map[List[C], List[C]])
extends MarkovChain[C, List[C]] {
  def this(start: C, stop: C, chainLength: Int) =
    this(start, stop, chainLength, Map[List[C], List[C]]())

  def add(k: List[C], v: C) = {
    val value = chain.getOrElse(k, List()) :+ v
    val newChain = chain + (k -> value)
    new MarkovChainList[C](start, stop, chainLength, chain + (k -> value))
  }

  def choice(l: List[C]): Option[C] = Some(random(l))
}

class MarkovChainMap[C](val start: C,
             val stop: C,
             val chainLength: Int,
             val chain: Map[List[C], Map[C,Int]])
extends MarkovChain[C, Map[C, Int]] {
  def this(start: C, stop: C, chainLength: Int) =
    this(start, stop, chainLength, Map[List[C], Map[C,Int]]())

  def add(k: List[C], v: C) = {
    val m = chain.getOrElse(k, Map[C,Int]())
    val newM = m + (v -> (m.getOrElse(v, 0) + 1))
    new MarkovChainMap[C](start, stop, chainLength, chain + (k -> newM))
  }

  def choice(m: Map[C,Int]): Option[C] = {
    def inner(l: List[(C,Int)], c: Int): Option[C]= {
      l match  {
        case (char, i) :: Nil =>
          Some(char)
        case (char, i) :: xs =>
          val n = (c - i)
          if (n <= 0)
            Some(char)
          else
            inner(l.tail, n)
        case Nil =>
          None
      }
    }
    inner(m.toList, new Random().nextInt(m.values.sum))
  }
}
