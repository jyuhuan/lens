package me.yuhuan.lens

import scala.StringBuilder
import scala.collection.mutable

/**
 * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 10/6/15.
 */
class Alphabet(var isFrozen: Boolean, w2i: mutable.HashMap[String, Int], i2w: mutable.HashMap[Int, String]) extends (String ⇒ Int) {

  val indexForUnknowns = 0
  var lastIndex: Int = indexForUnknowns

  def apply(word: String): Int = this word2Index word
  def apply(indexedWord: Int): String = this index2Word indexedWord

  /**
   * Freeze the alphabet.
   * If the alphabet is frozen, no new words will be added.
   * Unknown words will have 0 as the index.
   */
  def freeze(): Unit = isFrozen = true
  def thaw(): Unit = isFrozen = false

  def size: Int = lastIndex + 1

  def word2Index(word: String): Int = {

    if (w2i contains word) w2i(word)
    else {
      if (isFrozen) indexForUnknowns
      else {
        lastIndex += 1
        w2i(word) = lastIndex
        i2w(lastIndex) = word
        lastIndex
      }
    }
  }

  def indices = 0 until size

  def index2Word(indexedWord: Int): String = i2w(indexedWord)

  override def toString = {
    val sb = new StringBuilder()
    for ((i, w) ← i2w) {
      sb append i
      sb append ":\t"
      sb append w
      sb append "\n"
    }
    sb.toString()
  }
}

object Alphabet {
  def emtpy: Alphabet = new Alphabet(isFrozen = false, new mutable.HashMap[String, Int](), new mutable.HashMap[Int, String]())
}
