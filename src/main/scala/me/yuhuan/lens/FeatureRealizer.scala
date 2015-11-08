package me.yuhuan.lens

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class FeatureRealizer {
  val ab = Alphabet.empty

  def freeze(): Unit = ab.freeze()
  def thaw(): Unit = ab.thaw()

  def realize[X](fv: FeatureVector): SparseVector = {
    SparseVector(fv.features.map { case Feature(n, v, a) ⇒ ab.word2Index(n + v) → a }.toSeq: _*)
  }

  def apply[X](fv: FeatureVector) = this realize fv
}
