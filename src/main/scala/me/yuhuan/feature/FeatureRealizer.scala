package me.yuhuan.feature

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class FeatureRealizer {
  val ab = Alphabet.emtpy

  def freeze(): Unit = ab.freeze()
  def thaw(): Unit = ab.thaw()

  def realize[X](fv: FeatureVector[X]): SparseVector = {
    SparseVector(fv.features.map(f ⇒ ab.word2Index(f.name + f.value) → f.amount).toSeq: _*)
  }
}
