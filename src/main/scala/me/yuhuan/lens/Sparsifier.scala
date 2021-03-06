package me.yuhuan.lens
import me.yuhuan.math.linalg.vector.SparseVector

/**
 * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 11/19/15.
 */
class Sparsifier {
  val ab = Alphabet.empty

  def freeze(): Unit = ab.freeze()
  def thaw(): Unit = ab.thaw()

  def sparsify[X](fs: Iterable[Feature[X]]): SparseVector = {
    SparseVector(fs.map { case Feature(n, v, a) => ab.word2Index(s"$n$v") -> a }.toSeq: _*)
  }

  def curMaxDimension = ab.size

  def apply[X](fs: Iterable[Feature[X]]): SparseVector = sparsify(fs)

}
