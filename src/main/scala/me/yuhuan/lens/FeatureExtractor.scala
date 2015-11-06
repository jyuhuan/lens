package me.yuhuan.lens

import me.yuhuan.lens.Featurizer

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait FeatureExtractor[X] {
  def featurizers: Iterable[Featurizer[X, Any]]

  def apply(x: X): FeatureVector = {
    val features = featurizers.flatMap(f ⇒ f(x).values.map { case (v, a) ⇒ Feature(f.name, v, a) })
    FeatureVector(features.toSeq: _*)
  }
}

object FeatureExtractor {
  def apply[X](fs: Featurizer[X, Any]*) = new FeatureExtractor[X] {
    def featurizers: Iterable[Featurizer[X, Any]] = fs
  }
}
