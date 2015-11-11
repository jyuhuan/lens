package me.yuhuan.lens

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */

trait SimpleFeatureExtractor[-X] { outer ⇒
  def apply(x: X): FeatureVector

  def ++[Y <: X](that: SimpleFeatureExtractor[Y]) = new SimpleFeatureExtractor[Y] {
    override def apply(y: Y): FeatureVector = {
      outer(y) ++ that(y)
    }
  }
}

object SimpleFeatureExtractor {
  def apply[X](fxs: SimpleFeatureExtractor[X]*) = new SimpleFeatureExtractor[X] {
    def apply(x: X): FeatureVector = fxs.reduce(_ ++ _)(x)
  }
}

trait FeatureExtractor[-X] extends SimpleFeatureExtractor[X] { outer ⇒
  val featurizers: Iterable[Featurizer[X, Any]]

  def apply(x: X): FeatureVector = {
    val features = featurizers.flatMap(f ⇒ f(x).values.map { case (v, a) ⇒ Feature(f.name, v, a) })
    FeatureVector(features.toSeq: _*)
  }
}

object FeatureExtractor {
  def apply[X](fs: Featurizer[X, Any]*) = new FeatureExtractor[X] {
    val featurizers: Iterable[Featurizer[X, Any]] = fs
  }
}
