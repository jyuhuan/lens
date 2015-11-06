package me.yuhuan.feature

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait Featurizer[-X, +Y] extends (X ⇒ FeatureVector[Y]) { outer ⇒
  def apply(x: X): FeatureVector[Y] = this featurize x
  def featurize(x: X): FeatureVector[Y]

  def map[Z](newName: String)(f: Y ⇒ Z): Featurizer[X, Z] = new Featurizer[X, Z] { inner ⇒
    def featurize(x: X): FeatureVector[Z] = outer.featurize(x).map(newName)(f)
  }

  def catToNum(newName: String)(f: Y ⇒ Double): Featurizer[X, Unit] = new Featurizer[X, Unit] {
    def featurize(x: X) = outer.featurize(x).catToNum(newName)(f)
  }

  def ++[X1 <: X, Y1 >: Y](that: Featurizer[X1, Y1]): Featurizer[X1, Y1] = new Featurizer[X1, Y1] { inner ⇒
    def featurize(x: X1): FeatureVector[Y1] = outer.featurize(x) ++ that.featurize(x)
  }
}
