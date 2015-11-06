package me.yuhuan.feature

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */



trait FeatureVector[+X] { outer ⇒
  def features: Iterable[Feature[X]]

  def map[Y](newName: String)(f: X ⇒ Y): FeatureVector[Y] = new FeatureVector[Y] {
    def features: Iterable[Feature[Y]] = outer.features.map{ case Feature(_, v, a) ⇒ Feature[Y](newName, f(v), a) }
  }

  def catToNum(newName: String)(f: X ⇒ Double) = new FeatureVector[Unit] {
    def features = outer.features.map { case Feature(_, v, _) ⇒ Feature[Unit](newName, (), f(v)) }
  }

  def ++[Y >: X](that: FeatureVector[Y]): FeatureVector[Y] = new FeatureVector[Y] { inner ⇒
    def features: Iterable[Feature[Y]] = outer.features ++ that.features
  }

  override def toString = features.mkString("; ")
}

object FeatureVector {
  def apply[X](fs: Iterable[Feature[X]]) = new FeatureVector[X] {
    def features: Iterable[Feature[X]] = fs
  }
}
