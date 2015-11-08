package me.yuhuan.lens

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait FeatureGroup[+X] { outer ⇒
  def name: String
  def values: Iterable[(X, Double)]

  def map[Y](newName: String)(f: X ⇒ Y): FeatureGroup[Y] = new FeatureGroup[Y] {
    def name = newName
    def values: Iterable[(Y, Double)] = outer.values.map{ case (v, a) ⇒ (f(v), a) }
  }

  def flatMap[Y](f: Featurizer[X, Y]) = new FeatureGroup[Y] {
    def name = f.name
    def values: Iterable[(Y, Double)] = for {
      (v1, a1) ← outer.values
      (v2, a2) ← f(v1).values
    } yield (v2, a1 * a2)
  }

  def catToNum(newName: String)(f: X ⇒ Double) = new FeatureGroup[Unit] {
    def name = newName
    def values = outer.values.map { case (v, _) ⇒ ((), f(v)) }
  }

  override def toString = values.map{ case (v, a) ⇒ s"$name=$v:$a" }.mkString("; ")
}

object FeatureGroup {
  def apply[X](_name: String)(fs: Iterable[(X, Double)]) = new FeatureGroup[X] {
    def name = _name
    def values: Iterable[(X, Double)] = fs
  }

  def ofCats[X](name: String)(cs: X*) = FeatureGroup(name)(cs.map(x ⇒ x → 1.0))
  def ofNums(name: String)(ds: Double*) = FeatureGroup(name)(ds.map(d ⇒ () → d))
}
