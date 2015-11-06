package me.yuhuan.lens

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait Featurizer[-X, +Y] extends (X ⇒ FeatureGroup[Y]) { outer ⇒
  def name: String

  def apply(x: X): FeatureGroup[Y] = this featurize x
  def featurize(x: X): FeatureGroup[Y]

  def map[Z](newName: String)(f: Y ⇒ Z): Featurizer[X, Z] = new Featurizer[X, Z] { inner ⇒
    def name = newName
    def featurize(x: X): FeatureGroup[Z] = outer.featurize(x).map(newName)(f)
  }

  def andThen[Z](f: Featurizer[Y, Z]): Featurizer[X, Z] = new Featurizer[X, Z] {
    def name = outer.name + f.name
    def featurize(x: X): FeatureGroup[Z] = outer.featurize(x).flatMap(f)
  }

  def catToNum(newName: String)(f: Y ⇒ Double): Featurizer[X, Unit] = new Featurizer[X, Unit] {
    def name = newName
    def featurize(x: X) = outer.featurize(x).catToNum(newName)(f)
  }

//  def ++[X1 <: X, Y1 >: Y](that: Featurizer[X1, Y1]): Featurizer[X1, Y1] = new Featurizer[X1, Y1] { inner ⇒
//    def name = s"${outer.name}$$${that.name}"
//    def featurize(x: X1): FeatureGroup[Y1] = outer.featurize(x) ++ that.featurize(x)
//  }
}
