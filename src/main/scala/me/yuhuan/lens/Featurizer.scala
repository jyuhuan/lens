package me.yuhuan.lens

import scala.collection.mutable

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

  def andThen(fs: Featurizer[Y, Any]*): FeatureExtractor[X] = new FeatureExtractor[X] {
    def featurizers: Iterable[Featurizer[X, Any]] = ???

    val cache = mutable.HashMap[X, FeatureGroup[Y]]()

    override def apply(x: X): FeatureVector = {
      val ys: FeatureGroup[Y] = cache.getOrElseUpdate(x, outer.featurize(x))

      val groups: Iterable[FeatureGroup[Any]] = for (f ← fs) yield new FeatureGroup[Any] {
        def name: String = s"${outer.name}$$${f.name}"
        def values: Iterable[(Any, Double)] = for {
          (yv, ya) ← ys.values
          (zv, za) ← f.featurize(yv).values
        } yield (zv, ya * za)
      }

      new FeatureVector {
        def features: Iterable[Feature[Any]] = groups.flatMap(g ⇒ g.values.map {
          case (v, a) ⇒ Feature(g.name, v, a)
        })
      }
    }
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
