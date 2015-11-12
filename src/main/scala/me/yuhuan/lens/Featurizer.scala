package me.yuhuan.lens

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait Featurizer[-X, Y] { outer =>
  def apply(x: X): Iterable[Feature[Y]]

  def ~~>[Z](fs: Featurizer[Y, Z]*): Featurizer[X, Z] = new Featurizer[X, Z] {
    def apply(x: X): Iterable[Feature[Z]] = {
      val outerResult = outer(x)
      for {
        Feature(n1, v1, a1) <- outerResult
        f <- fs
        Feature(n2, v2, a2) <- f(v1)
      } yield Feature(n1 + "$$" + n2, v2, a1 * a2)
    }
  }

  def ++[X1 <: X](f: Featurizer[X1, Y]): Featurizer[X1, Y] = new Featurizer[X1, Y] {
    def apply(x: X1): Iterable[Feature[Y]] = f(x) ++ outer(x)
  }
}

object Featurizer {
  def Cat[X, Y](name: String)(f: X => Option[Y]): Featurizer[X, Y] = new Featurizer[X, Y] {
    def apply(x: X): Iterable[Feature[Y]] = f(x).map(y => Feature.Cat(name, y))
  }

  def Num[X](name: String)(f: X => Option[Double]): Featurizer[X, Unit] = new Featurizer[X, Unit] {
    def apply(x: X): Iterable[Feature[Unit]] = f(x).map(y => Feature.Num(name, y))
  }

  def Cats[X, Y](name: String)(f: X => Iterable[Y]): Featurizer[X, Y] = new Featurizer[X, Y] {
    def apply(x: X): Iterable[Feature[Y]] = f(x).map(y => Feature.Cat(name, y))
  }

  def Nums[X](name: String)(f: X => Iterable[Double]): Featurizer[X, Unit] = new Featurizer[X, Unit] {
    def apply(x: X): Iterable[Feature[Unit]] = f(x).map(y => Feature.Num(name, y))
  }
}
