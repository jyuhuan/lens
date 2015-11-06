package me.yuhuan.feature

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait Feature[+X] {
  def name: String
  def value: X
  def amount: Double

  override def toString = value match {
    case v: Unit ⇒ s"$name:$amount"
    case _ ⇒ s"$name=$value:$amount"
  }
}

object Feature {
  def apply[X](_name: String, _value: X, _amount: Double) = new Feature[X] {
    def name = _name
    def value = _value
    def amount = _amount
  }

  def unapply[X](f: Feature[X]): Option[(String, X, Double)] = Some(f.name, f.value, f.amount)

  def Cat[X](_name: String, _value: X) = new Feature[X] {
    def name = _name
    def value = _value
    def amount = 1.0
  }

  def Num(_name: String, _amount: Double) = new Feature[Unit] {
    def name = _name
    def value = ()
    def amount = _amount
  }
}

//case class CategoricalFeature[+X](name: String, value: CategoricalValue[X], amount: Double = 1.0) extends Feature[X]
//case class NumericalFeature[+X](name: String, value: NumericalValue[X]) extends Feature[X]