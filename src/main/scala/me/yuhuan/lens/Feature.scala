package me.yuhuan.lens

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait Feature[X] {
  def name: String
  def value: X
  def amount: Double

  override def toString = s"$name=$value:$amount"
}

object Feature {
  def apply[X](_name: String, _value: X, _amount: Double): Feature[X] = new Feature[X] {
    def name: String = _name
    def value: X = _value
    def amount: Double = _amount
  }

  def Cat[X](_name: String, _value: X): Feature[X] = new Feature[X] {
    def name: String = _name
    def value: X = _value
    def amount: Double = 1.0
  }

  def Num(_name: String, _amount: Double): Feature[Unit] = new Feature[Unit] {
    def name: String = _name
    def value: Unit = ()
    def amount: Double = _amount
  }

  def unapply[X](x: Feature[X]): Option[(String, X, Double)] = Some((x.name, x.value, x.amount))
}