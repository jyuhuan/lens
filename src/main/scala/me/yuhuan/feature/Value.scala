package me.yuhuan.feature

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
sealed trait Value[+X] {
  def value: X

  override def toString = value.toString
}

case class CategoricalValue[+X](value: X) extends Value[X]
case class NumericalValue[+X](value: X) extends Value[X]
