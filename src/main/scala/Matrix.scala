/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 12/17/12
 * Time: 1:01 PM
 * To change this template use File | Settings | File Templates.
 */
class Matrix[T: Numeric](protected val vectors: Seq[Seq[T]]) {
  import Numeric.Implicits._
  require(vectors.forall(_.length == vectors.head.length))

  def update(x: Int, y: Int, value: T) = new Matrix(vectors.updated(x,vectors(y).updated(x,value)))

  def dimensions = (vectors.length, vectors.head.length)

  def row(num: Int) = vectors(num)

  def column(num: Int) = vectors map (_(num))

  def +(m: Matrix[T]) = {
    require(m.dimensions == dimensions)
    new Matrix((m.vectors zip vectors) map { case(a,b) => (a zip b) map {case(a,b) => a + b } })
  }

  def *(s: T) = new Matrix(vectors map (_ map (_ * s)))

  def *(m: Matrix[T]) = {
    require(m.dimensions._2 == dimensions._1, f"Matrix m needs to have dimensions nx${dimensions._1}, is ${m.dimensions._1}x${m.dimensions._2}")
    new Matrix(
      (0 until dimensions._1) map (x =>
        (0 until dimensions._2) map (y =>
          (this.row(x) zip m.column(y)).foldLeft(implicitly[Numeric[T]].fromInt(0): T){case (rest, (a,b)) => rest + (a*b)}
        )
      )
    )
  }

  def -(m: Matrix[T]) = {
    val im = implicitly[Numeric[T]]
    this + (m * im.fromInt(-1))
  }

  def apply(x: Int, y: Int) = {
    require(y < vectors.length && y >= 0 && x < vectors.head.length && x >= 0)
    vectors(y)(x)
  }

  override def toString = "[ [" + vectors.foldLeft("")((str,value) => str + value.foldLeft("")((str,value) => str + value + " ").dropRight(1) + "]\n  [").dropRight(4) + " ]"
}

object Matrix {
  def createMatrixOf[T: Numeric](value: T, rows: Int, columns: Int) = {
    require(rows > 0 && columns > 0)
    new Matrix[T]((0 until rows) map (_ => (0 until columns) map (_ => value)))
  }

  def apply[T: Numeric](vs: Seq[T]*): Matrix[T] = new Matrix(vs)
}
