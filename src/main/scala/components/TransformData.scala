package gridcomponent.components

import shapeless._
import shapeless.record._

case class Fechas(fecha: String)

trait TransformData[A] {
  def transform(a: A): Map[String, Any]
}

object TransformData {
  implicit def genericTransform[A, ARepr <: HList](implicit gen: LabelledGeneric.Aux[A, ARepr],
                                                   toMap: ops.record.ToMap[ARepr]
                                                  ): TransformData[A] = new TransformData[A] {
    override def transform(rpt: A): Map[String, Any] = {

      val genRecord = gen.to(rpt)

      toMap(genRecord).map { case (k: Symbol, v) =>

        val valor =   v match {
          case Fechas(f) => f
          case v => v
        }

        /*case Some(v) => v.toString
        case Fechas(v) => v.toString
        case v: Double if ii.typeNumber == "money" => fmtMiles(v)
        case v: Any => v.toString
        case v => v.toString*/

        k.name.toString -> valor

      }

    }
  }
  def apply[A](rpt: A)(implicit transformer: TransformData[A]): Map[String, Any] = transformer.transform(rpt)
}