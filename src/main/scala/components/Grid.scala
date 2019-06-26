package gridcomponent.components

import monix.execution.Ack.Continue
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html
import shapeless._
import shapeless.record._

import monix.execution.Scheduler.Implicits.global

import outwatch.dom._
import outwatch.dom.dsl._

class Grid(colFmt: Seq[ValCol]) extends GridEditable(colFmt) with  FormatNumber {

  def titleTbl(titles: Seq[ValCol]) =
    titles.sortWith((prev, next) => prev.col < next.col) map(i => td(i.title, i.styleTitle))

  def showDetails(l: Seq[Map[String, Any]]) = l.map { i =>
      tr(id := "row", //+ i.ejercicio.toString + i.no_pedido,
        newRow (
          colFmt.map { ii =>
            val valor = i.getOrElse(ii.field, "") match {
                case Some(v) => v.toString
                case Fechas(v) => v.toString
                case v: Double if ii.typeNumber == "money" => fmtMiles(v)
                case v: Any => v.toString
                case v => v.toString
            }
            CellVal(valor, styles = ii.styleCell)
          }
        )
      )
    }

  /*val vere = Sink.create[dom.KeyboardEvent]{ k: dom.KeyboardEvent =>
    dataState.onNext(AddRow(Map("ejercicio" -> "2000")))
    Continue
  }*/

  //dataState.foreach{ r => println(r) }

//  def render(state: Observable[(ActionsApp, AppState)], store: Observer[ActionsApp]): VDomModifier =
  def render(events: VDomModifier) =
    for {
      s <- dataState
    } yield {
      table( id := "tblGrid",
        thead( tr( titleTbl( colFmt ) ), height := "20px" ),
        tbody(
          events,
          showDetails(s._2.rows)
        )
      )
    }
}