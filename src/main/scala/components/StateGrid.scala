package gridcomponent.components

import outwatch.util.Store
import monix.execution.Scheduler.Implicits.global

trait StateGrid {

  val initialState = GridState()

  sealed trait ActionsGrid
  case object Clean extends ActionsGrid

  case class Update(rows: Seq[Map[String, Any]]) extends ActionsGrid
  case class AddRow(dat: Map[String, Any]) extends ActionsGrid

  case class GridState( rows: Seq[Map[String, Any]] = Seq.empty)

  val reduce: (GridState, ActionsGrid) => GridState = (s, a) => a match {
    case Clean => s.copy(rows = Seq.empty)
    case Update(datos) => s.copy(rows = datos)
    case AddRow(dat) =>
      val addedRows: Seq[Map[String, Any]] = s.rows ++: Seq(dat)
      s.copy(rows = addedRows)
  }

  val dataState = Store.create[ActionsGrid, GridState]( Clean, initialState, reduce ).unsafeRunSync()

}