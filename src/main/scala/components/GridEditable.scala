package gridcomponent.components

import monix.execution.Ack.Continue
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html
import outwatch.dom.{Sink, StaticVDomModifier, VDomModifier}
import outwatch.dom.dsl.{contentEditable, _}

import cats.implicits._

case class ValCol(col: Int, field: String, title:String, styleTitle: VDomModifier, styleCell: VDomModifier, typeNumber: String = "")

case class CellVal(txt: String, styles: VDomModifier)

abstract class GridEditable(colFmt: Seq[ValCol]) extends StateGrid {

  def newCell(cellVal: CellVal)  = td(cellVal.txt, cellVal.styles)

  def newRow(value: Seq[CellVal]) = value.map { cellVal => newCell(cellVal) }

  def cellValue(col: Int, txt: String, edit: Boolean)(implicit row: html.TableRow): html.TableCell = {
    val cell = row.insertCell( -1).asInstanceOf[html.TableCell]
    cell.innerHTML = txt
    cell.contentEditable = if(edit) "true" else "false"
    cell
  }

  val keyEnter = onKeyUp.filter { _.keyCode == KeyCode.Enter }

  case class CellKeyBoardEvent(cell: html.TableCell, key: Int)

  val keyInsert = onKeyUp.filter { k =>
      k.keyCode == KeyCode.Insert
    }.map { v =>
      val cellActive = v.target.asInstanceOf[ html.TableCell ]

      val t1 = cellActive.parentNode.asInstanceOf[html.TableRow]

      val t2 = t1.parentNode.asInstanceOf[html.Table]
      val tbl = dom.document.getElementById("tblGrid").asInstanceOf[html.Table]

      val newRow = t2.insertRow( -1 ).asInstanceOf[ html.TableRow ]

      colFmt.map { i =>
        val edit: Boolean = i.styleCell match {
          case modifier: StaticVDomModifier if modifier == contentEditable => modifier.asInstanceOf[Boolean]
          case _ => false
        }
        cellValue ( col = i.col, txt = "", edit = edit )(newRow)
      }

      /*val cellRenglon = cellValue( 1,   "", false )
      val cellClave = cellValue( 2,   "", true )
      val cellDescripcion = cellValue( 3,   "", false )
      val cellCantidad = cellValue( 4,   "", true )*/

      val nextRenglon = tbl.rows.length - 1
      //cellRenglon.textContent = nextRenglon.toString*/
      nextRenglon
    }


  /*val keyInsert = onKeyUp.filter { k => k.keyCode == KeyCode.Insert }.map { v =>
    val cellActive = v.target.value.asInstanceOf[ html.TableCell ]
    val t1 = cellActive.parentNode.asInstanceOf[html.TableRow]
    val t2 = t1.parentNode.asInstanceOf[html.Table]
    val tbl = dom.document.getElementById("tblRenglones").asInstanceOf[html.Table]
    implicit val newRow = t2.insertRow( -1 ).asInstanceOf[ html.TableRow ]
    val cellRenglon = cellValue( 1,   "", false )
    val cellClave = cellValue( 2,   "", true )
    val cellDescripcion = cellValue( 3,   "", false )
    val cellCantidad = cellValue( 4,   "", true )
    val nextRenglon = tbl.rows.length - 1
    cellRenglon.textContent = nextRenglon.toString
    nextRenglon
  }*/

  def MoveRowDonw(cell: html.TableCell, numbCol: Int) = {
    val tbl = dom.document.getElementById("tblRenglones").asInstanceOf[html.Table]
    val currRow = cell.parentNode.asInstanceOf[ html.TableRow ]
    val maxRows = tbl.rows.length
    if ( (currRow.rowIndex + 1) < maxRows ) {
      val nextRow = cell.parentNode.nextSibling.asInstanceOf[html.TableRow] //MoveRowDonw(cell)
      val nextCell = nextRow.cells(numbCol).asInstanceOf[html.TableCell]
      nextCell
    } else cell
  }

  sealed trait MoveCursor
  final case object CursLeft extends MoveCursor
  final case object CursRight extends MoveCursor
  final case object CursDown extends MoveCursor
  final case object CursUp extends MoveCursor

  def changePosCell(cellAct: html.TableCell, mov: MoveCursor) = {
    val tbl = dom.document.getElementById("tblRenglones").asInstanceOf[html.Table]
    val currRow = cellAct.parentNode.asInstanceOf[ html.TableRow ]
    val maxRows = tbl.rows.length

    mov match {
      case CursDown =>
        val nextCell = MoveRowDonw(cellAct, cellAct.cellIndex)
        nextCell.focus()
      case CursUp =>
        if ( currRow.rowIndex > 1) {
          val nextRow = cellAct.parentNode.previousSibling.asInstanceOf[html.TableRow]
          val nextCell = nextRow.cells(cellAct.cellIndex).asInstanceOf[html.TableCell]
          nextCell.focus()
        }
      case CursLeft =>
        if (cellAct.cellIndex >= 1) {
          val nextCell = cellAct.previousSibling.asInstanceOf[html.TableCell]
          nextCell.focus()
        }
      case CursRight => //El indice de las columnas empieza en 0
        val nextCell = cellAct.cellIndex match {
          case col if col >= 3 => MoveRowDonw(cellAct, 0)
          case _ => cellAct.nextSibling.asInstanceOf[ html.TableCell ]
        }
        nextCell.focus()
    }
  }

  val onKeyDownTable = Sink.create[CellKeyBoardEvent]{ v: CellKeyBoardEvent =>
    val cellActive =  v.cell
    val t1 = cellActive.parentNode.asInstanceOf[html.TableRow]
    val t2 = t1.parentNode.asInstanceOf[html.Table]
    if (v.key == 38) changePosCell( cellActive, CursUp )
    else if (v.key == 40) changePosCell( cellActive, CursDown )
    else if (v.key == 37) changePosCell( cellActive, CursLeft )
    else if (v.key == 39) changePosCell( cellActive, CursRight )
    Continue
  }
}
