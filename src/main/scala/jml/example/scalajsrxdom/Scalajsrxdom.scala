package jml.example.scalajsrxdom

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import scalatags.JsDom.all._
import pl.metastack.metarx.{Array=>RxArray, _}


object Scalajsrxdom extends js.JSApp {

  implicit def RxMod[T <: scalatags.jsdom.Frag](channel: ReadChannel[T]): scalatags.JsDom.Modifier = {
    return new scalatags.JsDom.Modifier() {
      override def applyTo(b: org.scalajs.dom.Element): Unit = {
        var node: org.scalajs.dom.Node = b.appendChild(
          org.scalajs.dom.document.createComment("")
        )
        channel.attach( elem=> {
          val next = elem.render
          node.parentNode.replaceChild(next, node)
          node = next
        })
        // MutationObserverInit()
      }
    }
  }

  implicit def RxBuf[T <: scalatags.jsdom.Frag](buffer: DeltaBuffer[T]): scalatags.JsDom.Modifier = {
    return new scalatags.JsDom.Modifier() {
      override def applyTo(b: org.scalajs.dom.Element): Unit = {
        var beg: org.scalajs.dom.Node = b.appendChild(
          org.scalajs.dom.document.createComment("")
        )
        var end: org.scalajs.dom.Node = b.appendChild(
          org.scalajs.dom.document.createComment("")
        )
        import pl.metastack.metarx.Buffer.{Delta, Position}
        buffer.map(_.render).changes.attach {
          case Delta.Insert(Position.Head(), elem) => beg.parentNode.insertBefore(elem.render, beg.nextSibling)
          case Delta.Insert(Position.Last(), elem) => end.parentNode.insertBefore(elem.render, end)
          case Delta.Insert(Position.Before(ref), elem) => beg.parentNode.insertBefore(elem.render, ref)
          case Delta.Insert(Position.After(ref), elem) => beg.parentNode.insertBefore(elem.render, ref.nextSibling)
          case Delta.Remove(elem) => elem.parentNode.removeChild(elem)
          case Delta.Replace(ref, elem) => elem.parentNode.replaceChild(elem, ref)
          case Delta.Clear() => {
            var node: org.scalajs.dom.Node = beg;
            while( node!=end ) node.parentNode.removeChild(node)
          }
        }
      }
    }
  }  

  implicit def RxAttrValue[T: AttrValue] = new AttrValue[ReadChannel[T]]{
    def apply(elem: org.scalajs.dom.Element, attr: Attr, channel: ReadChannel[T]): Unit = {
      channel.attach( value =>
        implicitly[AttrValue[T]].apply(elem, attr, value)
      )
    }
  }

  implicit def RxStyleValue[T: StyleValue] = new StyleValue[ReadChannel[T]]{
    def apply(elem: org.scalajs.dom.Element, style: Style, channel: ReadChannel[T]): Unit = {
      channel.attach( value=>
        implicitly[StyleValue[T]].apply(elem, style, value)
      )
    }
  }

  def main(): Unit = {
    val text = Var("HELLO")
    val list = Buffer(1,2)
    dom.document.body.appendChild( div(
      p(
        text.map(span(_)), 
        " - ",
        list.size.map(span(_))
      ),
    	ul(
        list.map(x=>li(
          x.toString,
          button(onclick := {
            ()=> list -= x;
          }, "DROP")
        ))
    	),
      button(
        id:="add-digit",
        onclick :=  { 
          ()=> list += (list.get.foldLeft(0)(Math.max(_,_))+1)
        },
        "ADD DIGIT"
      ),
      table(`class`:="table table-bordered",
        list.map(x=> tr(td(x), td(x), td(x), td(x), td(x), td(x), td(x)))
      )
    ).render)
    text := "HELLO WORLD"
  }

}


