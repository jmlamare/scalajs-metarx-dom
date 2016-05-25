package jml.example.scalajsrxdom
 
import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import scalatags.JsDom.all._
import pl.metastack.metarx.{Array=>RxArray, _}
 
 
object Scalajsrxdom extends js.JSApp {
 
  implicit def RxVal[T <: scalatags.jsdom.Frag](channel: ReadChannel[T]): scalatags.JsDom.Frag = {
    return new scalatags.JsDom.Frag() {
      var node: org.scalajs.dom.Node = org.scalajs.dom.document.createComment("")
      channel.attach( elem=> {
        if( node.parentNode!=null ) {
          val temp = elem.render
          node.parentNode.replaceChild(temp, node)
          node = temp
        }
        else node = elem.render
      })
      override def render = node
      override def applyTo(b: org.scalajs.dom.Element): Unit = b.appendChild(node)
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

  def lup[T](b:Array[Array[T]])(implicit frac: scala.math.Fractional[T], tag: scala.reflect.ClassTag[T]): Option[(Array[Array[T]], Array[Int])] = {
    import frac._
    b.map(x=>println(x.mkString(",")))
    println("--")
    val perm: Array[Int] = (0 to b.length-1).toArray
    def get(lin:Int, col:Int): T = b(perm(lin))(col)
    def set(lin:Int, col:Int, cell:T) = b(perm(lin))(col) = cell
    for(pos<-0 to b.length-2) {
      val max:Int = Range(pos, b.length-1).maxBy(get(_,pos))
      println(pos + "->" + max)
      val (rp,rm) = (perm(pos), perm(max))
      perm(pos)=rm
      perm(max)=rp
      if( frac.compare(get(pos, pos), frac.zero)==0 ) return None
      for(r<-pos+1 to b.length-1) {
        set(r, pos, get(r,pos) / get(pos, pos))
        for(c<-pos+1 to b.length-1) set(r, c, get(r, c) - get(r, pos) * get(pos, c))
      }
    }
    return if( frac.compare(get(b.length-1, b.length-1),frac.zero)==0 ) None else Some(b, perm)
  }
   
  def solve[T](a:Array[Array[T]], b:Array[T])(implicit frac: scala.math.Fractional[T], tag: scala.reflect.ClassTag[T]): Option[Array[T]] = lup(a) match {
    case Some( (lu, p) ) => {
      import frac._
      val y = Array.fill(b.length)(frac.zero)
      val x = Array.fill(b.length)(frac.zero)
      for(i <- Range(0,b.length)) y(i) = b(p(i)) - Range(0,i).map(j=>lu(i)(j) * y(j)).sum
      for(i <- b.length-1 to 0 by -1) x(i) = (y(i) - Range(i+1,b.length).map(j=>lu(i)(j) * x(j)).sum) / lu(i)(i)
      return Some(x)
    }
    case _ => return None
  }
 
  def main(): Unit = {
    val list = Buffer(1,2,2,3)
    val size = Var[Int](3)
    
    dom.document.body.appendChild( div(
      /*
      p(
        list.size.map(span(_))
      ),
      button(
        id:="add-digit",
        onclick :=  {
          ()=> list += (list.get.foldLeft(0)(Math.max(_,_))+1)
        },
        "ADD DIGIT"
      ),
      ul(
        list.map(x=>li(
          x.toString,
          button(
            onclick := {
              ()=> list -= x;
            },
            "DROP"
          )
        ))
      ),
      */
      h2(
        "Matrix Size ",
        input(
          "Size", 
          value := size.get,
          onchange := { (event: org.scalajs.dom.Event)=>
            size := event.currentTarget.asInstanceOf[org.scalajs.dom.html.Input].value.toInt
          }
        )
      ),
      size.map(size =>{
        val matrix: Dict[(Int,Int),Rational] = Dict((1 to size).flatMap(x=>(1 to size).map(y=>(x,y)->Rational(if(x==y) 1 else 0))).toMap)
        val vector: Dict[Int, Rational] = Dict((1 to size).map(x=>x->Rational(x)).toMap)
        val result = matrix.changes.foldLeft(Array.ofDim[Rational](size,size))((matrix,change)=> change match {
          case pl.metastack.metarx.Dict.Delta.Insert((r,c),v)=>matrix.updated(r-1, matrix(r-1).updated(c-1, v))
          case pl.metastack.metarx.Dict.Delta.Update((r,c),v)=>matrix.updated(r-1, matrix(r-1).updated(c-1, v))
          case pl.metastack.metarx.Dict.Delta.Remove((r,c))=>matrix.updated(r-1, matrix(r).updated(c-1, Rational(0)))
          case pl.metastack.metarx.Dict.Delta.Clear()=>Array.ofDim[Rational](size,size)
        }).zip(vector.changes.foldLeft(Array.ofDim[Rational](size))((vector,change)=> change match {
          case pl.metastack.metarx.Dict.Delta.Insert(r,v)=>vector.updated(r-1, v)
          case pl.metastack.metarx.Dict.Delta.Update(r,v)=>vector.updated(r-1, v)
          case pl.metastack.metarx.Dict.Delta.Remove(r)=>vector.updated(r-1, Rational(0))
          case pl.metastack.metarx.Dict.Delta.Clear()=>Array.ofDim[Rational](size)
        })).map { 
          case (matrix, vector)=> solve(matrix.map(_.clone), vector)
        }

        def handler[T](set:Dict[T, Rational], key:T) = {
          (event: org.scalajs.dom.Event) => Rational.unapply(event.currentTarget.asInstanceOf[org.scalajs.dom.html.Input].value) match {
            case Some(x) => set.update(key, x)
            case None => event.currentTarget.asInstanceOf[org.scalajs.dom.html.Input].value = set.get.apply(key).toString
          }
        }
        table(`class`:="table table-bordered", (1 to size).map(row=>
          tr(
            (1 to size).map(col=>
              td( display:="inline-block",
                input(
                  width:="40px",
                  value:=matrix.get.apply((row,col)).toString,
                  onchange:=handler(matrix, (row,col))
                ),
                " * x" + col + (if(col==size) " = " else " + ")
              )
            ),
            td( display:="inline-block",
              input( 
                width:="40px",
                value:=vector.get.apply(row).toString,
                onchange:=handler(vector, row)
              )
            ),
            td( display:="inline-block",
              ";      x" + row + " = ",
              result.map(r=>span(r.map(_.apply(row-1).toString)))
            )
          )
        ))
      })

    ).render);
  }
 
}
 