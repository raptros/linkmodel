package local.nodens.linkmodel
/**
 * Base Manipulator. Defines the in-place versions of all the manipulation operators.
 * Defined seperately both for clarity and for letting Document be implicitely converted without trouble.
 * The names of the in-place operations are the names of the Manipulate operations with >.
 * @param manip An Option[Manipulator] to apply computations to.
 * @see Manipulate for some explanation
 * @note Implicit conversions to this are defined.
 */
class BaseManipulate(manip:Option[Manipulator]) {
  /**
   * The key to in-place manipulations. 
   * Basically, it applies the computation to manip and then re-enters the initial path.
   * @param comp An operation on a manipulator.
   * @return A Manipulator for the same path as the original manip, based of the computation's result, inside the Option monad. 
   */
  def applyInPlace(comp: Manipulator => Option[Document]):Option[Manipulator] = for {
    m <- manip
    d <- comp(m)
  } yield (d /~ m.path.tail)
  
  /*def applyInPlace(comp: Manipulator => Option[Document]):Option[Manipulator] = manip flatMap {
    m => comp(m) map (_ /~ m.path.tail)
  }*/

  def +/>(name:String):Option[Manipulator] = applyInPlace(_ +/ name) / name
  def +#@>(link:Link):Option[Manipulator] = applyInPlace(_ +#@ link)
  def +#@@>(links:Seq[Link]):Option[Manipulator] = applyInPlace(_ +#@@ links)
  def +#@#@@>(i:Int, link:Link):Option[Manipulator] = applyInPlace(_ +#@#@@(i, link))
  def -/>(name:String):Option[Manipulator] = applyInPlace(_ -/ name)
  def -#@>(i:Int):Option[Manipulator] = applyInPlace(_ -#@ i)
  def -#@@>(i:Int):Option[Manipulator] = applyInPlace(_ -#@@ i)
  def -#@#@@>(l:Int, ls:Int):Option[Manipulator] = applyInPlace(_ -#@#@@(l, ls))
  def ~#@>(iOld:Int, iNew:Int):Option[Manipulator] = applyInPlace(_ ~#@(iOld, iNew))
  def ~#@@>(iOld:Int, iNew:Int):Option[Manipulator] = applyInPlace(_ ~#@@(iOld, iNew))
  def ~#@#@@>(iOld:Int, iNew:Int, ls:Int):Option[Manipulator] = applyInPlace(_ ~#@#@@(iOld, iNew, ls))
  def ~/>(name:String, newName:String):Option[Manipulator] = applyInPlace(_ ~/(name, newName))
  def ~/>(rename:(String,String)):Option[Manipulator] = applyInPlace(_ ~/ rename)
}

/**
 * The Manipulate class allows chaining of Manipulator operations by operating 
 * inside the Option monad. There are basically two kinds of operators:
 *  simple operators, which simply obtain information from the current Document, and 
 *  operations, which generate a new Document.
 * Remember that all of this happens in the option monad, so applying getOrElse at
 * the end of the chain is advised. I'd also recommend using the original Document 
 * as or-else.
 * @note Short version: extends all of the operators in Manipulator to operate inside Option.
 * @note Implicit conversions to this are defined
 * @param manip Manipulator (in Option) to apply computations to.
 * @see Manipulator for operation definitions
 * @see BaseManipulate for in place operations
 */
case class Manipulate(manip:Option[Manipulator]) extends BaseManipulate(manip) {
  //mapping versions of everything in Manipulator
  def /(secName:String):Option[Manipulator] = manip.map(_ / secName)
  def /~(path:Seq[String]):Option[Manipulator] = manip.map(_ /~ path)
  def #@(i:Int):Option[Link] = manip.flatMap(_ #@ i)
  def #@@(i:Int):Option[LinkSeq] = manip.flatMap(_ #@@ i)
  def #@#@@(l:Int, ls:Int):Option[Link] = manip.flatMap(_ #@#@@(l, ls))
  def links:Seq[Link] = manip.map(_.links).toList.flatten
  def linkSeqs:Seq[LinkSeq] = manip.map(_.linkSeqs).toList.flatten
  def renderXML = manip.map(_.renderXML)
  def path = manip.map(_.path)
  def pathStr = manip.map(_.pathStr)
  def +/(name:String):Option[Document] = manip.flatMap(_ +/ name)
  def +#@(link:Link):Option[Document] = manip.flatMap(_ +#@ link)
  def +#@@(links:Seq[Link]):Option[Document] = manip.flatMap(_ +#@@ links)
  def +#@#@@(i:Int, link:Link):Option[Document] = manip.flatMap(_ +#@#@@(i, link))
  def -/(name:String):Option[Document] = manip.flatMap(_ -/ name)
  def -#@(i:Int):Option[Document] = manip.flatMap(_ -#@ i)
  def -#@@(i:Int):Option[Document] = manip.flatMap(_ -#@@ i)
  def -#@#@@(l:Int, ls:Int):Option[Document] = manip.flatMap(_ -#@#@@(l, ls))
  def ~#@(iOld:Int, iNew:Int):Option[Document] = manip.flatMap(_ ~#@(iOld, iNew))
  def ~#@@(iOld:Int, iNew:Int):Option[Document] = manip.flatMap(_ ~#@@(iOld, iNew))
  def ~#@#@@(iOld:Int, iNew:Int, ls:Int):Option[Document] = manip.flatMap(_ ~#@#@@(iOld, iNew, ls))
  def ~/(name:String, newName:String):Option[Document] = manip.flatMap(_ ~/(name, newName))
  def ~/(rename:(String,String)):Option[Document] = manip.flatMap(_ ~/ rename)
}

