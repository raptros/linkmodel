package local.nodens.linkmodel
class BaseManipulate(manip:Option[Manipulator]) {
  //special stuff only sensible here
  def applyInPlace(comp: Manipulator => Option[Document]):Option[Manipulator] = for {
    m <- manip
    d <- comp(m)
  } yield (d /~ m.path.tail)
  
  /*def applyInPlace(comp: Manipulator => Option[Document]):Option[Manipulator] = manip flatMap {
    m => comp(m) map (_ /~ m.path.tail)
  }*/

  //in place manipulations!
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
}

case class Manipulate(manip:Option[Manipulator]) extends BaseManipulate(manip) {
  //mapping versions of everything in Manipulator
  def /(secName:String):Option[Manipulator] = manip.map(_ / secName)
  def /~(path:Seq[String]):Option[Manipulator] = manip.map(_ /~ path)
  def #@(i:Int):Option[Link] = manip.flatMap(_ #@ i)
  def #@@(i:Int):Option[LinkSeq] = manip.flatMap(_ #@@ i)
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
}

abstract class Manipulator(val current:ContainsSections, val parents:List[Manipulator]) {
  //access subsection
  def /(secName:String):Manipulator = current.sections.get(secName).map {
    c => new SectionManipulator(c, this::parents)
  }.getOrElse(new NoneManipulator(secName, this::parents))
  
  //access a path of subsections
  def /~(path:Seq[String]):Manipulator = path.foldLeft(this) (_ / _)
  
  def getDoc:ContainsSections
  def renderXML = getDoc.renderXML

  //get the ith link
  def #@(i:Int):Option[Link] = links.lift(i)

  //get the ith link sequence
  def #@@(i:Int):Option[LinkSeq] = linkSeqs.lift(i)

  //updates the section heirarchy
  def renewChild(newChild:Option[Section]):Option[Document]

  //get list of links
  def links:Seq[Link]

  //get list of link sequences
  def linkSeqs:Seq[LinkSeq]

  //add a link
  def +#@(link:Link):Option[Document]

  //add a link sequence
  def +#@@(links:Seq[Link]):Option[Document]

  //add a link to a link sequence
  def +#@#@@(i:Int, link:Link):Option[Document]

  //add a section
  def +/(name:String):Option[Document] = renewChild(Some(Section(name)))

  //remove a section
  def -/(name:String):Option[Document] 

  //remove a link
  def -#@(i:Int):Option[Document]

  //remove a linkSeq
  def -#@@(i:Int):Option[Document]

  //remove link l from linkseq ls
  def -#@#@@(l:Int, ls:Int):Option[Document]

  //move link at position iOld to position iNew
  def ~#@(iOld:Int, iNew:Int):Option[Document]

  //move link sequence at position iOld to position iNew
  def ~#@@(iOld:Int, iNew:Int):Option[Document]

  //move link at position iOld in linksequence ls to position iNew
  def ~#@#@@(iOld:Int, iNew:Int, ls:Int):Option[Document]
  
  def prependPath(below:List[String]):List[String]

  val path:List[String] = prependPath(List[String]())
  val pathStr = path reduceLeft (_ +"/"+ _)
}

class SectionManipulator(val sec:Section, parents:List[Manipulator]) extends Manipulator(sec, parents) with SequenceStuff {
  def renewChild(newChild:Option[Section]):Option[Document] = {
    val newCurrent = newChild map {
      nc => newSection(sections = sec.sections + (nc.name -> nc)) 
    }
    parents.head.renewChild(newCurrent)
  }

  def newSection(name:String = sec.name, links:Seq[Link] = sec.links, linkSeqs:Seq[LinkSeq] = sec.linkSeqs, sections:SecMap = sec.sections):Section = new Section(
    name, links, linkSeqs, sections)

  def updateSelf(name:String = sec.name, links:Seq[Link] = sec.links, linkSeqs:Seq[LinkSeq] = sec.linkSeqs, sections:SecMap = sec.sections):Option[Document] = {
    val newCurrent = Some(new Section(name, links, linkSeqs, sections))
    parents.head.renewChild(newCurrent)
  }

  def getDoc = parents.head.getDoc
  def links = sec.links
  def linkSeqs = sec.linkSeqs
  
  def +#@(link:Link):Option[Document] = updateSelf(links = (sec.links :+ link)) //keep an eye on this bit

  def +#@@(links:Seq[Link]):Option[Document] = updateSelf(linkSeqs = (sec.linkSeqs :+ LinkSeq(links)))

  def +#@#@@(i:Int, link:Link):Option[Document] = {
    val newCurrent = #@@(i) map {
      lseq => newSection(linkSeqs = sec.linkSeqs.updated(i, LinkSeq(lseq.links :+ link)))
    }
    parents.head.renewChild(newCurrent)
  }

  //remove a section
  def -/(name:String):Option[Document] = updateSelf(sections = (sec.sections - name))
  //remove a link
  def -#@(i:Int):Option[Document] = updateSelf(links = removeFromSeq(i, sec.links))

  //remove a linkSeq
  def -#@@(i:Int):Option[Document] = updateSelf(linkSeqs = removeFromSeq(i, sec.linkSeqs))

  //remove link l from linkseq ls
  def -#@#@@(l:Int, ls:Int):Option[Document] = updateSelf(linkSeqs = 
    updateSequenceItem(ls, sec.linkSeqs){
      linkSeq => new LinkSeq(removeFromSeq(l, linkSeq.links))
    })

  //move link at position iOld to position iNew
  def ~#@(iOld:Int, iNew:Int):Option[Document] = updateSelf(links = moveInSeq(iOld, iNew, sec.links))

  //move link sequence at position iOld to position iNew
  def ~#@@(iOld:Int, iNew:Int):Option[Document] = updateSelf(linkSeqs = 
    moveInSeq(iOld, iNew, sec.linkSeqs))

  //move link at position iOld in linksequence ls to position iNew
  def ~#@#@@(iOld:Int, iNew:Int, ls:Int):Option[Document] = updateSelf(linkSeqs =
    updateSequenceItem(ls, sec.linkSeqs){
      linkSeq => new LinkSeq(moveInSeq(iOld, iNew, linkSeq.links))
    })

  def prependPath(below:List[String]) = parents.head.prependPath(sec.name::below)
}

abstract class NonStoringManipulator(current:ContainsSections, parents:List[Manipulator])
  extends Manipulator(current, parents) {
  def links:Seq[Link] = Seq[Link]()
  def linkSeqs:Seq[LinkSeq] = Seq[LinkSeq]()
  def +#@(link:Link):Option[Document] = None
  def +#@@(links:Seq[Link]):Option[Document] = None
  def +#@#@@(i:Int, link:Link):Option[Document] = None
  def -#@(i:Int):Option[Document] = None
  def -#@@(i:Int):Option[Document] = None
  def -#@#@@(l:Int, ls:Int):Option[Document] = None
  def ~#@(iOld:Int, iNew:Int):Option[Document] = None
  def ~#@@(iOld:Int, iNew:Int):Option[Document] = None
  def ~#@#@@(iOld:Int, iNew:Int, ls:Int):Option[Document] = None
}

class BaseManipulator(doc:Document) extends NonStoringManipulator(doc, List[Manipulator]()) {
  //base 
  def renewChild(newChild:Option[Section]):Option[Document] = newChild map { 
    nc => new Document(doc.sections + (nc.name -> nc))
  }
  def getDoc = doc
  def prependPath(below:List[String]) = "Document"::below
  def -/(name:String):Option[Document] = Some(new Document(doc.sections - name))
}


class NoneSection extends ContainsSections {
  val sections = Map[String,Section]()
  def renderXML = <fail />
}

class NoneManipulator(secName:String, parents:List[Manipulator])
  extends NonStoringManipulator(new NoneSection(), parents) {

  def getDoc = parents.head.getDoc
  override def /(sn2:String) = new NoneManipulator(sn2, this::parents)
  def -/(name:String):Option[Document] = None
  def renewChild(newChild:Option[Section]):Option[Document] = None
  def prependPath(below:List[String]) = parents.head.prependPath(("!" +secName)::below)
}
