package local.nodens.linkmodel


case class Manipulate(manip:Option[Manipulator]) {
  def /(secName:String):Option[Manipulator] = manip.map(_ / secName)

  def #@(i:Int):Option[Link] = manip.flatMap(_ #@ i)
  def #@@(i:Int):Option[LinkSeq] = manip.flatMap(_ #@@ i)
  def links:Seq[Link] = manip.map(_.links).toList.flatten
  def linkSeqs:Seq[LinkSeq] = manip.map(_.linkSeqs).toList.flatten
  def renderXML = manip.map(_.renderXML)
  def path = manip.map(_.path)
  def +/(name:String):Option[Document] = manip.flatMap(_ +/ name)
  def +#@(link:Link):Option[Document] = manip.flatMap(_ +#@ link)
  def +#@@(links:Seq[Link]):Option[Document] = manip.flatMap(_ +#@@ links)
  def +#@#@@(i:Int, link:Link):Option[Document] = manip.flatMap(_ +#@#@@(i, link))
}

abstract class Manipulator(val current:ContainsSections, val parents:List[Manipulator]) {
  //access subsections
  def /(secName:String):Manipulator = current.sections.get(secName).map {
    c => new SectionManipulator(c, this::parents)
  }.getOrElse(new NoneManipulator(secName, this::parents))

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
  

  def path:String
}

class SectionManipulator(val sec:Section, parents:List[Manipulator]) extends Manipulator(sec, parents) {
  def renewChild(newChild:Option[Section]):Option[Document] = {
    val newCurrent = newChild map { nc => new Section(
      sec.name, sec.links, sec.linkSeqs,
      sec.sections + (nc.name -> nc))
    }
    parents.head.renewChild(newCurrent)
  }

  def getDoc = parents.head.getDoc
  def links = sec.links
  
  def linkSeqs = sec.linkSeqs
  
  def +#@(link:Link):Option[Document] = {
    val newCurrent = new Section(sec.name,
      (sec.links :+ link), //keep an eye on this bit
      sec.linkSeqs, sec.sections)
    parents.head.renewChild(Some(newCurrent))
  }

  def +#@@(links:Seq[Link]):Option[Document] = {
    val newCurrent = new Section(sec.name, sec.links,
      sec.linkSeqs :+ LinkSeq(links), //eyes!
      sec.sections)
    parents.head.renewChild(Some(newCurrent))
  }

  def +#@#@@(i:Int, link:Link):Option[Document] = {
    val newCurrent = #@@(i) map {
      lseq => new Section(sec.name, sec.links,
        sec.linkSeqs.updated(i, LinkSeq(lseq.links :+ link)),
        sec.sections)
    }
    parents.head.renewChild(newCurrent)
  }

  val path = parents.head.path + "/" + sec.name
}

abstract class NonStoringManipulator(current:ContainsSections, parents:List[Manipulator])
  extends Manipulator(current, parents) {
  def links:Seq[Link] = Seq[Link]()
  def linkSeqs:Seq[LinkSeq] = Seq[LinkSeq]()
  def +#@(link:Link):Option[Document] = None
  def +#@@(links:Seq[Link]):Option[Document] = None
  def +#@#@@(i:Int, link:Link):Option[Document] = None
}

class BaseManipulator(doc:Document) extends NonStoringManipulator(doc, List[Manipulator]()) {
  //base 
  def renewChild(newChild:Option[Section]):Option[Document] = newChild map { 
    nc => new Document(doc.sections + (nc.name -> nc))
  }
  def getDoc = doc
  val path="Document"
}


class NoneSection extends ContainsSections {
  val sections = Map[String,Section]()
  def renderXML = <fail />
}

class NoneManipulator(secName:String, parents:List[Manipulator])
  extends NonStoringManipulator(new NoneSection(), parents) {

  def getDoc = parents.head.getDoc
  override def /(sn2:String) = new NoneManipulator(sn2, this::parents)
  def renewChild(newChild:Option[Section]):Option[Document] = None
  val path = parents.head.path + "!/" + secName
}
