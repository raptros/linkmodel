package local.nodens.linkmodel

/**
 * The Manipulator for Sections.
 * @param sec The Section object this Manipulator applies to.
 * @param parents Parents object
 * @note See Manipulator
 * @note See SequenceStuff
 */
class SectionManipulator(val sec:Section, parents:List[Manipulator]) extends Manipulator(sec, parents) with SequenceStuff {

  def renewChild(newChild:Option[Section]):Option[Document] = {
    val newCurrent = newChild map {
      nc => newSection(sections = sec.sections + (nc.name -> nc)) 
    }
    parents.head.renewChild(newCurrent)
  }

  /**
   * newSection employes default arguments to produce modified copies of sec.
   * @see Constructor for Section.
   */
  def newSection(name:String = sec.name, links:Seq[Link] = sec.links, linkSeqs:Seq[LinkSeq] = sec.linkSeqs, sections:SecMap = sec.sections):Section = new Section(
    name, links, linkSeqs, sections)
  
  /**
   * updateSelf renews the heirarchy for modifications based on the current section;
   * uses default arguments.
   * @see Section constructor
   * @see newSection
   * @see renewChild
   */
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
  def -/(name:String):Option[Document] = updateSelf(sections = (sec.sections - name))
  def -#@(i:Int):Option[Document] = updateSelf(links = removeFromSeq(i, sec.links))
  def -#@@(i:Int):Option[Document] = updateSelf(linkSeqs = removeFromSeq(i, sec.linkSeqs))
  def -#@#@@(l:Int, ls:Int):Option[Document] = updateSelf(linkSeqs = 
    updateSequenceItem(ls, sec.linkSeqs){
      linkSeq => new LinkSeq(removeFromSeq(l, linkSeq.links))
    })
  def ~#@(iOld:Int, iNew:Int):Option[Document] = updateSelf(links = moveInSeq(iOld, iNew, sec.links))
  def ~#@@(iOld:Int, iNew:Int):Option[Document] = updateSelf(linkSeqs = 
    moveInSeq(iOld, iNew, sec.linkSeqs))
  def ~#@#@@(iOld:Int, iNew:Int, ls:Int):Option[Document] = updateSelf(linkSeqs =
    updateSequenceItem(ls, sec.linkSeqs){
      linkSeq => new LinkSeq(moveInSeq(iOld, iNew, linkSeq.links))
    })

  def prependPath(below:List[String]) = parents.head.prependPath(sec.name::below)
}

/**
 * Represents Manipulators for components that don't contain things like links etc.
 * Effectively, it's what's common between BaseManipulator and NoneManipulator.
 * @note Implements most operations by returning None to denote failure.
 */
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

/**
 * BaseManipulator is the Manipulator for Documents.
 * @param doc The base of the heirarchy.
 * @note Provides termination implementation for various section-recursive methods, including renewChild.
 * @note Implicit conversion from Document has been defined.
 * @see NonStoringManipulator for implementation of operators.
 */
class BaseManipulator(doc:Document) extends NonStoringManipulator(doc, List[Manipulator]()) {
  def renewChild(newChild:Option[Section]):Option[Document] = newChild map { 
    nc => new Document(doc.sections + (nc.name -> nc))
  }
  def getDoc = doc
  def prependPath(below:List[String]) = "Document"::below
  def -/(name:String):Option[Document] = Some(new Document(doc.sections - name))
}

/**
 * Represents a non-existant section.
 */
class NoneSection extends ContainsSections {
  val sections = Map[String,Section]()
  def renderXML = <fail />
}

/**
 * A manipulator for invalid paths. 
 */
class NoneManipulator(secName:String, parents:List[Manipulator])
  extends NonStoringManipulator(new NoneSection(), parents) {
  def getDoc = parents.head.getDoc
  override def /(sn2:String) = new NoneManipulator(sn2, this::parents)
  def -/(name:String):Option[Document] = None
  def renewChild(newChild:Option[Section]):Option[Document] = None
  def prependPath(below:List[String]) = parents.head.prependPath(("!" +secName)::below)
}
