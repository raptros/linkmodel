package local.nodens.linkmodel
import scala.xml._
import scala.collection.immutable.Map


trait Comp {
  def renderXML:Elem
  //override def toString = renderXML.toString
}

final class LinkSymAssoc(val title:String) {
  @inline def #@#(url:String):Link = Link(title, url)
}

/**
 * Link represents a url with a title, in XML. It can be used as a case class.
 * fields: title, url
 */
class Link(val title:String, val url:String) extends Comp{
  def renderXML:Elem = <link title={title} url={url} />
  override def equals(a:Any) = a match {
    case other:Link => title == other.title && url == other.url
    case _ => false
  }
  override def toString = "Link(" + title + ", " + url +")"
}

object Link {
  def apply(title:String, url:String):Link = new Link(title, url)
  def unapply(link:Link) = Some(Pair(link.title, link.url))
  def parse(node:Node):Option[Link] = 
  optMatch(node.label == "link",
    Link((node \ "@title").text, (node \ "@url").text))
}

/**
 * LinkSeq is a sequence of links, in order.
 */
class LinkSeq(val links:Seq[Link]) extends Comp {
  override def equals(a:Any) = a match{
    case other:LinkSeq => links == other.links
    case _ => false
  }
  def renderXML:Elem = 
  <sequence>
    {links map (_.renderXML)}
  </sequence>
  override def toString = "LinkSeq(" + 
  links.toString + ")"
}

object LinkSeq {
  def apply(links:Seq[Link]) = new LinkSeq(links)
  def unapply(ls:LinkSeq) = Some(ls.links)
  def parse(node:Node):Option[LinkSeq] = optMatch(node.label == "sequence",
    LinkSeq((node.child map Link.parse) flatten))
}

/**
 * Section has a name and any a list of elements.
 */
class Section(
  val name:String,
  val links:Seq[Link], 
  val linkSeqs:Seq[LinkSeq], 
  val sections:SecMap) extends Comp with ContainsSections {

  def this(name:String, elemMap:Map[Int,Seq[Comp]]) = this(
    name,
    elemMap.getOrElse(0, Seq[Link]()).asInstanceOf[Seq[Link]],
    elemMap.getOrElse(1, Seq[LinkSeq]()).asInstanceOf[Seq[LinkSeq]],
    collectSecs(elemMap.getOrElse(2, Seq[Section]()).asInstanceOf[Seq[Section]])
  )

  def this(name:String, elements:Seq[Comp]) = this(name, elements groupBy Section.partFunc)
  
  override def equals(a:Any) = a match {
    case other:Section => name == other.name && links == other.links &&
    linkSeqs == other.linkSeqs && sections == other.sections
    case _ => false
  }

  def renderXML:Elem =
  <section name={name}>
    {links map (_.renderXML)}
    {linkSeqs map (_.renderXML)}
    {sections.values.map (_.renderXML)}
  </section>

  override def toString = "Section(" + name + ", " + links.toString +
  ", " + linkSeqs.toString + ", " + sections.toString + ")"
}

object Section {
  def apply(name:String, element:Comp*) = new Section(name, element.toSeq)
  def unapplySeq(sec:Section) = Some(Pair(sec.name, sec.links ++ sec.linkSeqs ++ sec.sections))
  
  def parseStuff(node:Node):Option[Comp] = List(
    Link.parse _, LinkSeq.parse _, Section.parse _
  ).foldLeft(None:Option[Comp])(_ orElse _(node))

  def parse(node:Node):Option[Section] = optMatch(
    node.label == "section",
    new Section((node \ "@name").text,
      node.child map(parseStuff) flatten))

  private def partFunc(elem:Comp):Int = elem match {
    case Link(_,_) => 0
    case LinkSeq(_) => 1
    case Section(_, _*) => 2
  }
}
/**
 * Document is top level of storage.
 */
class Document(val sections:SecMap) extends Comp with ContainsSections {

  def this(sectionSeq:Seq[Section]) = this(collectSecs(sectionSeq))

  override def equals(a:Any) = a match {
    case other:Document => sections == other.sections
    case _ => false
  }

  def renderXML:Elem = 
  <document>
    {sections.values map (_.renderXML)}
  </document>

  override def toString = "Document(" + sections.toString() + ")"
}

object Document {
  def apply(section:Section*) = new Document(section.toSeq)
  def unapplySeq(doc:Document) = Some(doc.sections.values.toSeq)
  def parse(node:Node):Option[Document] = optMatch(
    node.label == "document",
    new Document(node.child map(Section.parse) flatten))
}

/**
 * Model is a convenience operations object.
 */
object Model {
  def parse(doc:Node):Option[Document] = Document.parse(doc)
  def renderXML(doc:Document):Elem = doc.renderXML
  def load(path:String):Option[Document] = parse(XML.loadFile(path))
  def save(path:String, doc:Document) = XML.save(path, Model.renderXML(doc))
}
