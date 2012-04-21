package local.nodens;
package object linkmodel {
  type SecMap=Map[String,Section]
  
  def collectSecs(sections:Seq[Section]) = sections.foldLeft(Map[String,Section]()) {
    case (map, sec) => map + (sec.name -> sec)
  }

  def optMatch[A](test: => Boolean, theVal: => A):Option[A] = if (test) Some(theVal) else None

  implicit def string2LinkSymAssoc(title: String) = new LinkSymAssoc(title)


  trait ContainsSections extends Comp{
    def sections:SecMap
  }

  implicit def document2Manipulator(doc:Document) = new BaseManipulator(doc)
  implicit def document2BaseManipulate(doc:Document) = new BaseManipulate(Some(document2Manipulator(doc)))
  implicit def optDocument2Manipulate(oDoc:Option[Document]) = Manipulate(oDoc map (document2Manipulator(_)))
  implicit def manipulator2BaseManipulate(manor:Manipulator) = Manipulate(Some(manor))
  implicit def optManipulator2Manipulate(oManor:Option[Manipulator]) = Manipulate(oManor)
}
