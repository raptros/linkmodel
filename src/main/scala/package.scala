package local.nodens;
/**
 * package object for the linkmodel system.
 * defines some types, some simple functions, and some implicit conversions.
 */
package object linkmodel {
  type SecMap=Map[String,Section]
  
  /**
   * collects sections into a map.
   */
  def collectSecs(sections:Seq[Section]) = sections.foldLeft(Map[String,Section]()) {
    case (map, sec) => map + (sec.name -> sec)
  }

  /**
   * Creates an Option based on the test.
   * @tparam A The type to be put into the option.
   * @param test A boolean computation deciding the Option.
   * @param theVal A computation returning something to be placed into the Option
   * @return Either Some around the eval of theVal, or None, based on test
   * @note Makes things much neater
   */
  def optMatch[A](test: => Boolean, theVal: => A):Option[A] = if (test) Some(theVal) else None

  import scala.xml.Elem
  /**
   * Parent trait for all components of the model.
   */
  trait Comp {
    /**
     * Render this node as XML.
     */
    def renderXML:Elem
  }

  /**
   * Trait for things that contains sections.
   */
  trait ContainsSections extends Comp {
    /**
     * Must be implemented.
     */
    def sections:SecMap

    /**
     * Tests whether or not a named section is present.
     */
    def contains(name:String):Boolean = sections contains name
  }

  /**
   * Enables the #@# syntax.
   * @see LinkSymAssoc
   */
  implicit def string2LinkSymAssoc(title: String) = new LinkSymAssoc(title)

  /**
   * A class that allows generation of Links using #@# joining strings.
   */
  final class LinkSymAssoc(val title:String) {
    @inline def #@#(url:String):Link = Link(title, url)
  }


  implicit def document2Manipulator(doc:Document) = new BaseManipulator(doc)
  implicit def document2BaseManipulate(doc:Document) = new BaseManipulate(Some(document2Manipulator(doc)))
  implicit def optDocument2Manipulate(oDoc:Option[Document]) = Manipulate(oDoc map (document2Manipulator(_)))
  implicit def manipulator2BaseManipulate(manor:Manipulator) = Manipulate(Some(manor))
  implicit def optManipulator2Manipulate(oManor:Option[Manipulator]) = Manipulate(oManor)
}
