package org.flowpaint.property

import _root_.scala.collection.mutable.Map
import _root_.scala.xml.{Elem, Node}
import util.{DataSample, Tome, PropertyRegister, Library}
/**
 *  Contains deserialization.
 */
object Data {
    def fromXML(node: Node): Data = {

        val data = new DataImpl()

        val nums = node \ "num"
        val texts = node \ "text"
        val refs = node \ "ref"

        nums foreach ((n: Node) => {

            val id = (n \ "@id").text

            try {
              val value = n.text.toFloat
  
              data.setFloatProperty(id, value)
            }
            catch {
              case e : Exception => // Catch number format exceptions and skip those properties
            }
        })

        texts foreach ((n: Node) => {

            val id = (n \ "@id").text
            val value = n.text

            data.setStringProperty(id, value)
        })

        refs foreach ((n: Node) => {

            val id = (n \ "@id").text
            val value = n.text

            data.setReference(id, value)
        })

        data
    }

}


/**
 *   Holds some simple key-value data.  Provides listener support.
 *
 * @author Hans Haggstrom
 */
trait Data {

    /**
     *   A listener that takes the changed Data object and the name of the changed parameter as parameters.
     *   If the parameter is null, the whole data may have changed.
     */
    type DataListener = (Data, String) => Unit

    def addListener(listener: DataListener)

    def removeListener(listener: DataListener)


    def getFloatProperty(name: String, default: Float): Float

    def setFloatProperty(name: String, value: Float)

    def getFloatProperty(id: Int, default: Float): Float

    def setFloatProperty(id: Int, value: Float)

    def getStringProperty(name: String, default: String): String

    def setStringProperty(name: String, value: String)

    def getReference[T  <: Tome ]( name : String, default  : T, library : Library ) : T

    def setReference( name : String, reference : String )


    /**
     *   Copies the float properties of this Data to the specified sample.
     */
    def getFloatProperties(target: DataSample)

    def getFloatProperties() : DataSample


    /**
     * The names of the float properties available.
     */
    def getFloatPropertyNames(): List[String]

    /**
     * The names of the string properties available.
     */
    def getStringPropertyNames(): List[String]

    /**
     * Names of all avaialble properties
     */
    def getPropertyNames : List[String]

    /**
     *   Copies the float properties of the specified sample to this Data.
     */
    def setFloatProperties(values: DataSample)

    def containsFloatProperty( name : String) : Boolean

    def containsStringProperty( name : String ) : Boolean

    def containsProperty( name : String ) : Boolean


    /**
     *  Removes all properties.
     */
    def clear()


    def removeFloatProperty(name: String)

    def removeStringProperty(name: String)


    def getStringProperties(target: Map[String, String])

    /**
     *  Sets this Data to the values of the specified source data.  Any previous values are removed first.
     */
    def set(sourceData: Data)


    /**
     *  Sets this Data to the values of the specified source data.  Any previous values are not removed.
     */
    def setValuesFrom(sourceData: Data)

    /**
     * Interpolate this Data towards the specified target value, by the specified amount, 0 = no change, 1 = become target.
     * If a value is in only one of the samples, that value is used directly.
     */
    def interpolate( amount : Float, target: Data )

  /**
   * Clears this sample, sets it to start, and interpolates it towards the target by the specified amount, 0 = start value, 1 = target value.
   * If a value is in only one of the samples, that value is used directly.
   */
  def interpolate( amount : Float, start: Data , target: Data )

  def toXML() : List[Elem]
}
