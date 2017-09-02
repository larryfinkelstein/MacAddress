import scala.util.matching.Regex
import scalaz.{Failure, Success, Validation}

trait MacType {
  lazy val MacStringRE: Regex = "^(([0-9A-Fa-f]){12})$".r
  lazy val MacFormatRE: Regex = "(([0-9A-Fa-f]{2}[\\.:-]){5}([0-9A-Fa-f]{2}))".r
  lazy val MacFormatZeroRE: Regex = "(([0-9A-Fa-f]{1,2}[\\.:-]){5}([0-9A-Fa-f]{1,2}))".r
  lazy val MacCiscoRE: Regex = "([0-9a-fA-F]{4}[\\.:-][0-9a-fA-F]{4}[\\.:-][0-9a-fA-F]{4})$".r

  protected def isValid(mac: String): Boolean = {
    mac match {
      case MacFormatRE(a, _*) => true
      case MacStringRE(a, _*) => true
      case MacFormatZeroRE(a, _*) => true
      case MacCiscoRE(a, _*) => true
      case _ => false
    }
  }

  /**
    * Return Mac as string, without any formatting, i.e., 010203040506
    * @return
    */
  protected def toString(mac: String): String = {
    mac match {
      case MacFormatRE(a, _*) => a.replaceAll(":","").replaceAll("\\.","").replaceAll("-","")
      case MacCiscoRE(a, _*) => a.replaceAll(":","").replaceAll("\\.","").replaceAll("-","")
      case MacFormatZeroRE(a, _*) =>
        val sb = new StringBuilder
        a.split("[\\.:-]").foreach( b => {
          if (b.length == 1) sb.append(s"0$b") else sb.append(b)
        })
        sb.mkString
      case MacStringRE(a, _*) => a
      case _ => ""
    }
  }

  /**
    * Return Mac address as a colon delimited string, i.e., 01:02:03:04:05:06
    * @return
    */
  protected def toFormat(mac: String, delimiter: Char = ':'): String = {
    mac match {
      // scalastyle:off magic.number
      case MacStringRE(a, _*) => a.replaceAll("(.{2})", "$1" + delimiter).substring(0, 17)
      // scalastyle:on magic.number
      case MacFormatRE(a, _*) =>
        a.replaceAll("\\.",delimiter.toString).replaceAll("-",delimiter.toString).replaceAll(":", delimiter.toString)
      case MacCiscoRE(a, _*) => toFormat(toString(mac), delimiter)
      case MacFormatZeroRE(a, _*) => toFormat(toString(mac),delimiter)
      case _ => ""
    }
  }

  protected def toCiscoFormat(mac: String, delimiter: Char = '.'): String = {
    mac match {
      // scalastyle:off magic.number
      case MacStringRE(a, _*) => a.replaceAll("(.{4})", "$1" + delimiter).substring(0, 14)
      // scalastyle:on magic.number
      case MacFormatRE(a, _*) => toCiscoFormat(toString(mac), delimiter)
      case MacCiscoRE(a, _*) =>
        a.replaceAll("\\.",delimiter.toString).replaceAll("-",delimiter.toString).replaceAll(":", delimiter.toString)
      case MacFormatZeroRE(a, _*) => toCiscoFormat(toString(mac), delimiter)
      case _ => ""
    }
  }

  protected def toBytes(mac: String): Array[Byte] = {
    toString(mac).replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  protected def toLong(mac: String): Long = {
    val hb = toBytes(toFormat(mac))
    var longMac = 0L
    for (i <- hb.indices) {
      val t: Long = (hb(i) & 0xffL) << ((5 - i) * 8)
      //println(s"$i: ${hb(i)} - ${t}")
      longMac |= t
    }
    longMac
  }
}

class MacAddress private (mac: String) extends MacType {
  val macAddress: String = mac

  def isValid: Boolean = super.isValid(macAddress)
  override def toString: String = super.toString(macAddress)
  def toBytes: Array[Byte] = super.toBytes(macAddress)
  def toCiscoFormat: String = super.toCiscoFormat(macAddress)
  def toCiscoFormat(delimiter: Char): String = super.toCiscoFormat(macAddress, delimiter)
  def toFormat: String = super.toFormat(macAddress)
  def toFormat(delimiter: Char): String = super.toFormat(macAddress, delimiter)
  def toLong: Long = super.toLong(macAddress)
}

object MacAddress extends MacType {
  def apply(mac: String): Validation[String, MacAddress] = {
    if (!super.isValid(mac)) Failure(s"Unable to recognize MAC format for $mac") else Success(new MacAddress(mac))
  }
}

