import scala.util.matching.Regex

class MacAddress(macAddress: String) {

  lazy val MacStringRE:Regex = "^(([0-9A-Fa-f]){12})$".r
  lazy val MacFormatRE:Regex = "(([0-9A-Fa-f]{2}[\\.:-]){5}([0-9A-Fa-f]{2}))".r
  lazy val MacFormatZeroRE:Regex = "(([0-9A-Fa-f]{1,2}[\\.:-]){5}([0-9A-Fa-f]{1,2}))".r
  lazy val MacCiscoRE:Regex = "([0-9a-fA-F]{4}[\\.:-][0-9a-fA-F]{4}[\\.:-][0-9a-fA-F]{4})$".r

  def isValid: Boolean = {
    macAddress match {
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
  def asString: Option[String] = {
    macAddress match {
      case MacFormatRE(a, _*) => Some(a.replaceAll(":","").replaceAll("\\.","").replaceAll("-",""))
      case MacCiscoRE(a, _*) => Some(a.replaceAll(":","").replaceAll("\\.","").replaceAll("-",""))
      case MacFormatZeroRE(a, _*) =>
        val sb = new StringBuilder
        a.split("[\\.:-]").foreach( b => {
          if (b.length == 1) sb.append(s"0$b") else sb.append(b)
        })
        Some(sb.mkString)
      case MacStringRE(a, _*) => Some(a)
      case _ => None
    }
  }

  /**
    * Return Mac address as a colon delimited string, i.e., 01:02:03:04:05:06
    * @return
    */
  def asFormat(delimiter: Char = ':'): Option[String] = {
      macAddress match {
        // scalastyle:off magic.number
        case MacStringRE(a, _*) => Some(a.replaceAll("(.{2})", "$1" + delimiter).substring(0, 17))
        // scalastyle:on magic.number
        case MacFormatRE(a, _*) =>
          Some(a.replaceAll("\\.",delimiter.toString).replaceAll("-",delimiter.toString).replaceAll(":", delimiter.toString))
        case MacCiscoRE(a, _*) => new MacAddress(asString.getOrElse(a)).asFormat(delimiter)
        case MacFormatZeroRE(a, _*) => new MacAddress(asString.getOrElse(a)).asFormat(delimiter)
        case _ => None
      }
    }

  def asCiscoFormat(delimiter: Char = '.'): Option[String] = {
    macAddress match {
      // scalastyle:off magic.number
      case MacStringRE(a, _*) => Some(a.replaceAll("(.{4})", "$1" + delimiter).substring(0, 14))
      // scalastyle:on magic.number
      case MacFormatRE(a, _*) => new MacAddress(asString.getOrElse(a)).asCiscoFormat(delimiter)
      case MacCiscoRE(a, _*) =>
        Some(a.replaceAll("\\.",delimiter.toString).replaceAll("-",delimiter.toString).replaceAll(":", delimiter.toString))
      case MacFormatZeroRE(a, _*) => new MacAddress(asString.getOrElse(a)).asCiscoFormat(delimiter)
      case _ => None
    }
  }

  def asBytes: Array[Byte] = {
    hex2bytes(asString.getOrElse(""))
  }

  def asLong: Long = {
    val b = asFormat().getOrElse("N/A").split(":")
    b.foreach(println)
    1L
  }

  def hex2bytes(hex: String): Array[Byte] = {
    hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
      case None => bytes.map("%02x".format(_)).mkString
      case _ => bytes.map("%02x".format(_)).mkString(sep.get)
    }
  }

}

