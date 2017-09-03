import scala.util.matching.Regex
import scalaz.{Failure, Success, Validation}

trait MacType {

  /**
    * Return Mac as string, without any formatting, i.e., 010203040506
    * @return
    */
  protected def toHexString(mac: String): String = mac

  /**
    * Return Mac address as a colon delimited string, i.e., 01:02:03:04:05:06
    * @return
    */
  protected def toHexidecimal(mac: String, delimiter: Char = ':'): String = {
    mac.replaceAll("(.{2})", "$1" + delimiter).substring(0, 17)
  }

  protected def toDotNotation(mac: String, delimiter: Char = '.'): String = {
    mac.replaceAll("(.{4})", "$1" + delimiter).substring(0, 14)
  }

  protected def toBytes(mac: String): Array[Byte] = {
    toHexString(mac).replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  protected def toLong(mac: String): Long = {
    val hb = toBytes(toHexidecimal(mac))
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
  // Saved in hexidecimal string format
  val macAddress: String = mac

  override def toString: String = super.toHexString(macAddress)
  def toBytes: Array[Byte] = super.toBytes(macAddress)
  def toDotNotation: String = super.toDotNotation(macAddress)
  def toDotNotation(delimiter: Char): String = super.toDotNotation(macAddress, delimiter)
  def toHexidecimal: String = super.toHexidecimal(macAddress)
  def toHexidecimal(delimiter: Char): String = super.toHexidecimal(macAddress, delimiter)
  def toBitReversed: String = toHexidecimal(macAddress, '-')
  def toByteString: String = s"[${toBytes.map("%02x" format _).mkString(",")}]"
  def toHexString: String = toString
  def toLong: Long = super.toLong(macAddress)
}

object MacAddress extends MacType {
  private lazy val MacStringRE: Regex = "^(([0-9A-Fa-f]){12})$".r
  private lazy val MacFormatRE: Regex = "(([0-9A-Fa-f]{2}[\\.:-]){5}([0-9A-Fa-f]{2}))".r
  private lazy val MacFormatZeroRE: Regex = "(([0-9A-Fa-f]{1,2}[\\.:-]){5}([0-9A-Fa-f]{1,2}))".r
  private lazy val MacCiscoRE: Regex = "([0-9a-fA-F]{4}[\\.:-][0-9a-fA-F]{4}[\\.:-][0-9a-fA-F]{4})$".r

  def apply(mac: String): Validation[String, MacAddress] = {
//    if (!super.isValid(mac)) Failure(s"Unable to recognize MAC format for $mac") else Success(new MacAddress(super.asHexString(mac))))
    /**
      * Return Mac as hexidecimal string, without any formatting, i.e., 010203040506
      * @return
      */
      mac match {
        case MacFormatRE(a, _*) => Success(new MacAddress(a.replaceAll(":","").replaceAll("\\.","").replaceAll("-","").toLowerCase))
        case MacCiscoRE(a, _*) => Success(new MacAddress(a.replaceAll(":","").replaceAll("\\.","").replaceAll("-","").toLowerCase))
        case MacFormatZeroRE(a, _*) =>
          val sb = new StringBuilder
          a.split("[\\.:-]").foreach( b => {
            if (b.length == 1) sb.append(s"0$b") else sb.append(b)
          })
          Success(new MacAddress(sb.mkString.toLowerCase))
        case MacStringRE(a, _*) => Success(new MacAddress(a.toLowerCase))
        case _ => Failure(s"Unable to recognize MAC format for $mac")
      }
  }

  def getRandomMACAddress: String = {
    val random = new util.Random
    Array.fill[String](6)(f"${random.nextInt(255)}%02x").mkString(":")
  }
}

