import org.scalatest.{FunSuite, MustMatchers}

class MacAddressTest extends FunSuite with MustMatchers {
  test("Valid Mac") {
    val mac = new MacAddress("01:02:03:04:05:06")
    mac.isValid mustBe true
    mac.asString.getOrElse("N/A") mustEqual "010203040506"
    mac.asFormat().getOrElse("N/A") mustEqual "01:02:03:04:05:06"
    mac.asFormat('-').getOrElse("N/A") mustEqual "01-02-03-04-05-06"
    mac.asFormat('.').getOrElse("N/A") mustEqual "01.02.03.04.05.06"
    mac.asCiscoFormat().getOrElse("N/A") mustEqual "0102.0304.0506"
    mac.asCiscoFormat(':').getOrElse("N/A") mustEqual "0102:0304:0506"
    mac.asBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
//    mac.asLong mustEqual 0
  }

  test("Valid Mac String") {
    val mac = new MacAddress("010203040506")
    mac.isValid mustBe true
    mac.asString.getOrElse("N/A") mustEqual "010203040506"
    mac.asFormat().getOrElse("N/A") mustEqual "01:02:03:04:05:06"
    mac.asFormat('-').getOrElse("N/A") mustEqual "01-02-03-04-05-06"
    mac.asFormat('.').getOrElse("N/A") mustEqual "01.02.03.04.05.06"
    mac.asCiscoFormat().getOrElse("N/A") mustEqual "0102.0304.0506"
    mac.asCiscoFormat(':').getOrElse("N/A") mustEqual "0102:0304:0506"
    mac.asBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
  }

  test("Valid Mac with dash") {
    val mac = new MacAddress("01-02-03-04-05-06")
    mac.isValid mustBe true
    mac.asString.getOrElse("N/A") mustEqual "010203040506"
    mac.asFormat().getOrElse("N/A") mustEqual "01:02:03:04:05:06"
    mac.asFormat('-').getOrElse("N/A") mustEqual "01-02-03-04-05-06"
    mac.asFormat('.').getOrElse("N/A") mustEqual "01.02.03.04.05.06"
    mac.asCiscoFormat().getOrElse("N/A") mustEqual "0102.0304.0506"
    mac.asCiscoFormat(':').getOrElse("N/A") mustEqual "0102:0304:0506"
    mac.asBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
  }

  test("Valid Cisco Mac") {
    val mac = new MacAddress("0102-0304-0506")
    mac.isValid mustBe true
    mac.asString.getOrElse("N/A") mustEqual "010203040506"
    mac.asFormat().getOrElse("N/A") mustEqual "01:02:03:04:05:06"
    mac.asFormat('-').getOrElse("N/A") mustEqual "01-02-03-04-05-06"
    mac.asFormat('.').getOrElse("N/A") mustEqual "01.02.03.04.05.06"
    mac.asCiscoFormat().getOrElse("N/A") mustEqual "0102.0304.0506"
    mac.asCiscoFormat(':').getOrElse("N/A") mustEqual "0102:0304:0506"
    mac.asBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)

    val mac1 = new MacAddress("0102.0304.0506")
    mac1.isValid mustBe true
    mac1.asString.getOrElse("N/A") mustEqual "010203040506"
    mac1.asFormat().getOrElse("N/A") mustEqual "01:02:03:04:05:06"
    mac1.asFormat('-').getOrElse("N/A") mustEqual "01-02-03-04-05-06"
    mac1.asFormat('.').getOrElse("N/A") mustEqual "01.02.03.04.05.06"
    mac1.asCiscoFormat().getOrElse("N/A") mustEqual "0102.0304.0506"
    mac1.asCiscoFormat(':').getOrElse("N/A") mustEqual "0102:0304:0506"
    mac1.asBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
  }

  test("Leading Zero Macs") {
    val mac = new MacAddress("0:02:03:04:05:06")
    mac.isValid mustBe true
    mac.asString.getOrElse("N/A") mustEqual "000203040506"
    mac.asFormat().getOrElse("N/A") mustEqual "00:02:03:04:05:06"
    mac.asFormat('-').getOrElse("N/A") mustEqual "00-02-03-04-05-06"
    mac.asFormat('.').getOrElse("N/A") mustEqual "00.02.03.04.05.06"
    mac.asCiscoFormat().getOrElse("N/A") mustEqual "0002.0304.0506"
    mac.asCiscoFormat(':').getOrElse("N/A") mustEqual "0002:0304:0506"
    mac.asBytes mustEqual Array[Byte](0, 2, 3, 4, 5, 6)
  }

  test("Shorthand Zero Macs") {
    val mac = new MacAddress("01:2:03:04:05:06")
    mac.isValid mustBe true
    mac.asString.getOrElse("N/A") mustEqual "010203040506"
    mac.asFormat().getOrElse("N/A") mustEqual "01:02:03:04:05:06"
    mac.asFormat('-').getOrElse("N/A") mustEqual "01-02-03-04-05-06"
    mac.asFormat('.').getOrElse("N/A") mustEqual "01.02.03.04.05.06"
    mac.asCiscoFormat().getOrElse("N/A") mustEqual "0102.0304.0506"
    mac.asCiscoFormat(':').getOrElse("N/A") mustEqual "0102:0304:0506"
    mac.asBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
  }

  test("Invalid Short Mac") {
    val mac = new MacAddress("01:2:03:04:05")
    mac.isValid mustBe false
    mac.asString.getOrElse("N/A") mustEqual "N/A"
    mac.asFormat().getOrElse("N/A") mustEqual "N/A"
    mac.asFormat('-').getOrElse("N/A") mustEqual "N/A"
    mac.asFormat('.').getOrElse("N/A") mustEqual "N/A"
    mac.asCiscoFormat().getOrElse("N/A") mustEqual "N/A"
    mac.asCiscoFormat(':').getOrElse("N/A") mustEqual "N/A"
    mac.asBytes mustEqual Array[Byte]()
  }

  test("Invalid bad characters Mac") {
    val mac = new MacAddress("01:2:03:04:05:0g")
    mac.isValid mustBe false
    mac.asString.getOrElse("N/A") mustEqual "N/A"
    mac.asFormat().getOrElse("N/A") mustEqual "N/A"
    mac.asFormat('-').getOrElse("N/A") mustEqual "N/A"
    mac.asFormat('.').getOrElse("N/A") mustEqual "N/A"
    mac.asCiscoFormat().getOrElse("N/A") mustEqual "N/A"
    mac.asCiscoFormat(':').getOrElse("N/A") mustEqual "N/A"
    mac.asBytes mustEqual Array[Byte]()
  }

  test("Hex bytes conversion") {
    val mac = new MacAddress("48:65:6C:6C:6F:20")
    mac.isValid mustBe true
    mac.asFormat(' ').getOrElse("N/A") mustEqual "48 65 6C 6C 6F 20"
    val bytes = mac.hex2bytes(mac.asFormat(' ').getOrElse("N/A"))
    bytes mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
    val hex = mac.bytes2hex(bytes, Option(":"))
    hex mustEqual "48:65:6C:6C:6F:20".toLowerCase

    val mac2 = new MacAddress("48-65-6C-6C-6F-20")
    mac2.isValid mustBe true
    mac2.asFormat('-').getOrElse("N/A") mustEqual "48-65-6C-6C-6F-20"
    val bytes2 = mac2.hex2bytes(mac.asFormat('-').getOrElse("N/A"))
    bytes2 mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
    val hex2 = mac2.bytes2hex(bytes2, Option("-"))
    hex2 mustEqual "48-65-6C-6C-6F-20".toLowerCase

    val mac3 = new MacAddress("48656C6C6F20")
    mac3.isValid mustBe true
    mac3.asString.getOrElse("N/A") mustEqual "48656C6C6F20"
    val bytes3 = mac3.hex2bytes(mac.asString.getOrElse("N/A"))
    bytes3 mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
    val hex3 = mac3.bytes2hex(bytes3)
    hex3 mustEqual "48656C6C6F20".toLowerCase
  }
}
