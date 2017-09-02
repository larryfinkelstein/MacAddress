import org.scalatest.{FunSuite, MustMatchers}

class MacAddressTest extends FunSuite with MustMatchers {
  test("Valid Mac") {
    val mac = MacAddress("01:02:03:04:05:06").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "010203040506"
    mac.toFormat mustEqual "01:02:03:04:05:06"
    mac.toFormat('-') mustEqual "01-02-03-04-05-06"
    mac.toFormat('.') mustEqual "01.02.03.04.05.06"
    mac.toCiscoFormat mustEqual "0102.0304.0506"
    mac.toCiscoFormat(':') mustEqual "0102:0304:0506"
    mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 1108152157446L
  }

  test("Valid Mac String") {
    val mac = MacAddress("010203040506").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "010203040506"
    mac.toFormat mustEqual "01:02:03:04:05:06"
    mac.toFormat('-') mustEqual "01-02-03-04-05-06"
    mac.toFormat('.') mustEqual "01.02.03.04.05.06"
    mac.toCiscoFormat mustEqual "0102.0304.0506"
    mac.toCiscoFormat(':') mustEqual "0102:0304:0506"
    mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 1108152157446L
  }

  test("Valid Mac with dash") {
    val mac = MacAddress("01-02-03-04-05-06").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "010203040506"
    mac.toFormat mustEqual "01:02:03:04:05:06"
    mac.toFormat('-') mustEqual "01-02-03-04-05-06"
    mac.toFormat('.') mustEqual "01.02.03.04.05.06"
    mac.toCiscoFormat mustEqual "0102.0304.0506"
    mac.toCiscoFormat(':') mustEqual "0102:0304:0506"
    mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 1108152157446L
  }

  test("Valid Cisco Mac") {
    val mac = MacAddress("0102-0304-0506").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "010203040506"
    mac.toFormat mustEqual "01:02:03:04:05:06"
    mac.toFormat('-') mustEqual "01-02-03-04-05-06"
    mac.toFormat('.') mustEqual "01.02.03.04.05.06"
    mac.toCiscoFormat mustEqual "0102.0304.0506"
    mac.toCiscoFormat(':') mustEqual "0102:0304:0506"
    mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 1108152157446L

    val mac1 = MacAddress("0102.0304.0506").toList.head
    mac1.isValid mustBe true
    mac1.toString mustEqual "010203040506"
    mac1.toFormat mustEqual "01:02:03:04:05:06"
    mac1.toFormat('-') mustEqual "01-02-03-04-05-06"
    mac1.toFormat('.') mustEqual "01.02.03.04.05.06"
    mac1.toCiscoFormat mustEqual "0102.0304.0506"
    mac1.toCiscoFormat(':') mustEqual "0102:0304:0506"
    mac1.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac1.toLong mustEqual 1108152157446L
  }

  test("Leading Zero Macs") {
    val mac = MacAddress("0:02:03:04:05:06").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "000203040506"
    mac.toFormat mustEqual "00:02:03:04:05:06"
    mac.toFormat('-') mustEqual "00-02-03-04-05-06"
    mac.toFormat('.') mustEqual "00.02.03.04.05.06"
    mac.toCiscoFormat mustEqual "0002.0304.0506"
    mac.toCiscoFormat(':') mustEqual "0002:0304:0506"
    mac.toBytes mustEqual Array[Byte](0, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 8640529670L
  }

  test("Shorthand Zero Macs") {
    val mac = MacAddress("01:2:03:04:05:06").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "010203040506"
    mac.toFormat mustEqual "01:02:03:04:05:06"
    mac.toFormat('-') mustEqual "01-02-03-04-05-06"
    mac.toFormat('.') mustEqual "01.02.03.04.05.06"
    mac.toCiscoFormat mustEqual "0102.0304.0506"
    mac.toCiscoFormat(':') mustEqual "0102:0304:0506"
    mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 1108152157446L
  }

  test("Invalid Short Mac") {
    val mac = MacAddress("01:2:03:04:05")
    mac.isFailure mustBe true
    mac.fold(fail => fail mustEqual "Unable to recognize MAC format for 01:2:03:04:05", mac => println(mac))
    mac.toList mustBe empty
  }

  test("Invalid bad characters Mac") {
    val mac = MacAddress("01:2:03:04:05:0g")
    mac.isFailure mustBe true
    mac.fold(fail => fail mustEqual "Unable to recognize MAC format for 01:2:03:04:05:0g", mac => println(mac))
    mac.toList mustBe empty
  }

  test("Hex bytes conversion") {
    val mac = MacAddress("48:65:6C:6C:6F:20").toList.head
    mac.isValid mustBe true
    mac.toFormat(' ') mustEqual "48 65 6C 6C 6F 20"
    mac.toBytes mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
    mac.toFormat mustEqual "48:65:6C:6C:6F:20"

    val mac2 = MacAddress("48-65-6C-6C-6F-20").toList.head
    mac2.isValid mustBe true
    mac2.toFormat('-') mustEqual "48-65-6C-6C-6F-20"
    mac2.toBytes mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
    mac2.toFormat mustEqual "48:65:6C:6C:6F:20"

    val mac3 = MacAddress("48656C6C6F20").toList.head
    mac3.isValid mustBe true
    mac3.toString mustEqual "48656C6C6F20"
    mac3.toBytes mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
    mac3.toString mustEqual "48656C6C6F20"
  }
}
