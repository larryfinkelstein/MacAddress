import org.scalatest.{FunSuite, MustMatchers}

class MacAddressTest extends FunSuite with MustMatchers {
  test("Valid Mac") {
    val mac = MacAddress("01:02:03:04:05:06").toList.head
    mac.isValid mustBe true
    mac.toHexidecimal mustEqual "01:02:03:04:05:06"
    mac.toBitReversed mustEqual "01-02-03-04-05-06"
    mac.toByteString mustEqual "[01,02,03,04,05,06]"
    mac.toDotNotation mustEqual "0102.0304.0506"
    mac.toHexString mustEqual "010203040506"

    mac.toString mustEqual "010203040506"
    mac.toHexidecimal mustEqual "01:02:03:04:05:06"
    mac.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
    mac.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
    mac.toDotNotation mustEqual "0102.0304.0506"
    mac.toDotNotation(':') mustEqual "0102:0304:0506"
    mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 1108152157446L
  }

  test("Valid Mac with Hex") {
    val mac = MacAddress("0a:0B:0c:0D:0e:0F").toList.head
    mac.isValid mustBe true
    mac.toHexidecimal mustEqual "0a:0b:0c:0d:0e:0f"
    mac.toBitReversed mustEqual "0a-0b-0c-0d-0e-0f"
    mac.toByteString mustEqual "[0a,0b,0c,0d,0e,0f]"
    mac.toDotNotation mustEqual "0a0b.0c0d.0e0f"
    mac.toHexString mustEqual "0a0b0c0d0e0f"

    mac.toString mustEqual "0a0b0c0d0e0f"
    mac.toHexidecimal mustEqual "0a:0b:0c:0d:0e:0f"
    mac.toHexidecimal('-') mustEqual "0a-0b-0c-0d-0e-0f"
    mac.toHexidecimal('.') mustEqual "0a.0b.0c.0d.0e.0f"
    mac.toDotNotation mustEqual "0a0b.0c0d.0e0f"
    mac.toDotNotation(':') mustEqual "0a0b:0c0d:0e0f"
    mac.toBytes mustEqual Array[Byte](10, 11, 12, 13, 14, 15)
    mac.toLong mustEqual 11042563100175L
  }

  test("Valid Mac String") {
    val mac = MacAddress("010203040506").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "010203040506"
    mac.toHexidecimal mustEqual "01:02:03:04:05:06"
    mac.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
    mac.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
    mac.toDotNotation mustEqual "0102.0304.0506"
    mac.toDotNotation(':') mustEqual "0102:0304:0506"
    mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 1108152157446L
  }

  test("Valid Mac with dash") {
    val mac = MacAddress("01-02-03-04-05-06").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "010203040506"
    mac.toHexidecimal mustEqual "01:02:03:04:05:06"
    mac.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
    mac.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
    mac.toDotNotation mustEqual "0102.0304.0506"
    mac.toDotNotation(':') mustEqual "0102:0304:0506"
    mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 1108152157446L
  }

  test("Valid Cisco Mac") {
    val mac = MacAddress("0102-0304-0506").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "010203040506"
    mac.toHexidecimal mustEqual "01:02:03:04:05:06"
    mac.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
    mac.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
    mac.toDotNotation mustEqual "0102.0304.0506"
    mac.toDotNotation(':') mustEqual "0102:0304:0506"
    mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 1108152157446L

    val mac1 = MacAddress("0102.0304.0506").toList.head
    mac1.isValid mustBe true
    mac1.toString mustEqual "010203040506"
    mac1.toHexidecimal mustEqual "01:02:03:04:05:06"
    mac1.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
    mac1.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
    mac1.toDotNotation mustEqual "0102.0304.0506"
    mac1.toDotNotation(':') mustEqual "0102:0304:0506"
    mac1.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    mac1.toLong mustEqual 1108152157446L
  }

  test("Leading Zero Macs") {
    val mac = MacAddress("0:02:03:04:05:06").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "000203040506"
    mac.toHexidecimal mustEqual "00:02:03:04:05:06"
    mac.toHexidecimal('-') mustEqual "00-02-03-04-05-06"
    mac.toHexidecimal('.') mustEqual "00.02.03.04.05.06"
    mac.toDotNotation mustEqual "0002.0304.0506"
    mac.toDotNotation(':') mustEqual "0002:0304:0506"
    mac.toBytes mustEqual Array[Byte](0, 2, 3, 4, 5, 6)
    mac.toLong mustEqual 8640529670L
  }

  test("Shorthand Zero Macs") {
    val mac = MacAddress("01:2:03:04:05:06").toList.head
    mac.isValid mustBe true
    mac.toString mustEqual "010203040506"
    mac.toHexidecimal mustEqual "01:02:03:04:05:06"
    mac.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
    mac.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
    mac.toDotNotation mustEqual "0102.0304.0506"
    mac.toDotNotation(':') mustEqual "0102:0304:0506"
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
    mac.toHexidecimal(' ') mustEqual "48 65 6c 6c 6f 20"
    mac.toBytes mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
    mac.toHexidecimal mustEqual "48:65:6c:6c:6f:20"

    val mac2 = MacAddress("48-65-6c-6c-6f-20").toList.head
    mac2.isValid mustBe true
    mac2.toHexidecimal('-') mustEqual "48-65-6c-6c-6f-20"
    mac2.toBytes mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
    mac2.toHexidecimal mustEqual "48:65:6c:6c:6f:20"

    val mac3 = MacAddress("48656C6C6F20").toList.head
    mac3.isValid mustBe true
    mac3.toString mustEqual "48656c6c6f20"
    mac3.toBytes mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
    mac3.toHexidecimal mustEqual "48:65:6c:6c:6f:20"
  }

  test("Random Mac Address Generator") {
    val randomMac = MacAddress.getRandomMACAddress
    val mac = MacAddress(randomMac).toList.head
    mac.isValid mustBe true
    mac.toString mustEqual randomMac.replaceAll(":","")
    mac.toHexidecimal mustEqual randomMac
    mac.toHexidecimal('-') mustEqual randomMac.replaceAll(":","-")
    mac.toHexidecimal('.') mustEqual randomMac.replaceAll(":",".")
    val ciscoRandomMac = randomMac.replaceAll(":","").substring(0,4) + "." +
      randomMac.replaceAll(":","").substring(4,8) + "." + randomMac.replaceAll(":","").substring(8,12)
    mac.toDotNotation mustEqual ciscoRandomMac
    mac.toDotNotation(':') mustEqual ciscoRandomMac.replaceAll("\\.", ":")
    //mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
    //mac.toLong mustEqual 1108152157446L

  }
}
