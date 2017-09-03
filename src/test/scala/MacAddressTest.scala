import org.scalatest.{FunSuite, MustMatchers}

class MacAddressTest extends FunSuite with MustMatchers {
  test("Valid Mac") {
    MacAddress("01:02:03:04:05:06")
      .fold(fail => fail mustBe empty,mac => {
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
      })
  }

  test("Valid Mac with Hex") {
    MacAddress("0a:0B:0c:0D:0e:0F")
      .fold(fail => fail mustBe empty, mac => {
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
      })
  }

  test("Valid Mac String") {
    MacAddress("010203040506")
      .fold(fail => fail mustBe empty, mac => {
        mac.toString mustEqual "010203040506"
        mac.toHexidecimal mustEqual "01:02:03:04:05:06"
        mac.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
        mac.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
        mac.toDotNotation mustEqual "0102.0304.0506"
        mac.toDotNotation(':') mustEqual "0102:0304:0506"
        mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
        mac.toLong mustEqual 1108152157446L
      })
  }

  test("Valid Mac with dash") {
    MacAddress("01-02-03-04-05-06")
      .fold(fail => fail mustBe empty, mac => {
        mac.toString mustEqual "010203040506"
        mac.toHexidecimal mustEqual "01:02:03:04:05:06"
        mac.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
        mac.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
        mac.toDotNotation mustEqual "0102.0304.0506"
        mac.toDotNotation(':') mustEqual "0102:0304:0506"
        mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
        mac.toLong mustEqual 1108152157446L
      })
  }

  test("Valid Cisco Mac") {
    MacAddress("0102-0304-0506")
      .fold(fail => fail mustBe empty, mac => {
        mac.toString mustEqual "010203040506"
        mac.toHexidecimal mustEqual "01:02:03:04:05:06"
        mac.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
        mac.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
        mac.toDotNotation mustEqual "0102.0304.0506"
        mac.toDotNotation(':') mustEqual "0102:0304:0506"
        mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
        mac.toLong mustEqual 1108152157446L
      })

    MacAddress("0102.0304.0506")
      .fold(fail => fail mustBe empty, mac => {
        mac.toString mustEqual "010203040506"
        mac.toHexidecimal mustEqual "01:02:03:04:05:06"
        mac.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
        mac.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
        mac.toDotNotation mustEqual "0102.0304.0506"
        mac.toDotNotation(':') mustEqual "0102:0304:0506"
        mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
        mac.toLong mustEqual 1108152157446L
      })
  }

  test("Leading Zero Macs") {
    MacAddress("0:02:03:04:05:06")
      .fold(fail => fail mustBe empty, mac => {
        mac.toString mustEqual "000203040506"
        mac.toHexidecimal mustEqual "00:02:03:04:05:06"
        mac.toHexidecimal('-') mustEqual "00-02-03-04-05-06"
        mac.toHexidecimal('.') mustEqual "00.02.03.04.05.06"
        mac.toDotNotation mustEqual "0002.0304.0506"
        mac.toDotNotation(':') mustEqual "0002:0304:0506"
        mac.toBytes mustEqual Array[Byte](0, 2, 3, 4, 5, 6)
        mac.toLong mustEqual 8640529670L
      })
  }

  test("Shorthand Zero Macs") {
    MacAddress("01:2:03:4:05:06")
      .fold(fail => fail mustBe empty, mac => {
        mac.toString mustEqual "010203040506"
        mac.toHexidecimal mustEqual "01:02:03:04:05:06"
        mac.toHexidecimal('-') mustEqual "01-02-03-04-05-06"
        mac.toHexidecimal('.') mustEqual "01.02.03.04.05.06"
        mac.toDotNotation mustEqual "0102.0304.0506"
        mac.toDotNotation(':') mustEqual "0102:0304:0506"
        mac.toBytes mustEqual Array[Byte](1, 2, 3, 4, 5, 6)
        mac.toLong mustEqual 1108152157446L
      })
  }

  test("Invalid Short Mac") {
    val mac = MacAddress("01:2:03:04:05")
      .fold(fail => {
        fail mustEqual "Unable to recognize MAC format for 01:2:03:04:05"
      }, mac => {
        mac.toHexString mustEqual ""
      })
  }

  test("Invalid bad characters Mac") {
    val mac = MacAddress("01:2:03:04:05:0g")
      .fold(fail => {
        fail mustEqual "Unable to recognize MAC format for 01:2:03:04:05:0g"
      }, mac => {
        mac.toHexString mustEqual ""
      })
  }

  test("Hex bytes conversion") {
    MacAddress("48:65:6C:6C:6F:20")
      .fold(fail => fail mustBe empty, mac => {
        mac.toHexidecimal(' ') mustEqual "48 65 6c 6c 6f 20"
        mac.toBytes mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
        mac.toHexidecimal mustEqual "48:65:6c:6c:6f:20"
      })

    MacAddress("48-65-6c-6c-6f-20")
      .fold(fail => fail mustBe empty, mac => {
        mac.toHexidecimal('-') mustEqual "48-65-6c-6c-6f-20"
        mac.toBytes mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
        mac.toHexidecimal mustEqual "48:65:6c:6c:6f:20"
      })

    MacAddress("48656C6C6F20")
      .fold(fail => fail mustBe empty, mac => {
        mac.toString mustEqual "48656c6c6f20"
        mac.toBytes mustEqual Array[Byte](72, 101, 108, 108, 111, 32)
        mac.toHexidecimal mustEqual "48:65:6c:6c:6f:20"
      })
  }

  test("Random Mac Address Generator") {
    val randomMac = MacAddress.getRandomMACAddress
    val mac = MacAddress(randomMac).toList.head
    MacAddress(randomMac)
      .fold(fail => fail mustBe empty, mac => {
        mac.toString mustEqual randomMac.replaceAll(":","")
        mac.toHexidecimal mustEqual randomMac
        mac.toHexidecimal('-') mustEqual randomMac.replaceAll(":","-")
        mac.toHexidecimal('.') mustEqual randomMac.replaceAll(":",".")
        val ciscoRandomMac = randomMac.replaceAll(":","").substring(0,4) + "." +
          randomMac.replaceAll(":","").substring(4,8) + "." + randomMac.replaceAll(":","").substring(8,12)
        mac.toDotNotation mustEqual ciscoRandomMac
        mac.toDotNotation(':') mustEqual ciscoRandomMac.replaceAll("\\.", ":")
      })
  }
}
