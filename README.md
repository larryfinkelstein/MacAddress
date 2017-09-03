# MacAddress

Example to validate and reformat MAC addresses into a number of different formats.

## Validation

Validation is run during the apply method.  Invaild MAC
addresses will return a failure, with an error message.

```
val goodMac = MacAddress("A:B:C:D:E:F")
goodMac: scalaz.Validation[String,MacAddress] = Success(0a0b0c0d0e0f)
 
val badMac = MacAddress("01:2:03:04:05:0g")
badMac: scalaz.Validation[String,MacAddress] = Failure(Unable to recognize MAC format for 01:2:03:04:05:0g)
```

## Formats

```
val mac = MacAddress("0a:0B:0c:0D:0e:0F").toList.head
mac: MacAddress = 0a0b0c0d0e0f
 
mac.toHexidecimal
res1: String = 0a:0b:0c:0d:0e:0f
 
mac.toBitReversed
res2: String = 0a-0b-0c-0d-0e-0f
 
mac.toByteString
res3: String = [0a,0b,0c,0d,0e,0f]
 
mac.toDotNotation
res4: String = 0a0b.0c0d.0e0f
 
mac.toHexString
res5: String = 0a0b0c0d0e0f
```

## Generate random mac
```
MacAddress.getRandomMACAddress
res6: String = b5:bc:ce:52:db:b8
```

