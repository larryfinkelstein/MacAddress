# MacAddress

Example to validate and reformat MAC addresses into a number of different formats.

## Validation
```
val mac = new MacAddress("01:2:03:04:05:06")
mac.isValid
res0: Boolean = true
```

## Formats

```
mac.asString
res1: Option[String] = Some(010203040506)
 
mac.asFormat()
res2: Option[String] = Some(01:02:03:04:05:06)
 
mac.asCiscoFormat()
res3: Option[String] = Some(0102.0304.0506)
 
mac.asBytes
res4: Array[Byte] = Array(1, 2, 3, 4, 5, 6) 
```
