package cross

class DummyClass extends Serializable {
  val var1 = "đĐe"
  val var2 = 123
  val var3 = 0.999
  val var4 = null
  val var5 = new {
    val x = Map(0 -> 123.4567, "k1" -> "value1", "k2" -> "Tiếng Việt")
  }
  val var6 = "a dynamic var"
}

object Data {
  val dataFile = "php-ser.data"
  val dummy = new DummyClass

  val data = Array(
    123456789012345D,
    123456789012345D,
    123.456,
    123.4567F,
    "Tiếng Việt",
    Array("Cường", "Đỗ", "Đức", "Gia", "Bảo"),
    Map(0 -> "Cường", 1 -> "Đỗ", 2 -> "Đức", 3 -> "Gia", 4 -> "Bảo"),
    Array(123, "abc"),
    Map(0 -> 123, 1 -> "abc"),
    Map("k1" -> "value1", "k2" -> "Tiếng Việt"),
    Map("k1" -> 123, "k2" ->
      "aAàÀảẢãÃáÁạẠăĂằẰẳẲẵẴắẮặẶâÂầẦẩẨẫẪấẤậẬbBcCdDđĐeEèÈẻẺẽẼéÉẹẸêÊềỀểỂễỄếẾệỆfFgGhHiIìÌỉỈĩĨíÍịỊjJkKlLmMnNoOòÒỏỎõÕóÓọỌôÔồỒổỔỗỖốỐộỘơƠờỜởỞỡỠớỚợỢpPqQrRsStTuUùÙủỦũŨúÚụỤưƯừỪửỬữỮứỨựỰvVwWxXyYỳỲỷỶỹỸýÝỵỴzZ")
  )

  val dummyExpected = "cross\\DummyClass" -> Map(
    "var1" -> dummy.var1,
    "var2" -> dummy.var2,
    "var3" -> dummy.var3,
    "var4" -> dummy.var4,
    "var5" -> ("stdClass", Map("x" -> Map("0" -> 123.4567, "k1" -> "value1", "k2" -> "Tiếng Việt"))),
    "var6" -> dummy.var6
  )

  val expectedData = Map(
    0 -> data(0),
    1 -> data(1),
    2 -> data(2),
    3 -> 123.4567, //java Float ser then unser => Double
    4 -> data(4),
    5 -> Map(0 -> "Cường", 1 -> "Đỗ", 2 -> "Đức", 3 -> "Gia", 4 -> "Bảo"), //Array ser then unser => Map[String,_]
    6 -> Map(0 -> "Cường", 1 -> "Đỗ", 2 -> "Đức", 3 -> "Gia", 4 -> "Bảo"), //Map[Int,_] --> Map[String,_]
    7 -> Map(0 -> 123, 1 -> "abc"),
    8 -> Map(0 -> 123, 1 -> "abc"),
    9 -> data(9),
    10 -> data(10)
  )
}
