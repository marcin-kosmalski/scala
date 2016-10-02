object ws2 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(68); 

  val x = List(('a', 1), ('d', 6), ('l', 1), ('r', 1));System.out.println("""x  : List[(Char, Int)] = """ + $show(x ));$skip(61); 
  // val y = List(('r', 1),('d',1))
  val y = List(('r', 1));System.out.println("""y  : List[(Char, Int)] = """ + $show(y ));$skip(636); 

  def subtract(x: List[(Char, Int)], y: List[(Char, Int)]): List[(Char, Int)] = {
    def convertToMap(x: List[(Char, Int)]): Map[Char, Int] =
      x.groupBy(x => x._1).mapValues(listOfWords => listOfWords.map(wordPair => wordPair._2)).mapValues { x => x.head }

    def subtractPaar(map: Map[Char, Int], paar: (Char, Int)): Map[Char, Int] = {
      var (char, num) = paar
      map + (char -> (map(char) - num))
    }

    def subtractMap(mapX: Map[Char, Int], mapY: Map[Char, Int]): Map[Char, Int] = {
      mapY.foldLeft(mapX)(subtractPaar).filter(p => p._2 > 0)
    }

    subtractMap(convertToMap(x), convertToMap(y)).toList
  };System.out.println("""subtract: (x: List[(Char, Int)], y: List[(Char, Int)])List[(Char, Int)]""");$skip(17); val res$0 = 

  subtract(x,y);System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0))}

}
