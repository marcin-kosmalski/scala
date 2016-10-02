object ws2 {

  val x = List(('a', 1), ('d', 6), ('l', 1), ('r', 1))
                                                  //> x  : List[(Char, Int)] = List((a,1), (d,6), (l,1), (r,1))
  // val y = List(('r', 1),('d',1))
  val y = List(('r', 1))                          //> y  : List[(Char, Int)] = List((r,1))

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
  }                                               //> subtract: (x: List[(Char, Int)], y: List[(Char, Int)])List[(Char, Int)]

  subtract(x,y)                                   //> res0: List[(Char, Int)] = List((d,6), (a,1), (l,1))

}