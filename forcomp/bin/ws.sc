object ws {

  val x = List(('a', 1), ('d', 6), ('l', 1), ('r', 1))
                                                  //> x  : List[(Char, Int)] = List((a,1), (d,6), (l,1), (r,1))
  val y = List(('r', 1),('d',1))                  //> y  : List[(Char, Int)] = List((r,1), (d,1))

  def subtract(x: List[(Char, Int)], y: List[(Char, Int)]):  List[List[(Char, Int)]] = Nil
                                                  //> subtract: (x: List[(Char, Int)], y: List[(Char, Int)])List[List[(Char, Int)]
                                                  //| ]
 
  
   def convertToMap(x: List[(Char, Int)]): Map[Char, Int] =
    x.groupBy(x => x._1).mapValues(listOfWords => listOfWords.map(wordPair => wordPair._2)).mapValues { x => x.head }
                                                  //> convertToMap: (x: List[(Char, Int)])Map[Char,Int]
  
  def m(char:Char,mapX:Map[Char, Int],mapY:Map[Char, Int]):Map[Char, Int]={
  	if (mapY.get(char).isDefined){
  		mapX.updated(char, mapX.get(char).get - mapY.apply(char)).filter(p => p._2 >0)
  	}else{
  		mapX
  	}
  }                                               //> m: (char: Char, mapX: Map[Char,Int], mapY: Map[Char,Int])Map[Char,Int]
  
  def iterate(list:List[Char],result:Map[Char,Int],mapX:Map[Char, Int],mapY:Map[Char, Int]):Map[Char,Int]={
  	if(list.isEmpty){
  		result
  	}
  	else{
  		iterate(list.tail,m(list.head,result,mapY),mapX,mapY);
  	}
  }                                               //> iterate: (list: List[Char], result: Map[Char,Int], mapX: Map[Char,Int], mapY
                                                  //| : Map[Char,Int])Map[Char,Int]
  
   def k(x: List[(Char, Int)], y: List[(Char, Int)]): Map[Char, Int] = {
    var mapX = convertToMap(x)
    val mapY = convertToMap(y)
    
    
    val list=x.foldLeft("")((p1, p2) => p1 + p2._1).toList
    
    iterate(list,mapX,mapX,mapY)
   }                                              //> k: (x: List[(Char, Int)], y: List[(Char, Int)])Map[Char,Int]
  
 		k(x,y)                            //> res0: Map[Char,Int] = Map(d -> 5, a -> 1, l -> 1)
}