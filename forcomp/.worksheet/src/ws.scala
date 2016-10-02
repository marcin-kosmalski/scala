object ws {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(68); 

  val x = List(('a', 1), ('d', 6), ('l', 1), ('r', 1));System.out.println("""x  : List[(Char, Int)] = """ + $show(x ));$skip(33); 
  val y = List(('r', 1),('d',1));System.out.println("""y  : List[(Char, Int)] = """ + $show(y ));$skip(93); 

  def subtract(x: List[(Char, Int)], y: List[(Char, Int)]):  List[List[(Char, Int)]] = Nil;System.out.println("""subtract: (x: List[(Char, Int)], y: List[(Char, Int)])List[List[(Char, Int)]]""");$skip(183); 
 
  
   def convertToMap(x: List[(Char, Int)]): Map[Char, Int] =
    x.groupBy(x => x._1).mapValues(listOfWords => listOfWords.map(wordPair => wordPair._2)).mapValues { x => x.head };System.out.println("""convertToMap: (x: List[(Char, Int)])Map[Char,Int]""");$skip(224); 
  
  def m(char:Char,mapX:Map[Char, Int],mapY:Map[Char, Int]):Map[Char, Int]={
  	if (mapY.get(char).isDefined){
  		mapX.updated(char, mapX.get(char).get - mapY.apply(char)).filter(p => p._2 >0)
  	}else{
  		mapX
  	}
  };System.out.println("""m: (char: Char, mapX: Map[Char,Int], mapY: Map[Char,Int])Map[Char,Int]""");$skip(225); 
  
  def iterate(list:List[Char],result:Map[Char,Int],mapX:Map[Char, Int],mapY:Map[Char, Int]):Map[Char,Int]={
  	if(list.isEmpty){
  		result
  	}
  	else{
  		iterate(list.tail,m(list.head,result,mapY),mapX,mapY);
  	}
  };System.out.println("""iterate: (list: List[Char], result: Map[Char,Int], mapX: Map[Char,Int], mapY: Map[Char,Int])Map[Char,Int]""");$skip(250); 
  
   def k(x: List[(Char, Int)], y: List[(Char, Int)]): Map[Char, Int] = {
    var mapX = convertToMap(x)
    val mapY = convertToMap(y)
    
    
    val list=x.foldLeft("")((p1, p2) => p1 + p2._1).toList
    
    iterate(list,mapX,mapX,mapY)
   };System.out.println("""k: (x: List[(Char, Int)], y: List[(Char, Int)])Map[Char,Int]""");$skip(13); val res$0 = 
  
 		k(x,y);System.out.println("""res0: Map[Char,Int] = """ + $show(res$0))}
}
