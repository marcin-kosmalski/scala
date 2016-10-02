object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(835); 
  def times(chars: List[Char]): List[(Char, Int)] = {

    def timesAcc(chars: List[Char]): List[(Char, Int)] = {
      if (chars.isEmpty)
        Nil
      else
        timesAcc(chars.tail) ::: List((chars.head, 1))
    }

    def merge(pairs: List[(Char, Int)]): List[(Char, Int)] = {
			println(pairs)
      pairs match {
        case List()  => List()
        case y :: ys => mergeSingle(y,merge(ys),false)
      }
    }
    
    def mergeSingle(pair:(Char, Int),pairs:List[(Char, Int)],added:Boolean):List[(Char, Int)]={
    	 println(pair+":"+pairs)
    	 pairs match {
        case List()  => if(added) List() else List((pair._1,1))
        case y :: ys => if(y._1==pair._1) (y._1,y._2+1)::mergeSingle(pair,ys,true) else (y._1,y._2)::mergeSingle(pair,ys,false||added)
      }
    }

    merge(timesAcc(chars));
  };System.out.println("""times: (chars: List[Char])List[(Char, Int)]""");$skip(46); val res$0 = 

  times(List('c','a', 'b','a','a','b','d' ));System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0))}

}
