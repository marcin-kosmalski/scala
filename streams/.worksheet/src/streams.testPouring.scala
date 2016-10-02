package streams

object testPouring {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(77); 
 val problem=new Pouring(Vector(4,7,8));System.out.println("""problem  : streams.Pouring = """ + $show(problem ));$skip(25); 
 
 
 val m=new Solver();;System.out.println("""m  : <error> = """ + $show(m ))}
 
 
}
