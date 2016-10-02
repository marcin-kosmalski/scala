

object test extends App {
  val x = 1 //> x  : Int = 1
  def increase(i: Int) = i + 1 //> increase: (i: Int)Int
  println(increase(x))

  
  def test1(x:Int)= x+1;
  
  def mycaller(f:Int=>Int)(x:Int)=f(x)
  
  println(mycaller(x=>x+1)(3));
   println(mycaller(test1)(3));
  
   
   val m=new Test(3)
   println(m++8);
   
   class Test(x:Int){
     
     def ++ (x:Int)=x+1;
     
   }
  
}