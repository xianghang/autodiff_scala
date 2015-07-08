package stream_test

trait D {
  def fv: Double
  def df: D
  
  def +(g: D): D = {
    DImpl.cons(fv+g.fv, df + g.df) 
  }
  
  
  def *(g: D): D = {
    DImpl.cons(fv * g.fv, this * g.df + df * g)
  }
  
  def take(n: Int): List[Double] = {
    if (n == 0) {
      Nil
    } else {
      fv :: df.take(n - 1)
    }
  }
  
}

object DImpl {
  def cons(f: => Double, d: => D) = new D {
    val fv = f
    lazy val df = d
  }

  def zeros(): D = {
    cons(0, zeros())
  }
  
  def cnst(c: => Double): D = {
    cons(c, zeros())
  }
  
  def ident(x: => Double): D = {
    cons(x, cons(1, zeros))
  }
  
  def comp(f: Double => Double, fp: D => D)(g: D): D = {
    cons(f(g.fv), fp(g) * g.df) 
  }

  def sin(g: D): D = {
//    cons(math.sin(g.fv), cos(g) * g.df) 
    comp(math.sin, cos)(g)
  }
  
  def cos(g: D): D = {
//    cons(math.cos(f.fv), cnst(-1) * sin(f) * f.df)
    val minusSin = (x: D) => cnst(-1.0) * sin(x)
    comp(math.cos, minusSin)(g)
  }
}

object stream_test {
  def main(args: Array[String]) {
    val s0 = Stream.cons(1, Stream.cons(2, Stream.empty))
//    println("hello")
    val s1 = (1 to 10).toStream
    println(s1.take(2).toList)
    
    val zs: D = DImpl.zeros
    println(zs.take(5))
    
    val f0 = DImpl.cnst(1) * DImpl.cnst(2)
    println(f0.take(5))
    
    val f1 = DImpl.ident(0.5) * DImpl.cnst(2)
    println(f1.take(5))   
    
    val f2 = (x:Double) => { DImpl.ident(x) * DImpl.ident(x) }
    println(f2(2).take(5))   
    println(f2(3).take(5))   


    val f3 = (x:Double) => { DImpl.ident(x) * DImpl.ident(x) * DImpl.ident(x)}
    println(f3(2).take(5))   
    println(f3(3).take(5))   
    
   
    val f4 = (x:Double) => DImpl.sin(DImpl.ident(x))
    println(f4(math.Pi/2).take(8))
  }
}