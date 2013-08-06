def factorial(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, acc: Int): Int =
    if 
      (n <= 0) acc
    else 
      go(n-1, n*acc)
      
  go(n, 1)
}

def partial1[A,B,C](a: A, f: (A,B) => C): B => C = 
  new Function1[B, C] { 
    def apply(b:B) = f(a,b)
  }


def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
  new Function1[A, B=>C] {
    def apply(a:A) =
      new Function1[B, C] { 
        def apply(b:B) = f(a,b)
      } 

  }

def uncurry[A,B,C](f: A => B => C): (A, B) => C = 
  new Function2[A,B,C] {
    def apply(a:A,b:B) = f(a)(b)
  }

def compose[A,B,C](f: B => C, g: A => B): A => C = 
  new Function1[A,C]{
    def apply(a: A) = f(g(a))
  }