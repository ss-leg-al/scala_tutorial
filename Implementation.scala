package kuplrg

object Implementation extends Template {

  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a*b*c

  def concat(x: String, y: String): String = x+y

  def mulN(n: Int): Int => Int = (a: Int)=>(n*a)

  def twice(f: Int => Int): Int => Int = (a:Int)=>f(a)

  def compose(f: Int => Int, g: Int => Int): Int => Int =  (a:Int)=>f(g(a))

  def double(l: List[Int]): List[Int] = l.map(a=>(a*2))

  def product(l: List[Int]): Int = l match{
      case x::Nil=>x
      case x::xs => (x*product(xs))
      case Nil => 1
  }

  def getOrNotFound(m: Map[String, Int], s: String): Int = m.getOrElse(s,error("Not Found"))
  
  def depth(t: Tree): Int = t match{
    case Leaf(a)=>0
    case Branch(a,children)=>1+children.map(depth).max
  }
  

  def sum(t: Tree): Int = t match{
    case Leaf(a)=>a
    case Branch(a,children)=>a+(children.map(sum).sum)
  }

  def countLeaves(t: Tree): Int = t match{
    case Leaf(a)=>1
    case Branch(a,children)=>children.map(countLeaves).sum
  }

def flatten(t: Tree): List[Int] = t match{
    case Leaf(a)=>List(a)
    case Branch(a,children)=>a::children.flatMap(flatten)
  }
}

