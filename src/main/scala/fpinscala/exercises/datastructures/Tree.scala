package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) =>
      val ld = l.depth + 1
      val rd = r.depth + 1
      ld.max(rd)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  
  def sizeViaFold: Int = 
    this.fold((_) => 1, (s1, s2) => 1+s1+s2)
  
  def depthViaFold: Int = 
    this.fold((_) => 0,  _.max(_)+1)
  
  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(a => Leaf(f(a)), (l, r) => Branch(l, r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l, r) => 
      val lpos = l.firstPositive
      if lpos > 0 then lpos else r.firstPositive
  

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lmax = l.maximum
      val rmax = r.maximum
      lmax.max(rmax)
  

  extension (t: Tree[Int]) def maximumViaFold: Int = 
    t.fold(a=>a, _.max(_))
  
