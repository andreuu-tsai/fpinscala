package fpinscala.exercises.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList1: List[A] =
    this match
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    
  def toList: List[A] =
    @annotation.tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] =
      ll match
        case Empty => acc.reverse
        case Cons(h, t) => go(t(), h() :: acc)
    go(this, Nil)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B]=
    foldRight((init, LazyList(init))): (a, b0) => 
      lazy val b1 = b0
      val b2 = f(a, b1(0))
      (b2, cons(b2, b1(1)))
    ._2

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    this match
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case _ => empty
  
  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((n, this)):
      case (1, Cons(h, t)) => Some(h(), (0, empty))
      case (n, Cons(h, t)) if n > 1 => Some((h(), (n-1, t())))
      case _ => None

  def drop(n: Int): LazyList[A] =
    this match
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    this match
      case Cons(h, t) if p(h())=> cons(h(), t().takeWhile(p))
      case _ => empty
  
  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else empty)

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None

  def zipWith[B,C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)):
      case (Cons(a, t1), Cons(b, t2)) => Some((f(a(), b()), (t1(), t2())))
      case _ => None
  
  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)):
      case (Empty, Empty) => None
      case (Cons(a, t1), Empty) => Some(((Some(a()), None), (t1(), empty)))
      case (Empty, Cons(b, t2)) => Some(((None, Some(b())), (empty, t2())))
      case (Cons(a, t1), Cons(b, t2)) => Some(((Some(a()), Some(b())), (t1(), t2())))

  def startsWith[A](prefix: LazyList[A]): Boolean =
    zipAll(prefix).forAll((oba, obb) => (oba, obb) match
      case (Some(_), None) => true
      case (None, None) => true
      case (None, Some(_)) => false
      case (Some(a1), Some(a2)) if a1 == a2 => true
      case _ => false
     )
  
  def tails: LazyList[LazyList[A]] =
    unfold(this)(s => s match
      case Cons(h, t) => Some((cons(h(), t()), t()))
      case Empty => None
    ).append(LazyList(empty))

  def hasSubsequence[A](l: LazyList[A]): Boolean =
    tails.exists(_.startsWith(l))

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] = 
    foldRight(empty)((a, acc) => cons(f(a), acc))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this)(
      s => s match 
        case Empty => None
        case Cons(h, t) => Some(f(h()), t())
      )

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty)((a, acc) => if f(a) then cons(a, acc) else acc)
  
  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, acc) => cons(a, acc))
  
  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)((a, acc) => f(a).append(acc))

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  lazy val fibs: LazyList[Int] =
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current+next))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))(s => Some((s._1, (s._2, s._1+s._2))))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(n => Some(n, n+1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)(a => Some(a, a))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(())(_ => Some(1, ()))
