type Set = Int => Boolean

def contains(s: Set , elem: Int): Boolean = s(elem)

def positiveInt: Set = x => x > 0

//contains(positiveInt, 2)

def singletonSet(elem: Int): Set = (x: Int) => elem == x

val singleSetExample = singletonSet(-1)

def union(s: Set , t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)

//union(singleSetExample, positiveInt)(2)

def intersect(s: Set , t: Set) : Set = (x: Int) => contains(s, x) && contains(t, x)

//intersect(singleSetExample, positiveInt)(2)

def diff(s: Set , t: Set): Set = (x: Int) => contains(s, x) && !contains(t, x)

//diff(singleSetExample, positiveInt)(-1)

def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x) && p(x)

//filter(positiveInt, (x) => x > 3)(4)

def forall(s: Set , p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if(a > 1000) true
    else if(contains(s, a)) p(a) && iter(a + 1)
    else iter(a + 1)
  }
  iter(-1000)
}

//forall(positiveInt, (x) => x > 1)

def exists(s: Set , p: Int => Boolean): Boolean = !forall(s, x => !p(x))

exists(positiveInt, x => x < -2)


def map(s: Set, f: Int => Int): Set = y => exists(s, x => f(x) == y)

//map(positiveInt, x => 1)(1)