def sum(a: Int, b: Int, c: Int) = a + b + c

sum(1, 2, 3)

val a = sum _

a(1, 2, 3) == a.apply(1, 2, 3)

val b = sum(1, _: Int, 3)

b(2) == 6

val more = 1
def addMore = (x: Int) => x + more
addMore(10) == 11

def makeIncreaser(more: Int) = (x: Int) => x + more
val inc1 = makeIncreaser(1)
val inc99 = makeIncreaser(99)
