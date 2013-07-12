def sum(a: Int, b: Int, c: Int) = a + b + c

sum(1, 2, 3)

val a = sum _

a(1, 2, 3) == a.apply(1, 2, 3)

val b = sum(1, _: Int, 3)

b(2) == 6
