def plainOldSum(x: Int, y: Int) = x + y
plainOldSum(1, 2) == 3

def curriedSum(x: Int)(y: Int) = x + y
curriedSum(1)(2) == 3

// what's actually happening:
def first(x: Int) = (y: Int) => x + y
val second = first(1)
second(2) == 3

// get the "second" function of curriedSum:
val onePlus = curriedSum(1)_
onePlus(2) == 3

val twoPlus = curriedSum(2)_
twoPlus(2) == 4
