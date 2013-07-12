/*
 * Won't get tail recursion optimized by the compiler
 * See stack trace
 */
def boom(x: Int): Int =
  if (x == 0) throw new Exception("boom!")
  else boom(x - 1) + 1

// boom(3)

/*
 * Will get tail recursion optimized by the compiler
 * See stack trace
 */
def bang(x: Int): Int =
  if (x == 0) throw new Exception("bang!")
  else bang(x - 1)

bang(3)
