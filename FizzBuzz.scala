val fizzBuzz = (1 to 100).map(n =>
    if (n%3 == 0 && n%5 == 0) "FizzBuzz"
    else if (n%3 == 0) "Fizz"
    else if (n%5 == 0) "Buzz"
    else n
)

println(fizzBuzz.mkString("\n"))
