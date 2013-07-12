val someNumbers = List(-11, -10, -5, 0, 5, 10)
someNumbers.foreach((x: Int) => println(x))
someNumbers.foreach(x => println(x))
someNumbers.foreach(println(_))
