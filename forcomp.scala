val filesHere = (new java.io.File(".")).listFiles

val scalaFiles =
  for {
    file <- filesHere
    if file.isFile
    if file.getName.endsWith(".scala")
  } yield file

for (sc <- scalaFiles) println(sc)

