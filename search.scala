/**
  int i = 0;                // This is Java
  boolean foundIt = false;
  while (i < args.length) {
    if (args[i].startsWith("-")) {
      i = i + 1;
      continue;
    }
    if (args[i].endsWith(".scala")) {
      foundIt = true;
      break;
    }
    i = i + 1;
  }
 */

def searchIt() = {
  var i = -1
  var foundIt = false

  while (i < args.length && !foundIt) {
    i = i + 1
    if (!args(i).startsWith("-")) {
      if (args(i).endsWith(".scala"))
        foundIt = true
    }
  }
  i
}

println(searchIt())

def searchFrom(i: Int): Int =
  if (i >= args.length) -1 // not found
  else if (args(i).startsWith("-")) searchFrom(i + 1) // keep searching
  else if (args(i).endsWith(".scala")) i // found it
  else searchFrom(i + 1) // keep searching

println(searchFrom(0))
