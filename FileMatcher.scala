import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: cell303
 * Date: 7/12/13
 * Time: 3:52 PM
 * To change this template use File | Settings | File Templates.
 */
object FileMatcher {
  private def filesHere = (new File(".")).listFiles

  def filesEnding(query: String) =
    filesMatching((fileName: String) => fileName.endsWith(query))

  def filesContaining(query: String) =
    filesMatching((fileName) => fileName.contains(query))

  def filesRegex(query: String) =
    filesMatching(_.matches(query))

  private def filesMatching(matcher: String => Boolean) = {
    for {
      file <- filesHere
      if matcher(file.getName)
    } yield file
  }
}
