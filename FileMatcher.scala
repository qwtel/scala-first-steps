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
    filesMatching(query,
      (fileName: String, query: String) => fileName.endsWith(query)
    )

  def filesContaining(query: String) =
    filesMatching(query,
      (fileName, query) => fileName.contains(query)
    )

  def filesRegex(query: String) =
    filesMatching(query, _.matches(_))

  def filesMatching(query: String, matcher: (String, String) => Boolean) = {
    for {
      file <- filesHere
      if matcher(file.getName, query)
    } yield file
  }
}
