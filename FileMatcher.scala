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
    for {
      file <- filesHere
      if file.getName.endsWith(query)
    } yield file

  def filesContaining(query: String) =
    for {
      file <- filesHere
      if file.getName.contains(query)
    } yield file

  def filesRegex(query: String) =
    for {
      file <- filesHere
      if file.getName.matches(query)
    } yield file
}
