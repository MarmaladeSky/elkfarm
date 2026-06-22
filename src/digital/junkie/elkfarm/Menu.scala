package digital.junkie.elkfarm

import cats.effect.Sync

import java.io.File
import scala.scalanative.libc.stdio.getchar
import scala.scalanative.libc.stdlib.system
import scala.scalanative.unsafe.*

object Menu {

  // Ctrl+C
  object Interrupted extends RuntimeException("interrupted by user") {
    override def fillInStackTrace(): Throwable = this
  }

  def select[F[_], A](
      options: Seq[A],
      title: String = "",
      show: A => String = (a: A) => a.toString
  )(implicit F: Sync[F]): F[A] = F.blocking {
    require(options.nonEmpty, "Menu.select requires at least one option")

    val n       = options.size
    var current = 0

    def render(): Unit = {
      for ((opt, i) <- options.zipWithIndex) {
        val label =
          if (i == current) s"> ${show(opt)}" // selected marker
          else s"  ${show(opt)}"
        print(s"[2K$label\r\n") // clear line, then draw
      }
      Console.flush()
    }

    if (title.nonEmpty) println(title)
    val _ = system(
      c"stty -echo -icanon -isig min 1 time 0"
    ) // raw-ish: no echo, char-at-a-time
    try {
      print("[?25l") // hide cursor
      render()
      var chosen = -1
      while (chosen < 0) {
        getchar() match {
          case 27 => // ESC: possible arrow-key sequence "ESC [ A/B"
            if (getchar() == 91) {
              getchar() match {
                case 65 => current = math.max(0, current - 1)     // up
                case 66 => current = math.min(n - 1, current + 1) // down
                case _  => ()
              }
            }
          case 'k'     => current = math.max(0, current - 1)
          case 'j'     => current = math.min(n - 1, current + 1)
          case 10 | 13 => chosen = current  // Enter (LF / CR)
          case 3       => throw Interrupted // Ctrl-C: abort cleanly
          case _       => ()
        }
        if (chosen < 0) {
          print(s"[${n}A") // cursor back up to the first option
          render()
        }
      }
      options(chosen)
    } finally {
      print("[?25h") // show cursor
      Console.flush()
      val _ = system(c"stty sane") // restore terminal
    }
  }

  /** Interactive search box on the first line with up to `window` result rows
    * below. `candidates(query)` produces the rows for the current query; the
    * user types to refine it, Down moves focus into the list, Up/Down navigate,
    * and Enter selects the highlighted row (Enter from the box picks the top
    * result). Typing or Backspace in the list returns focus to the box. If
    * `descend(row)` is `Some(path)`, Enter instead replaces the query with
    * `path` and keeps browsing rather than selecting the row. Pressing Tab in
    * the box replaces the query with `complete(firstRow)` when it is defined.
    */
  private def runSearch[F[_], A](
      candidates: String => Seq[A],
      descend: A => Option[String],
      complete: A => Option[String],
      title: String,
      show: A => String,
      window: Int,
      initial: String
  )(implicit F: Sync[F]): F[A] = F.blocking {

    val esc        = ""
    var query      = initial
    var current    = 0    // highlighted index into the filtered view
    var offset     = 0    // index of the first visible option (scroll position)
    var focusInput = true // true: editing query, false: navigating the list

    def view: Seq[A] = candidates(query)

    def isSep(c: Char): Boolean = c == '/' || c == ' '

    def render(): Unit = {
      val v = view
      // scroll so the highlighted row stays within the visible window
      if (current >= offset + window) offset = current - window + 1
      if (current < offset) offset = current
      offset = math.max(0, math.min(offset, math.max(0, v.size - window)))

      val caret = if (focusInput) s"$esc[7m $esc[0m" else ""
      print(s"$esc[2K> $query$caret\r\n") // search line + visible caret
      for (row <- 0 until window) {
        val idx = offset + row
        if (idx < v.size) {
          val label = show(v(idx))
          if (!focusInput && idx == current)
            print(s"$esc[2K> $label\r\n") // selected marker
          else
            print(s"$esc[2K  $label\r\n")
        } else if (row == 0 && v.isEmpty) {
          print(s"$esc[2K  (no matches)\r\n")
        } else {
          print(s"$esc[2K\r\n") // blank, clear any leftover from a wider view
        }
      }
      Console.flush()
    }

    if (title.nonEmpty) println(title)
    val _ = system(c"stty -echo -icanon -isig min 1 time 0")
    try {
      print(s"$esc[?25l") // hide cursor; we draw our own caret
      render()
      var chosen = -1
      while (chosen < 0) {
        getchar() match {
          case 27 => // ESC: possible arrow-key sequence "ESC [ A/B"
            if (getchar() == 91) {
              getchar() match {
                case 65 => // up
                  if (!focusInput) {
                    if (current == 0)
                      focusInput = true // back into the search box
                    else current -= 1
                  }
                case 66 => // down
                  if (focusInput) {
                    if (view.nonEmpty) { focusInput = false; current = 0 }
                  } else current = math.min(view.size - 1, current + 1)
                case _ => ()
              }
            }
          case 10 | 13 => // Enter (LF / CR): descend into, or pick, the match
            if (view.nonEmpty) descend(view(current)) match {
              case Some(path) => // not a leaf: drill in and keep browsing
                query = path; focusInput = true; current = 0
              case None => chosen = current
            }
          case 127 | 8 => // Backspace / Delete
            if (query.nonEmpty) {
              query = query.dropRight(1); focusInput = true; current = 0
            }
          case 23 => // Ctrl-W: delete the path segment / word before the cursor
            if (query.nonEmpty) {
              var i = query.length
              while (i > 0 && isSep(query.charAt(i - 1)))
                i -= 1 // trailing seps
              while (i > 0 && !isSep(query.charAt(i - 1))) i -= 1 // the segment
              query = query.substring(0, i); focusInput = true; current = 0
            }
          case 9 => // Tab: complete the input to the first available option
            if (focusInput)
              view.headOption.flatMap(complete).foreach { p =>
                query = p; current = 0
              }
          case c if c >= 32 && c <= 126 => // printable: extend the query
            query += c.toChar; focusInput = true; current = 0
          case 3 => throw Interrupted // Ctrl-C: abort cleanly
          case _ => ()
        }
        if (chosen < 0) {
          print(s"$esc[${window + 1}A") // cursor back up to the search line
          render()
        }
      }
      view(chosen)
    } finally {
      print(s"$esc[?25h") // show cursor
      Console.flush()
      val _ = system(c"stty sane") // restore terminal
    }
  }

  /** Like [[select]], but lets the user toggle any number of options on/off
    * (Space) before confirming (Enter). Returns the chosen options in their
    * original order; the result may be empty. An empty `options` list confirms
    * immediately with an empty selection.
    */
  def multiSelect[F[_], A](
      options: Seq[A],
      title: String = "",
      show: A => String = (a: A) => a.toString
  )(implicit F: Sync[F]): F[Seq[A]] = F.blocking {
    if (options.isEmpty) Seq.empty[A]
    else {
      val esc      = ""
      val n        = options.size
      var current  = 0
      var selected = Set.empty[Int]

      def render(): Unit = {
        for ((opt, i) <- options.zipWithIndex) {
          val cursor = if (i == current) ">" else " "
          val box    = if (selected.contains(i)) "[x]" else "[ ]"
          print(
            s"$esc[2K$cursor $box ${show(opt)}\r\n"
          ) // clear line, then draw
        }
        Console.flush()
      }

      if (title.nonEmpty) println(title)
      val _ = system(
        c"stty -echo -icanon -isig min 1 time 0"
      ) // raw-ish: no echo, char-at-a-time
      try {
        print(s"$esc[?25l") // hide cursor
        render()
        var done = false
        while (!done) {
          getchar() match {
            case 27 => // ESC: possible arrow-key sequence "ESC [ A/B"
              if (getchar() == 91) {
                getchar() match {
                  case 65 => current = math.max(0, current - 1)     // up
                  case 66 => current = math.min(n - 1, current + 1) // down
                  case _  => ()
                }
              }
            case 'k' => current = math.max(0, current - 1)
            case 'j' => current = math.min(n - 1, current + 1)
            case 32 => // Space: toggle the highlighted option
              if (selected.contains(current)) selected -= current
              else selected += current
            case 10 | 13 => done = true       // Enter: confirm selection
            case 3       => throw Interrupted // Ctrl-C: abort cleanly
            case _       => ()
          }
          if (!done) {
            print(s"$esc[${n}A") // cursor back up to the first option
            render()
          }
        }
        options.zipWithIndex.collect {
          case (opt, i) if selected.contains(i) => opt
        }
      } finally {
        print(s"$esc[?25h") // show cursor
        Console.flush()
        val _ = system(c"stty sane") // restore terminal
      }
    }
  }

  /** Like [[select]], but with a search box that filters a fixed list of
    * options by a case-insensitive substring match on their shown labels.
    */
  def search[F[_], A](
      options: Seq[A],
      title: String = "",
      show: A => String = (a: A) => a.toString,
      matches: (String, String) => Boolean = (q, label) =>
        label.toLowerCase.contains(q.toLowerCase)
  )(implicit F: Sync[F]): F[A] = {
    require(options.nonEmpty, "Menu.search requires at least one option")
    runSearch[F, A](
      candidates = q =>
        if (q.isEmpty) options else options.filter(a => matches(q, show(a))),
      descend = _ => None,
      complete = _ => None,
      title = title,
      show = show,
      window = math.min(options.size, 5),
      initial = ""
    )
  }

  /** A file picker built on [[runSearch]]: the search box holds a path and the
    * first five filesystem entries matching it are listed below. The text up to
    * the last `/` selects the directory to list; the rest filters its entries
    * by name prefix. Pressing Enter on a directory drills into it (the box
    * becomes that directory's path); Enter on a file selects and returns it.
    * Starts from `start` (current directory when empty).
    */
  def searchFiles[F[_]](
      title: String = "",
      start: String = ""
  )(implicit F: Sync[F]): F[File] =
    runSearch[F, File](
      candidates = listPathMatches,
      descend = f => if (f.isDirectory) Some(f.getPath + "/") else None,
      complete = f => Some(f.getPath + (if (f.isDirectory) "/" else "")),
      title = title,
      show = f => f.getName + (if (f.isDirectory) "/" else ""),
      window = 5,
      initial = start
    )

  /** Splits `query` into a directory part (up to the last `/`) and a name
    * prefix, then returns the first five entries of that directory whose names
    * start with the prefix, sorted by name.
    */
  private def listPathMatches(query: String): Seq[File] = {
    val slash = query.lastIndexOf('/')
    val (dirName, prefix) =
      if (slash < 0) (".", query)
      else (query.substring(0, slash + 1), query.substring(slash + 1))
    val dir     = new File(if (dirName.isEmpty) "/" else dirName)
    val entries = Option(dir.listFiles()).getOrElse(Array.empty[File])
    entries
      .filter(_.getName.startsWith(prefix))
      .sortBy(_.getName)
      .take(5)
      .toSeq
  }

  def input[F[_]](prompt: String, default: String = "")(implicit
      F: Sync[F]
  ): F[String] = F.blocking {
    val hint = if (default.nonEmpty) s" [$default]" else ""
    print(s"$prompt$hint: ")
    Console.flush()
    // Read char-at-a-time (with our own echo) so that Ctrl-C arrives as a byte
    // we can act on immediately rather than as a signal swallowed by the runtime.
    val _ = system(c"stty -echo -icanon -isig min 1 time 0")
    try {
      val sb   = new StringBuilder
      var done = false
      while (!done) {
        getchar() match {
          case 3       => throw Interrupted // Ctrl-C: abort cleanly
          case 10 | 13 => done = true       // Enter
          case -1      => done = true       // EOF (Ctrl-D)
          case 127 | 8 => // Backspace
            if (sb.nonEmpty) {
              sb.deleteCharAt(sb.length - 1)
              print("\b \b"); Console.flush()
            }
          case c if c >= 32 && c <= 126 => // printable: echo and keep
            sb.append(c.toChar); print(c.toChar); Console.flush()
          case _ => ()
        }
      }
      println()
      val line = sb.toString
      if (line.isEmpty) default else line
    } finally {
      val _ = system(c"stty sane")
    }
  }
}
