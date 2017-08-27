package com.vlthr.levee.source

import com.vlthr.levee.util.Util

import scala.collection.mutable.ArrayBuffer
import Positions._

trait AbstractFile {
  def name: String
  def path: String
  def exists: Boolean
  def content: String
}

case class InMemoryFile(content: String) extends AbstractFile {
  def name = "In-memory file"
  def path = "In-memory path"
  def exists = false
}

case class TemplateFile(path: String) extends AbstractFile {
  def name = ???
  def exists = true
  def content = Util.readWholeFile(path)
}

object SourcePosition {
  def fromLine(sourceFile: SourceFile,
               line: Int,
               charPositionInLine: Int,
               length: Int) = {
    val start = sourceFile.lineToOffset(line) + charPositionInLine
    val stop = start + length
    SourcePosition.at(sourceFile, start, stop)
  }
  def at(file: SourceFile, start: Int, end: Int) =
    file.atPos(Positions.Position(start, end))
}

/** A source position is comprised of a position in a source file */
case class SourcePosition(source: SourceFile,
                          pos: Position,
                          outer: SourcePosition = NoSourcePosition) {

  /** Is `that` a source position contained in this source position ?
    *  `outer` is not taken into account. */
  def contains(that: SourcePosition): Boolean =
    this.source == that.source && this.pos.contains(that.pos)

  def exists = pos.exists

  def lineContent: String = source.lineContent(point)

  def point: Int = pos.point

  /** The line of the position, starting at 0 */
  def line: Int = source.offsetToLine(point)

  /** Extracts the lines from the underlying source file as `Array[Char]`*/
  def linesSlice: Array[Char] =
    source.content.slice(source.startOfLine(start), source.nextLine(end))

  /** The lines of the position */
  def lines: List[Int] =
    List.range(source.offsetToLine(start), source.offsetToLine(end + 1)) match {
      case Nil => line :: Nil
      case xs => xs
    }

  def lineOffsets: List[Int] =
    lines.map(source.lineToOffset(_))

  def beforeAndAfterPoint: (List[Int], List[Int]) =
    lineOffsets.partition(_ <= point)

  /** The column of the position, starting at 0 */
  def column: Int = source.column(point)

  def start: Int = pos.start
  def startLine: Int = source.offsetToLine(start)
  def startColumn: Int = source.column(start)
  def startColumnPadding: String = source.startColumnPadding(start)

  def end: Int = pos.end
  def endLine: Int = source.offsetToLine(end)
  def endColumn: Int = source.column(end)

  def withOuter(outer: SourcePosition) = new SourcePosition(source, pos, outer)

  def display: String = source.content.slice(start, end).mkString
  def report: String = {
    val previous =
      if (line > 0) source.lineContent(source.lineToOffset(line - 1))
      else ""
    val thisLine = source.lineContent(start)
    val next = source.lineContent(source.lineToOffset(line + 1))
    val highlight = " " * (start - source.startOfLine(start)) + "^" * Math.max(
      end - start,
      1) + "\n"
    s"$previous$thisLine$highlight$next".trim
  }

  override def toString =
    if (source.exists) s"${source.file}:${line + 1}"
    else s"(no source file, offset = ${pos.point})"
}

/** A sentinel for a non-existing source position */
object NoSourcePosition extends SourcePosition(NoSource, NoPosition) {
  override def toString = "?"
  override def withOuter(outer: SourcePosition) = outer
}

case class SourceFile(file: AbstractFile, content: Array[Char]) {
  import Positions._
  final val LF = '\u000A'
  final val FF = '\u000C'
  final val CR = '\u000D'
  final val SU = '\u001A'

  /** Tab increment; can be overridden */
  def tabInc = 8

  def name = file.name
  def path = file.path

  override def equals(that: Any) = that match {
    case that: SourceFile => file.path == that.file.path && start == that.start
    case _ => false
  }
  override def hashCode = file.path.## + start.##

  def apply(idx: Int) = content.apply(idx)

  val length = content.length

  /** true for all source files except `NoSource` */
  def exists: Boolean = true

  /** The underlying source file */
  def underlying: SourceFile = this

  /** The start of this file in the underlying source file */
  def start = 0

  def atPos(pos: Position): SourcePosition =
    if (pos.exists) SourcePosition(underlying, pos)
    else NoSourcePosition

  def isSelfContained = underlying eq this

  /** Map a position to a position in the underlying source file.
    *  For regular source files, simply return the argument.
    */
  def positionInUltimateSource(position: SourcePosition): SourcePosition =
    SourcePosition(underlying, position.pos shift start)

  /** Is character a line break? */
  def isLineBreakChar(c: Char) = c match {
    case LF | FF | CR | SU => true
    case _ => false
  }

  private def isLineBreak(idx: Int) =
    if (idx >= length) false
    else {
      val ch = content(idx)
      // don't identify the CR in CR LF as a line break, since LF will do.
      if (ch == CR) (idx + 1 == length) || (content(idx + 1) != LF)
      else isLineBreakChar(ch)
    }

  private def calculateLineIndices(cs: Array[Char]) = {
    val buf = new ArrayBuffer[Int]
    buf += 0
    for (i <- 0 until cs.length) if (isLineBreak(i)) buf += i + 1
    buf += cs.length // sentinel, so that findLine below works smoother
    buf.toArray
  }
  private lazy val lineIndices: Array[Int] = calculateLineIndices(content)

  /** Map line to offset of first character in line */
  def lineToOffset(index: Int): Int = lineIndices(index)

  /** A cache to speed up offsetToLine searches to similar lines */
  private var lastLine = 0

  /** Convert offset to line in this source file
    *  Lines are numbered from 0
    */
  def offsetToLine(offset: Int): Int = {
    lastLine = bestFit(lineIndices, lineIndices.length, offset, lastLine)
    if (offset >= length) lastLine -= 1 // compensate for the sentinel
    lastLine
  }

  /** The index of the first character of the line containing position `offset` */
  def startOfLine(offset: Int): Int = {
    require(offset >= 0)
    lineToOffset(offsetToLine(offset))
  }

  /** The start index of the line following the one containing position `offset` */
  def nextLine(offset: Int): Int =
    lineToOffset(offsetToLine(offset) + 1 min lineIndices.length - 1)

  /** The content of the line containing position `offset` */
  def lineContent(offset: Int): String =
    content.slice(startOfLine(offset), nextLine(offset)).mkString

  /** The column corresponding to `offset`, starting at 0 */
  def column(offset: Int): Int = {
    var idx = startOfLine(offset)
    var col = 0
    while (idx != offset) {
      col += (if (idx < length && content(idx) == '\t') (tabInc - col) % tabInc
              else 1)
      idx += 1
    }
    col
  }

  /** The padding of the column corresponding to `offset`, includes tabs */
  def startColumnPadding(offset: Int): String = {
    var idx = startOfLine(offset)
    val pad = new StringBuilder
    while (idx != offset) {
      pad.append(if (idx < length && content(idx) == '\t') '\t' else ' ')
      idx += 1
    }
    pad.result()
  }

  /** The index `i` in `candidates.indices` such that `candidates(i) <= x` and
    *  `candidates(i)` is closest to `x`, determined by binary search, or -1
    *  if `x < candidates(0)`.
    *  @param  hint   If between 0 and `candidates.length` use this
    *                 as the first search point, otherwise use
    *                 `candidates.length/2`.
    *  @pre   candidates is sorted
    */
  def bestFit(candidates: Array[Int],
              length: Int,
              x: Int,
              hint: Int = -1): Int = {
    def recur(lo: Int, hi: Int, mid: Int): Int =
      if (x < candidates(mid))
        recur(lo, mid - 1, (lo + mid - 1) / 2)
      else if (mid + 1 < length && x >= candidates(mid + 1))
        recur(mid + 1, hi, (mid + 1 + hi) / 2)
      else mid
    val initMid = if (0 <= hint && hint < length) hint else length / 2
    if (length == 0 || x < candidates(0)) -1
    else recur(0, length, initMid)
  }

  override def toString = file.toString
}

object SourceFile {
  def fromFile(file: AbstractFile): SourceFile =
    SourceFile(file, file.content.toArray)
  def fromFile(path: String): SourceFile = fromFile(TemplateFile(path))
}

object NoSource extends SourceFile(InMemoryFile(""), Array()) {
  override def exists = false
  override def atPos(pos: Position): SourcePosition = NoSourcePosition
}

/** Position format in little endian:
  *  Start: unsigned 26 Bits (works for source files up to 64M)
  *  End: unsigned 26 Bits
  *  Point: unsigned 12 Bits relative to start
  *  NoPosition encoded as -1L (this is a normally invalid position
  *  because point would lie beyond end.
  */
object Positions {
  private val StartEndBits = 26
  val StartEndMask: Long = (1L << StartEndBits) - 1
  private val SyntheticPointDelta = (1 << (64 - StartEndBits * 2)) - 1

  /** The maximal representable offset in a position */
  val MaxOffset = StartEndMask

  /** Convert offset `x` to an integer by sign extending the original
    *  field of `StartEndBits` width.
    */
  def offsetToInt(x: Int) =
    x << (32 - StartEndBits) >> (32 - StartEndBits)

  /** A position indicates a range between a start offset and an end offset.
    *  Positions can be synthetic or source-derived. A source-derived position
    *  has in addition a point lies somewhere between start and end. The point
    *  is roughly where the ^ would go if an error was diagnosed at that position.
    *  All quantities are encoded opaquely in a Long.
    */
  class Position(val coords: Long) extends AnyVal {

    /** Is this position different from NoPosition? */
    def exists = this != NoPosition

    /** The start of this position. */
    def start: Int = {
      assert(exists)
      (coords & StartEndMask).toInt
    }

    /** The end of this position */
    def end: Int = {
      assert(exists)
      ((coords >>> StartEndBits) & StartEndMask).toInt
    }

    /** The point of this position, returns start for synthetic positions */
    def point: Int = {
      assert(exists)
      val poff = pointDelta
      if (poff == SyntheticPointDelta) start else start + poff
    }

    /** The difference between point and start in this position */
    def pointDelta =
      (coords >>> (StartEndBits * 2)).toInt

    def orElse(that: Position) =
      if (this.exists) this else that

    /** The union of two positions. This is the least range that encloses
      *  both positions. It is always a synthetic position.
      */
    def union(that: Position) =
      if (!this.exists) that
      else if (!that.exists) this
      else
        Position(this.start min that.start, this.end max that.end, this.point)

    /** Does the range of this position contain the one of that position? */
    def contains(that: Position): Boolean =
      !that.exists || exists && (start <= that.start && end >= that.end)

    /** Is this position synthetic? */
    def isSynthetic = pointDelta == SyntheticPointDelta

    /** Is this position source-derived? */
    def isSourceDerived = !isSynthetic

    /** Is this a zero-extent position? */
    def isZeroExtent = start == end

    /** A position where all components are shifted by a given `offset`
      *  relative to this position.
      */
    def shift(offset: Int) =
      if (exists) fromOffsets(start + offset, end + offset, pointDelta)
      else this

    /** The zero-extent position with start and end at the point of this position */
    def focus = if (exists) Position(point) else NoPosition

    /** The zero-extent position with start and end at the start of this position */
    def startPos = if (exists) Position(start) else NoPosition

    /** The zero-extent position with start and end at the end of this position */
    def endPos = if (exists) Position(end) else NoPosition

    /** A copy of this position with a different start */
    def withStart(start: Int) =
      fromOffsets(start,
                  this.end,
                  if (isSynthetic) SyntheticPointDelta else this.point - start)

    /** A copy of this position with a different end */
    def withEnd(end: Int) = fromOffsets(this.start, end, pointDelta)

    /** A copy of this position with a different point */
    def withPoint(point: Int) =
      fromOffsets(this.start, this.end, point - this.start)

    /** A synthetic copy of this position */
    def toSynthetic = if (isSynthetic) this else Position(start, end)

    override def toString = {
      val (left, right) = if (isSynthetic) ("<", ">") else ("[", "]")
      if (exists)
        s"$left$start..${if (point == start) "" else s"$point.."}$end$right"
      else
        s"${left}no position${right}"
    }
  }

  private def fromOffsets(start: Int, end: Int, pointDelta: Int) = {
    //assert(start <= end || start == 1 && end == 0, s"$start..$end")
    new Position(
      (start & StartEndMask).toLong |
        ((end & StartEndMask).toLong << StartEndBits) |
        (pointDelta.toLong << (StartEndBits * 2)))
  }

  /** A synthetic position with given start and end */
  def Position(start: Int, end: Int): Position = {
    val pos = fromOffsets(start, end, SyntheticPointDelta)
    assert(pos.isSynthetic)
    pos
  }

  /** A source-derived position with given start, end, and point delta */
  def Position(start: Int, end: Int, point: Int): Position = {
    val pointDelta = (point - start) max 0
    val pos = fromOffsets(
      start,
      end,
      if (pointDelta >= SyntheticPointDelta) 0 else pointDelta)
    assert(pos.isSourceDerived)
    pos
  }

  /** A synthetic zero-extent position that starts and ends at given `start`. */
  def Position(start: Int): Position = Position(start, start)

  /** A sentinel for a non-existing position */
  val NoPosition = Position(1, 0)

  /** The coordinate of a symbol. This is either an index or
    *  a zero-range position.
    */
  class Coord(val encoding: Int) extends AnyVal {
    def isIndex = encoding > 0
    def isPosition = encoding <= 0
    def toIndex: Int = {
      assert(isIndex)
      encoding - 1
    }
    def toPosition = {
      assert(isPosition)
      if (this == NoCoord) NoPosition else Position(-1 - encoding)
    }
  }

  /** An index coordinate */
  implicit def indexCoord(n: Int): Coord = new Coord(n + 1)
  implicit def positionCoord(pos: Position): Coord =
    if (pos.exists) new Coord(-(pos.point + 1))
    else NoCoord

  /** A sentinel for a missing coordinate */
  val NoCoord = new Coord(0)

}
