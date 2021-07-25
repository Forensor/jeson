package forensor.jeson

import Team.Team

object Jeson {
  type Square = Option[Team]
  type Row = Seq[Square]
  type Board = Seq[Row]

  val DIRS: List[List[Int]] = List(
    List(-2, -1),
    List(-2, +1),
    List(-1, -2),
    List(-1, +2),
    List(+1, -2),
    List(+1, +2),
    List(+2, -1),
    List(+2, +1)
  )

  val COORDS: Seq[Seq[String]] = Seq(
    Seq("a9", "b9", "c9", "d9", "e9", "f9" , "g9", "h9", "i9"),
    Seq("a8", "b8", "c8", "d8", "e8", "f8" , "g8", "h8", "i8"),
    Seq("a7", "b7", "c7", "d7", "e7", "f7" , "g7", "h7", "i7"),
    Seq("a6", "b6", "c6", "d6", "e6", "f6" , "g6", "h6", "i6"),
    Seq("a5", "b5", "c5", "d5", "e5", "f5" , "g5", "h5", "i5"),
    Seq("a4", "b4", "c4", "d4", "e4", "f4" , "g4", "h4", "i4"),
    Seq("a3", "b3", "c3", "d3", "e3", "f3" , "g3", "h3", "i3"),
    Seq("a2", "b2", "c2", "d2", "e2", "f2" , "g2", "h2", "i2"),
    Seq("a1", "b1", "c1", "d1", "e1", "f1" , "g1", "h1", "i1")
  )

  val INITIAL_FEN: String = "nnnnnnnnn/9/9/9/9/9/9/9/NNNNNNNNN w 0 1"

  def fen(game: Game): String = game.positions.last

  def gameOver(game: Game): Boolean = !hasPieces(game) || hasLeftCenter(game) || fiftyMoveRule(game)

  def get(game: Game, coord: String): Square = game.board(gi(coord).head)(gi(coord).last)

  def load(fen: String): Game = {
    val fields: Seq[String] = fen.split(" ")
    Game(
      transToBoard(simplerPos(fields.head)),
      List(),
      transToHalfMoves(fields(2)),
      List(fen),
      transToTeam(fields(1))
    )
  }

  def move(game: Game, san: String): Game = {
    val origCoords: String = san.substring(0, 2)
    val destCoords: String = san.takeRight(2)
    val movedBoardGame: Game = remove(put(game, destCoords, get(game, origCoords)), origCoords)
    val unlessFenAddedGame: Game = movedBoardGame.copy(
      turn = opTeam(movedBoardGame.turn),
      history = movedBoardGame.history :+ san,
      hmoves = {
        if (san.contains('x')) 0 else movedBoardGame.hmoves + 1
      }
    )
    val newFen: String = complexPos(transToPos(unlessFenAddedGame.board)) +
      { if (unlessFenAddedGame.turn == Team.White) " b" else " w" } + " " +
      unlessFenAddedGame.hmoves + " " +
      (unlessFenAddedGame.history.length / 2 + 1).toString
    unlessFenAddedGame.copy(positions = unlessFenAddedGame.positions :+ newFen)
  }

  def newGame(): Game = load(INITIAL_FEN)

  def put(game: Game, coord: String, square: Square): Game = {
    // Replaced row
    val rr: Row = game.board(gi(coord).head).updated(gi(coord).last, square)
    game.copy(board = game.board.updated(gi(coord).head, rr))
  }

  def remove(game: Game, coord: String): Game = put(game, coord, None)

  def takeback(game: Game): Game = {
    if (game.history.length <= 0) {
      game
    } else {
      game.copy(
        board = transToBoard(simplerPos(game.positions.reverse(1).split(" ").head)),
        history = game.history.init,
        hmoves = game.positions.reverse(1).split(" ")(2).toInt,
        positions = game.positions.init,
        turn = opTeam(game.turn)
      )
    }
  }

  def pgn(game: Game): String = {
    game.history.zipWithIndex.map{ case (san, i) =>
      if (i % 2 == 0) {
        s"${i / 2 + 1}. $san "
      } else {
        s"$san "
      }
    }.mkString("").trim
  }

  def validMoves(game: Game): Seq[String] = {
    COORDS.flatMap(r => r.flatMap(c => getMovesOfSquare(game, gi(c).head, gi(c).last)))
  }

  // getIndices
  private def gi(coord: String): Seq[Int] = {
    val row: Int = COORDS.indexOf(COORDS.find(r => r.contains(coord)).getOrElse(-1))
    val col: Int = COORDS(row).indexOf(coord)
    Seq(row, col)
  }

  private def inRange(n: Int): Boolean = n >= 0 && n <= 8

  private def repeatChar(c: Char, n: Int): String = {
    n match {
      case 0 => ""
      case _ => c + repeatChar(c, n - 1)
    }
  }

  private def hasLeftCenter(game: Game): Boolean = {
    game.history.nonEmpty && game.history.last.substring(0, 2) == "e5"
  }

  private def hasPieces(game: Game): Boolean = game.board.flatMap(_.toList).contains(Some(game.turn))

  private def opTeam(team: Team): Team = if (team == Team.White) Team.Black else Team.White

  private def simplerPos(fen: String): String = {
    if (fen.nonEmpty) {
      if (fen.head.isDigit) {
        repeatChar('x', fen.head.asDigit) + simplerPos(fen.tail)
      } else {
        fen.head + simplerPos(fen.tail)
      }
    } else {
      ""
    }
  }

  private def fiftyMoveRule(game: Game): Boolean = game.hmoves >= 50

  private def correctTurnSquare(turn: Team, square: Square): Boolean = {
    if (square.contains(turn)) true
    else false
  }

  private def complexPos(fen: String, n: Int = 0): String = {
    if (fen.nonEmpty) {
      if (fen.head == 'x') {
        complexPos(fen.tail, n + 1)
      } else {
        if (n > 0) {
          n + complexPos(fen)
        } else {
          fen.head + complexPos(fen.tail)
        }
      }
    } else {
      ""
    }
  }

  private def getMoveSan(game: Game, or: Int, oc: Int, dr: Int, dc: Int): String = {
    if (inRange(or) && inRange(oc) && inRange(dr) && inRange(dc)) {
      if (correctTurnSquare(game.turn, game.board(or)(oc))) {
        if (game.board(dr)(dc).contains(opTeam(game.turn))) {
          return s"${COORDS(or)(oc)}x${COORDS(dr)(dc)}"
        }
        if (game.board(dr)(dc).isEmpty) {
          return s"${COORDS(or)(oc)}${COORDS(dr)(dc)}"
        }
      }
    }
    ""
  }

  private def getMovesOfSquare(game: Game, or: Int, oc: Int): Seq[String] = {
    DIRS.map(d => getMoveSan(game, or, oc, or + d.head, oc + d.last)).filterNot(m => m == "")
  }

  private def transToBoard(pos: String): Board = pos.split("/").map(r => transToRow(r))

  private def transToPos(board: Board): String = {
    board.map(r => transToPartialPos(r)).mkString("").substring(1)
  }

  private def transToPartialPos(row: Row): String = "/" + row.map(s => transToC(s)).mkString("")

  private def transToC(square: Square): Char = {
    square match {
      case Some(Team.Black) => 'n'
      case Some(Team.White) => 'N'
      case _ => 'x'
    }
  }

  private def transToHalfMoves(hmoves: String): Int = if (hmoves.forall(_.isDigit)) hmoves.toInt else 0

  private def transToRow(row: String): Row = row.map(c => transToSquare(c))

  private def transToSquare(c: Char): Square = {
    c match {
      case 'n' => Some(Team.Black)
      case 'N' => Some(Team.White)
      case _ => None
    }
  }

  private def transToTeam(team: String): Team = if (team == "b") Team.Black else Team.White
}
