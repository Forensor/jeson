package forensor.jeson

import Jeson.Board
import Team.Team

case class Game(board: Board, history: List[String], hmoves: Int, positions: List[String], turn: Team)
