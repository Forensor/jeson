# jeson
Jeson Mor game engine. Pure &amp; free from side-effects.

## What is Jeson Mor?

<p align="center">
  <img src="https://github.com/Forensor/jeson/blob/master/img/board.png" />
</p>

Jeson Mor (Nine Horses) is a two-player strategy board game from Mongolia played on a 9×9 checkered gameboard. Each player has nine chess knights initially lined up on the first ranks. A player wins by being first to occupy the central square (red circle above) with a knight, and then leave that square. Knights move as in normal chess (see below).

<p align="center">
  <img src="https://github.com/Forensor/jeson/blob/master/img/movement.png" />
</p>

## Docs

fen -> Returns the fen of the current position<br>
gameOver -> Checks if the game has ended<br>
get -> Gets the piece located in the indicated square<br>
load -> Loads a fen position<br>
move -> Moves a piece of the board. It has to be valid<br>
validMoves -> Returns a list containing all the possible legal moves<br>
pgn -> Returns the pgn<br>
put -> Places a piece in the indicated square<br>
remove -> Removes a piece of the indicated square<br>
newGame -> Creates a game, setting it to the starting position<br>
takeback -> Takebacks the last move
