namespace FSAI

module Rule =
  let Valid = 3uy
  let Tie = 4uy

module Tile = 
  let Empty = 0uy 
  let White = 1uy
  let Black = 2uy

module GameLogic =
  let boardLength = 8
  
  let isValidMove (board: byte[,], tile: byte, x: int, y: int) = 
    true
    
  let getValidMoves (board: byte[,], tile: byte) =
    Seq.map(fun (x, y) -> isValidMove(board, tile, x ,y)) (Seq.cast board)

  // Returns the amount of tiles in the board corresponding to the passed tile
  let getScore (board: byte[,], tile: byte) =
    Seq.length((Seq.filter(fun cell -> cell = tile) (Seq.cast board)))
  
  let getWinner (board : byte[,]) = 
    let blackScore = getScore(board, Tile.Black)
    let whiteScore = getScore(board, Tile.White)

    if (blackScore = 0 || whiteScore = 0) || 
      (blackScore + whiteScore = (boardLength * boardLength)) || 
      (Seq.length(getValidMoves(board, Tile.White)) + Seq.length(getValidMoves(board, Tile.Black)) = 0) then

      if whiteScore > blackScore then Tile.White
      elif blackScore > whiteScore then Tile.Black
      else Rule.Tie
    else
      Tile.Empty