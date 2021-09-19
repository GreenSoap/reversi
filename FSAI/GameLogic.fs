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
  let error = 99uy
  let directions = [ 
    (1, -1)
    (0, 1)
    (1, 1)
    (-1, 0)
    (1, 0)
    (-1, -1)
    (0, -1)
    (1, -1)
  ]

  let getOpposingTile (tile: byte) =
    if (tile <> Tile.White && tile <> Tile.Black) then error
    elif tile = Tile.White then Tile.Black else Tile.White      
    
  let isValidMove (board: byte[,]) (tile: byte) (x: int) (y: int) =
    true
    
  let getValidMoves (board: byte[,]) (tile: byte) = 
    Seq.map(fun (x, y) -> isValidMove board tile x y) (Seq.cast board)

  // Returns the amount of tiles in the board corresponding to the passed tile
  let getScore (board: byte[,]) (tile: byte) =
    Seq.length((Seq.filter(fun cell -> cell = tile) (Seq.cast board)))
  
  let isBoardFull blackScore whiteScore =
    (blackScore + whiteScore = (boardLength * boardLength))

  let getWinner (board: byte[,]) = 
    let blackScore = getScore board Tile.Black
    let whiteScore = getScore board Tile.White

    if isBoardFull blackScore whiteScore || blackScore = 0 || whiteScore = 0 ||
      (Seq.length(getValidMoves board Tile.White) + Seq.length(getValidMoves board Tile.Black) = 0) then

      if whiteScore > blackScore then Tile.White
      elif blackScore > whiteScore then Tile.Black
      else Rule.Tie
    else
      Tile.Empty

  let getNonEmptyCornerTileAmount(board: byte[,]) (tile: byte) =
    (List.filter(fun corner -> corner = tile) [
      board.[0, 0];
      board.[0, boardLength-1];
      board.[boardLength-1, 0];
      board.[boardLength-1, boardLength-1]
    ]).Length

  // Minimax evaluation 
  let getBoardEvaluation(board: byte[,]) =
    let blackScore = getScore board Tile.Black
    let whiteScore = getScore board Tile.White
    let blackMobility = Seq.length(getValidMoves board Tile.Black)
    let whiteMobility = Seq.length(getValidMoves board Tile.White)

    if blackScore = 0 then
        -200000
    elif whiteScore = 0 then
        200000
    else
      if isBoardFull blackScore whiteScore || blackMobility + whiteMobility = 0 then
        if (blackScore < whiteScore) then
            -100000 - whiteScore + blackScore
        elif (blackScore > whiteScore) then
            100000 + blackScore - whiteScore
        else
            0
      else
        if blackScore + whiteScore > 55 then
            blackScore - whiteScore
        else
          ((blackScore - whiteScore) + (blackMobility - whiteMobility) * 10) + 
          (getNonEmptyCornerTileAmount board Tile.Black - getNonEmptyCornerTileAmount board Tile.White) * 100