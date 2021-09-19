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

  // Returns whether or not the coordinate is within the boundries of the board
  let isCoordinateInBounds (x: int, y: int) = (0 <= x && x < boardLength && 0 <= y && y < boardLength)

  let getOpposingTile (tile: byte) =
    if (tile <> Tile.White && tile <> Tile.Black) then error
    elif tile = Tile.White then Tile.Black else Tile.White      
  
  let rec foundMoveInDirection x y tile (board: byte[,]) direction =
    if not (isCoordinateInBounds (x, y)) || board.[x, y] = Tile.Empty then
      false
    else if  board.[x, y] = tile then
      true
    else
      foundMoveInDirection (x + fst direction) (y + snd direction) tile board direction

  let rec getMovesFromPosition x y (board: byte[,]) tile directions =
    if directions = List.Empty then
      []
    else
      match directions with
      | head::tail ->
        let dirX = x + fst head
        let dirY = y + snd head
        if (isCoordinateInBounds (dirX, dirY) && board.[dirX, dirY] = getOpposingTile tile) then
          let found = foundMoveInDirection dirX dirY tile board head
          if found then
            (x, y)::getMovesFromPosition x y board tile tail
          else 
            getMovesFromPosition x y board tile tail
        else
          getMovesFromPosition x y board tile tail


  let getValidMoves (board: byte[,]) (tile: byte) = 
    let mutable validMoves = []
    // Loop through the board
    for x in 0..boardLength-1 do
      for y in 0..boardLength-1 do
        // If current position on board is Empty then evaluate it for a move
        if (board.[x, y] = Tile.Empty) then
          let moves = getMovesFromPosition x y board tile directions
          validMoves <- moves @ validMoves
    validMoves

  // Returns the amount of tiles in the board corresponding to the passed tile
  let getScore (board: byte[,]) (tile: byte) =
    Seq.length((Seq.filter(fun cell -> cell = tile) (Seq.cast board)))
  
  let isBoardFull blackScore whiteScore = (blackScore + whiteScore = (boardLength * boardLength))

  let getWinner (board: byte[,]) = 
    let blackScore = getScore board Tile.Black
    let whiteScore = getScore board Tile.White

    if isBoardFull blackScore whiteScore || blackScore = 0 || whiteScore = 0 ||
      ((getValidMoves board Tile.White).Length + (getValidMoves board Tile.Black).Length = 0) then

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

  let getFlippedPieces (board: byte[,]) (tile: byte) (move: (int*int)) =
    let moveX, moveY = move
    if board.[moveX, moveY] <> Tile.Empty then []
    else 
      let rec getFlippedPiecesByDir (board: byte[,]) (tile: byte) (x: int, y: int) (directions: (int*int) list) =
        if directions = List.Empty then
          []
        else 
          let x = x + fst directions.Head
          let y = y + snd directions.Head

          if isCoordinateInBounds(x, y) && board.[x, y] = getOpposingTile tile then        
            let rec getDirFlippedPiecesArray (board: byte[,]) (tile: byte) (position: (int*int)) (direction: (int*int)) =
              let posX, posY = position
              let dirX, dirY = direction
              if isCoordinateInBounds(posX, posY) && board.[posX, posY] = getOpposingTile tile && board.[posX, posY] <> tile then
                getDirFlippedPiecesArray board tile (posX+dirX, posY+dirY) direction
              else []
            
            getDirFlippedPiecesArray board tile (x, y) directions.Head
          else
            getFlippedPiecesByDir board tile (x, y) directions.Tail
  
      getFlippedPiecesByDir board tile (moveX, moveY) directions
          
  let makeMove (board: byte[,]) (move: (int*int)) (tile: byte) =
    let flippedPieces = getFlippedPieces board tile move
    let boardCopy = Array2D.copy board
    for flippedPiece in flippedPieces do
      boardCopy.[fst flippedPiece, snd flippedPiece] <- tile

    if flippedPieces.Length > 0 then
      boardCopy.[fst move, snd move] <- tile
      boardCopy
    else boardCopy