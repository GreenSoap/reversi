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
  
  /// <summary>
  /// Return true or false whether or not a move have been found in the direction from a tile
  /// </summary>
  let rec foundMoveInDirection x y tile (board: byte[,]) direction =
    // If out of bounds or tile is empty then no move found
    if not (isCoordinateInBounds (x, y)) || board.[x, y] = Tile.Empty then
      false
    // if found own tile then we can make a match by placing on the origin (form where we started)
    else if  board.[x, y] = tile then
      true
    else
      foundMoveInDirection (x + fst direction) (y + snd direction) tile board direction

  /// <summary>
  /// Misleading function name. Loops through all direcitons on current tile and returns the first available move in a list
  /// </summary>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="board"></param>
  /// <param name="tile"></param>
  /// <param name="directions"></param>
  let rec getMovesFromPosition x y (board: byte[,]) tile directions =
    // If already checked through all directions then there are no more moves to return
    if directions = List.Empty then
      []
    else
      // Could've match with empty List here not too late to commit now
      match directions with
      | head::tail ->
        // Take first step towards direction
        let dirX = x + fst head // head -> x (direction)
        let dirY = y + snd head // tail -> y (direction)
        // If stepped on opposing tile then we want to continue to look for potential move
        // Else we have not stepped on sanything and thus cannot do much (no tiles to conqur)
        if (isCoordinateInBounds (dirX, dirY) && board.[dirX, dirY] = getOpposingTile tile) then
          // Check if a move is possible in the current direction
          let found = foundMoveInDirection dirX dirY tile board head
          if found then // If move found then append the current tile position to list of moves
            (x, y)::getMovesFromPosition x y board tile tail
          else // else just continue
            getMovesFromPosition x y board tile tail
        else
          getMovesFromPosition x y board tile tail

  /// <summary>
  /// Loops through the board and return all valid moves found for the given player
  /// </summary>
  /// <param name="board"></param>
  /// <param name="tile"></param>
  let getValidMoves (board: byte[,]) (tile: byte) = 
    // Not optimal
    // Thinking of converting to 1D coordinates and looping through that
    // then appending to list like usual x::func
    let mutable validMoves = []
    // Loop through the board
    for x in 0..boardLength-1 do
      for y in 0..boardLength-1 do
        // If current position on board is Empty then evaluate it for a move
        if (board.[x, y] = Tile.Empty) then
          let moves = getMovesFromPosition x y board tile directions
          // Merge the moves retrieved from the current location with the all moves found so far
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

  /// <summary>
  /// 
  /// </summary>
  /// <param name="board"></param>
  /// <param name="move"></param>
  /// <param name="tile"></param>
  let getFlippedPieces (board: byte[,]) (tile: byte) (move: (int*int)) =
    // destructure the move made
    let moveX, moveY = move
    if board.[moveX, moveY] <> Tile.Empty then []
    else 
      // Function for looping through all directions one by one and merging results
      let rec getFlippedPiecesByDir (board: byte[,]) (tile: byte) (x: int, y: int) (directions: (int*int) list) =
        if directions = List.Empty then
          []
        else 
          // Make step towrds direction
          let x = x + fst directions.Head
          let y = y + snd directions.Head

          // If stepped on opponent then that means potential for flipped pieces, so continue moving in that direction
          if isCoordinateInBounds(x, y) && board.[x, y] = getOpposingTile tile then        
            // Go through each tile in direction and append the position
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