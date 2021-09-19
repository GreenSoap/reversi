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

  /// <summary>
  /// Returns whether or not the coordinate is within the boundries of the board
  /// </summary>
  let isCoordinateInBounds (x: int, y: int) = (0 <= x && x < boardLength && 0 <= y && y < boardLength)
 
  /// <summary>
  /// Returns the Tile opposing the passed tile
  /// </summary>
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

  /// <summary>
  /// Returns the amount of tiles in the board corresponding to the passed tile
  /// </summary>
  let getScore (board: byte[,]) (tile: byte) =
    Seq.length((Seq.filter(fun cell -> cell = tile) (Seq.cast board)))
  
  /// <summary>
  /// Returns whether or not the board is full of Tiles based on the players' score.
  /// </summary>
  let isBoardFull blackScore whiteScore = (blackScore + whiteScore = (boardLength * boardLength))

  /// <summary>
  /// Returns the winning Tile, Rule.Tie if it's a tie, or Tile.Empty if the game is not yet over
  /// </summary>
  let getWinner (board: byte[,]) = 
    let blackScore = getScore board Tile.Black
    let whiteScore = getScore board Tile.White

    // If the board is full and one of the players has 0 in score, 
    // or if none of the players have any valid moves left 
    if isBoardFull blackScore whiteScore || blackScore = 0 || whiteScore = 0 ||
      ((getValidMoves board Tile.White).Length + (getValidMoves board Tile.Black).Length = 0) then

      if whiteScore > blackScore then Tile.White
      elif blackScore > whiteScore then Tile.Black
      else Rule.Tie
    else
      Tile.Empty

  /// <summary>
  /// Get the amount of cornered tiles in the board corresponding to the passed tile 
  /// </summary>
  let getNonEmptyCornerTileAmount(board: byte[,]) (tile: byte) =
    (List.filter(fun corner -> corner = tile) [
      board.[0, 0];
      board.[0, boardLength-1];
      board.[boardLength-1, 0];
      board.[boardLength-1, boardLength-1]
    ]).Length

  /// <summary>
  /// Makes an evaluation on how good the board is for the players. The higher the return value, the more advantageous for Tile.White.
  /// </summary>
  let getBoardEvaluation(board: byte[,]) =
    // Get the current scores of the players
    let blackScore = getScore board Tile.Black
    let whiteScore = getScore board Tile.White
    // Get ther movement options (more options = more mobility)
    let blackMobility = Seq.length(getValidMoves board Tile.Black)
    let whiteMobility = Seq.length(getValidMoves board Tile.White)

    // Relative to black player
    if blackScore = 0 then System.Int32.MinValue // If black ahs zero then thats good cause less is more
    elif whiteScore = 0 then System.Int32.MaxValue // If white has zero then (black has more in counter which is bad)
    else
      if isBoardFull blackScore whiteScore || blackMobility + whiteMobility = 0 then
        if (blackScore < whiteScore) then
            (System.Int32.MinValue / 2) - whiteScore + blackScore
        elif (blackScore > whiteScore) then
            (System.Int32.MaxValue / 2) + blackScore - whiteScore
        else
            0
      else
        if blackScore + whiteScore > 55 then
            blackScore - whiteScore
        else
          // Value mobility some amount (the more options the better)
          ((blackScore - whiteScore) + (blackMobility - whiteMobility) * 10) + 
          // Valye corners a lot (relative to black)
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
              if isCoordinateInBounds(posX, posY) && board.[posX, posY] = getOpposingTile tile then
                (posX, posY)::getDirFlippedPiecesArray board tile (posX+dirX, posY+dirY) direction
              else []
            
            let flippedPiecesInDir = getDirFlippedPiecesArray board tile (x, y) directions.Head
            flippedPiecesInDir @ getFlippedPiecesByDir board tile (x, y) directions.Tail
          else
            getFlippedPiecesByDir board tile (x, y) directions.Tail
  
      getFlippedPiecesByDir board tile (moveX, moveY) directions
  
  /// <summary>
  /// Changes the tiles of the flippable pieces to the corresponding tile, as well as the clicked tile.
  /// </summary>
  let makeMove (board: byte[,]) (move: (int*int)) (tile: byte) =
    let flippedPieces = getFlippedPieces board tile move
    let boardCopy = Array2D.copy board

    // Add the flipped pieces to the new board.
    for flippedPiece in flippedPieces do
      boardCopy.[fst flippedPiece, snd flippedPiece] <- tile

    // If there were any flipped pieces, change the tile of the tiled moved to.
    if flippedPieces.Length > 0 then
      boardCopy.[fst move, snd move] <- tile
      boardCopy
    else boardCopy