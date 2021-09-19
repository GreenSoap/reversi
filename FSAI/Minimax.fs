namespace FSAI

module Game =
  // Minimax algo with alpha-beta pruning to disregard inconsequential parts of the tree
  let rec minimax (board: byte[,]) (tile: byte) (depth: int) (alpha: int) (beta: int) (isMaximizingPlayer: bool) =
      // Return static evaluation if the leaf has no more children, or the game is over.

      // When max depth is reached and Game is not over yet then do general eval on rest of tree 
      if depth = 0 || GameLogic.getWinner board <> Tile.Empty then
        GameLogic.getBoardEvaluation board
      else
        // Get valid moves for current player
        let validMoves = GameLogic.getValidMoves board tile
        // If maximizing player then we want to start from the very bottom moving up or vice versa. score wise
        let minmaxEval = if isMaximizingPlayer then System.Int32.MinValue else System.Int32.MaxValue

        if Seq.length validMoves = 0 then
          minimax board (GameLogic.getOpposingTile tile) depth alpha beta (not isMaximizingPlayer)
        else      
          // Recursively look through childboards (from moves) and evaluate their value
          let rec getBestScoreFromValidMoves (board: byte[,]) (validMoves: (int*int)list) (tile: byte) (alpha: int) (beta: int) (isMaximizingPlayer: bool) =            
            // Get childboard from move (next board state)
            let childBoard = GameLogic.makeMove (Array2D.copy board) validMoves.Head tile


            if isMaximizingPlayer then
              // If max then get highest from child nodes
              let eval = minimax childBoard (GameLogic.getOpposingTile tile) (depth-1) alpha beta false
              let maxEval = max minmaxEval eval
              let alpha = max minmaxEval alpha

              // Exit from going through rest of sister nodes, already found minimum hightest value
              if (beta <= alpha) then
                maxEval
              else
                // Continue to sisters
                getBestScoreFromValidMoves board validMoves.Tail tile alpha beta false
            else // Else get lowest from child nodes
              let eval = minimax childBoard (GameLogic.getOpposingTile tile) (depth-1) alpha beta true
              let minEval = min minmaxEval eval
              let beta = min minmaxEval beta

              // Same as above, no need to check sister nodes because alpha > beta i don't need to continue
              if (beta <= alpha) then
                minEval
              else
                // Continue to sisters
                getBestScoreFromValidMoves board validMoves.Tail tile alpha beta true

          getBestScoreFromValidMoves board validMoves tile alpha beta isMaximizingPlayer