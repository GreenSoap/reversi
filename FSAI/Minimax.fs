namespace FSAI

module Game =
  // Minimax algo with alpha-beta pruning to disregard inconsequential parts of the tree
  let rec minimax (board: byte[,]) (tile: byte) (depth: int) (alpha: int) (beta: int) (isMaximizingPlayer: bool) =
      // Return static evaluation if the leaf has no more children, or the game is over.
      if depth = 0 || GameLogic.getWinner(board) <> Tile.Empty then
        GameLogic.getBoardEvaluation(board)
      else
        let validMoves = GameLogic.getValidMoves board tile
        let minmaxEval = if isMaximizingPlayer then System.Int32.MinValue else System.Int32.MaxValue

        if Seq.length validMoves = 0 then
          minimax board (GameLogic.getOpposingTile tile) (depth) alpha beta (not isMaximizingPlayer)
        else      
        
          let rec getBestScoreFromValidMoves (board: byte[,]) (validMoveAmount: int) (tile: byte) (alpha: int) (beta: int) (isMaximizingPlayer: bool) =            
            let childBoard = Array2D.copy board

            if isMaximizingPlayer then
              //makeMove to the new board here
              let eval = minimax childBoard (GameLogic.getOpposingTile tile) (depth-1) alpha beta false
              let maxEval = max minmaxEval eval
              let alpha = max minmaxEval alpha

              if (beta <= alpha) then
                maxEval
              else                
                getBestScoreFromValidMoves board (validMoveAmount-1) tile alpha beta false
            else
              //makeMove to the new board here
              let eval = minimax childBoard (GameLogic.getOpposingTile tile) (depth-1) alpha beta true
              let minEval = min minmaxEval eval
              let beta = min minmaxEval beta

              if (beta <= alpha) then
                minEval
              else                
                getBestScoreFromValidMoves board (validMoveAmount-1) tile alpha beta true

          getBestScoreFromValidMoves board (Seq.length validMoves) tile alpha beta isMaximizingPlayer