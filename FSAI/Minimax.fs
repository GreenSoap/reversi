namespace FSAI

module Game =
  // Minimax algo w/ alpha-beta pruning to disregard inconsequential parts of the tree
  let rec minimax (depth: int, alpha: int, beta: int, isMaximizingPlayer: bool, board: byte[,], tile: byte) =
      // If the last leaf has no more children, or the game is over
      if depth = 0 || (GameLogic.getWinner(board) <> Tile.Empty) then
        //TODO Eval
        0.0
      else
        if isMaximizingPlayer then
          let maxEval = -infinity

          maxEval
        else
          let maxEval = +infinity
          maxEval