A takuzu solver written in Haskell.

# TODO
Parse input files to Boards (Matrices)
Define loop skeleton
Implement a checker function


# Design ideas:
Use Data.Matrix for board representation. Each cell is of type Mark.
data Mark = X | O | Empty

// Solver loop idea:
solve(board):
  checkIfConsistent(board); // Todo later.

  loopUntilFixpoint:
    applyTechnique(avoidingTriples1, board);
    applyTechnique(avoidingTriples2, board);
    // add more techniques here as they are implemented

  ((x,y), newBoard) = applyRandomStep(board)
  candidate = solve(newBoard)
  if checkSolution(candidate)
    then candidate
    else solve(applyOppositeOfRandomStep(board, (x,y)))
