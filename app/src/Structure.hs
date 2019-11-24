module Structure where

data ProgramState = ProgramState
  { time :: Double
  }

stateTime :: ProgramState -> Double
stateTime (ProgramState t) = t

tickTimeForward :: ProgramState -> ProgramState
tickTimeForward (ProgramState t) = ProgramState (t + 0.01)
