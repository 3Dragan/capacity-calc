package org.dragan.puzzle

sealed trait Rule[-T <: Choice] {

  def exec: (T, GameState) => GameState
}

case object DevBecomeBusyByTheirTask extends Rule[Task] {
  override def exec: (Task, GameState) => GameState =
    (t, state) =>
      if (state.freeDevs.contains(t.d)) {
        state.copy(devs = state.devs + (t.d -> Busy(t.estimate)))
      } else {
        state
      }
}

case object GraphUpdater extends Rule[Task] {
  override def exec: (Task, GameState) => GameState =
    (t, state) => {
      val newGraph = Move(state.currentGraph).makeMove(t)
      state.copy(currentGraph = newGraph)
    }
}

case object ScoreUpdater extends Rule[Wait] {
  override def exec: (Wait, GameState) => GameState =
    (w, state) => {
      val updatedDevs = state.busyDevInfo.mapValues {
        devState =>
          if (devState.time > w.time) {
            Busy(devState.time - w.time)
          } else {
            Free
          }
      }

      state.copy(score = state.score + w.time, devs = state.devs ++ updatedDevs)
    }
}