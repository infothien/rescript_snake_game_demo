open Reprocessing

type state = {food: (int, int)}

let setup = env => {
  Env.size(~width=600, ~height=600, env)

  {food: (300, 160)}
}

let draw = (state, env) => {
  Draw.background(Constants.green, env)
  Draw.fill(Constants.red, env)
  Draw.rect(~pos=state.food, ~width=20, ~height=20, env)

  Draw.text(~pos=(10, 560), ~body="Score", env)
  state
}

let keyPressed = (state, env) => {
  switch Env.keyCode(env) {
  | Down => {
      let (x, y) = state.food
      {food: (x, y + 20)}
    }
  | _ => state
  }
}

run(~setup, ~draw, ~keyPressed, ())
