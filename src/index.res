open Reprocessing

type direction = Up | Down | Left | Right

type collision = Boundary | Tail | Food

type state = {
  food: (int, int),
  snake: array<(int, int)>,
  lastMovedAt: float,
  direction: direction,
  score: int,
}

let detectCollision = ({snake, food}) => {
  let snakeLength = snake->Js.Array2.length

  let (headX, headY) as head = snake[snakeLength - 1]

  let tail = snake->Js.Array2.slice(~start=0, ~end_=snakeLength - 2)

  if head == food {
    Some(Food)
  } else if headX < 0 || headX > 580 || headY < 0 || headY > 580 {
    Some(Boundary)
  } else if tail->Js.Array2.some(block => block == head) {
    Some(Tail)
  } else {
    None
  }
}

let setup = env => {
  Env.size(~width=600, ~height=600, env)

  {
    food: (300, 160),
    snake: [(20, 20), (40, 20), (60, 20), (80, 20), (100, 20)],
    lastMovedAt: Js.Date.now(),
    direction: Right,
    score: 0,
  }
}

let rec randomFood = ({snake} as state) => {
  let newFood = (
    Js.Math.random_int(0, 29) * Config.blockSize,
    Js.Math.random_int(0, 29) * Config.blockSize,
  )

  if snake->Js.Array2.some(block => block == newFood) {
    randomFood(state)
  } else {
    newFood
  }
}

let drawBlock = (~color, ~position, ~env) => {
  Draw.fill(color, env)
  Draw.rect(~pos=position, ~width=Config.blockSize, ~height=Config.blockSize, env)
}

let moveSnake = ({snake, lastMovedAt, direction, score} as state) => {
  let (headX, headY) = snake[snake->Js.Array2.length - 1]

  if Js.Date.now() -. lastMovedAt > Config.gameSpeed {
    let newHead = switch direction {
    | Up => (headX, headY - Config.blockSize)
    | Down => (headX, headY + Config.blockSize)
    | Left => (headX - Config.blockSize, headY)
    | Right => (headX + Config.blockSize, headY)
    }

    switch detectCollision(state) {
    | Some(Food) => {
        snake->Js.Array2.push(newHead)->ignore

        {
          ...state,
          score: score + 1,
          food: randomFood(state),
          lastMovedAt: Js.Date.now(),
        }
      }
    | Some(Boundary)
    | Some(Tail) => state
    | None => {
        snake->Js.Array2.push(newHead)->ignore

        snake->Js.Array2.shift->ignore

        {...state, lastMovedAt: Js.Date.now()}
      }
    }
  } else {
    state
  }
}

let draw = ({snake, food, score} as state, env) => {
  Draw.background(Config.boardColor, env)

  drawBlock(~color=Config.foodColor, ~position=food, ~env)

  snake->Js.Array2.forEach(block => {
    drawBlock(~color=Config.snakeColor, ~position=block, ~env)
  })

  Draw.text(~pos=(10, 560), ~body="Score: " ++ score->Belt.Int.toString, env)

  moveSnake(state)
}

let keyPressed = ({direction: previsouDir} as state, env) => {
  switch Env.keyCode(env) {
  | Up if previsouDir !== Down => {...state, direction: Up}
  | Down if previsouDir !== Up => {...state, direction: Down}
  | Left if previsouDir !== Right => {...state, direction: Left}
  | Right if previsouDir !== Left => {...state, direction: Right}
  | _ => state
  }
}

run(~setup, ~draw, ~keyPressed, ())
