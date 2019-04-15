open Reprocessing;

let xmax = 600;
let ymax = 600;
let g = 0.2;

module Fruit = {
  type t = {
    p: (float, float),
    v: (float, float),
    f: int /* fruit type */
  };
  let initial = () => {
    p: (0.0, 0.0),
    v: (Random.float(6.0) -. 3.0, 8.0 +. Random.float(5.0)),
    f: Random.int(6),
  };

  let tick = ({p: (x, y), v: (vx, vy), f}) =>
    switch (x, y) {
    | (x', y') when y' >= (-10.0) && x' >= (-310.0) && x' <= 310.0 => {
        p: (x' +. vx, y' +. vy),
        v: (vx, vy -. g),
        f,
      }
    | _ => initial()
    };
};

type state = {
  fruitimgs: array(imageT),
  background: imageT,
  score: int,
  fruits: list(Fruit.t),
};

let mousePositions = ref([]);

let gravity = 0.8;
let setup = env => {
  Env.size(~width=600, ~height=600, env);
  let background =
    Draw.loadImage(
      ~filename="../assets/background.png",
      env,
    );
  let fruitimgs = [|
    Draw.loadImage(
      ~filename="../assets/apple.png",
      env,
    ),
    Draw.loadImage(
      ~filename="../assets/banana.png",
      env,
    ),
    Draw.loadImage(
      ~filename="../assets/coconut.png",
      env,
    ),
    Draw.loadImage(
      ~filename="../assets/orange.png",
      env,
    ),
    Draw.loadImage(
      ~filename="../assets/pineapple.png",
      env,
    ),
    Draw.loadImage(
      ~filename="../assets/watermelon.png",
      env,
    ),
  |];
  {
    fruitimgs,
    background,
    score: 0,
    fruits: [Fruit.initial(), Fruit.initial(), Fruit.initial()],
  };
};
let draw = ({fruitimgs, score, fruits, background} as state, env) => {
  let (mouseX, mouseY) = Env.mouse(env);

  Draw.image(background, ~pos=(0, 0), ~width=600, ~height=600, env);

  fruits
  |> List.iter((fruit: Fruit.t) => {
       let (x, y) = fruit.p;

       Draw.image(
         fruitimgs[fruit.f],
         ~pos=(xmax / 2 - int_of_float(x), ymax - int_of_float(y)),
         ~width=40,
         ~height=40,
         env,
       );
     });

  if (Env.mousePressed(env)) {
    mousePositions := [Env.mouse(env), ...mousePositions^];
    Draw.fill(Constants.red, env);
    mousePositions^
    |> List.iter(mousePos =>
         Draw.rect(~pos=mousePos, ~width=5, ~height=5, env)
       );
  } else {
    mousePositions := [];
  };
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  Draw.text(~pos=(10, 20), ~body="Score: " ++ string_of_int(score), env);

  let addToScore = ref(0);
  let newFruits =
    fruits
    |> List.map((fruit: Fruit.t) =>
         if (Env.mousePressed(env)) {
           let (x, y) = fruit.p;
           if (Utils.intersectRectCircle(
                 ~rectPos=(float_of_int(mouseX), float_of_int(mouseY)),
                 ~rectW=1.,
                 ~rectH=1.,
                 ~circlePos=(
                   float_of_int(xmax) /. 2. -. x,
                   float_of_int(ymax) -. y,
                 ),
                 ~circleRad=20.,
               )) {
             addToScore := addToScore^ + 1;
             Fruit.initial();
           } else {
             fruit;
           };
         } else {
           fruit;
         }
       );

  {
    ...state,
    score: score + addToScore^,
    fruits: newFruits |> List.map(Fruit.tick),
  };
};

run(~setup, ~draw, ());
