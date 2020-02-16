open Revery;
open Revery.UI;
open Revery.UI.Components;

let getPathToAsset = assetName =>
  Environment.getExecutingDirectory() ++ assetName;

module Constants = {
  let gravity = (-500.0);
  let velocityY = 500.;
  let velocityX = 20.0;
  let friction = 0.8;

  let maxVelocityX = 200.0;
  let accelerationX = 100.0;

  // rendering
  let width = 800;
  let floorHeight = 60;
};

let clamp = (~min, ~max, value) => {
  value < min ? min : value > max ? max : value;
};

module Key = {
  type t =
    | Left
    | Right
    | Up
    | Unknown;

  let fromKeyEvent = (keyEvent: NodeEvents.keyEventParams) =>
    switch (keyEvent.keycode) {
    | v when Key.Keycode.right == v => Right
    | v when Key.Keycode.left == v => Left
    | 1073741906 => Up
    | _ => Unknown
    };
};

module State = {
  module PressedKeys = {
    type t = list(Key.t);

    let hasKey = (key, list) => list |> List.exists(k => k == key);

    let addKey = (key, list) =>
      hasKey(key, list) ? list : List.cons(key, list);

    let removeKey = (key, list) => list |> List.filter(k => k != key);
  };
  module Mario = {
    type movement =
      | Left(int)
      | Right(int)
      | Idle
      | Jump;

    type t = {
      positionX: float,
      positionY: float,
      velocityY: float,
      velocityX: float,
      movement,
    };

    let minY = Constants.floorHeight |> float_of_int;
    let maxY = 200.0;

    let init = () => {
      positionX: 30.0,
      positionY: minY,
      velocityY: 0.0,
      velocityX: 0.0,
      movement: Idle,
    };

    let isJumping = mario => mario.positionY > minY;

    let increaseFallingVelocity = (~positionY, ~velocityY) =>
      positionY <= minY ? 0.0 : positionY >= maxY ? (-100.0) : velocityY;

    let applyGravity = (~deltaTime, mario: t) => {
      let nextVelocityY = mario.velocityY +. Constants.gravity *. deltaTime;
      let nextPositionY = mario.positionY +. nextVelocityY *. deltaTime;

      let positionY = clamp(~min=minY, ~max=maxY, nextPositionY);
      let velocityY =
        increaseFallingVelocity(~positionY, ~velocityY=nextVelocityY);

      {...mario, velocityY, positionY};
    };

    let applyFriction = (~deltaTime, keys: PressedKeys.t, mario: t) => {
      let velocityX =
        (
          switch (
            PressedKeys.hasKey(Key.Left, keys),
            PressedKeys.hasKey(Key.Right, keys),
          ) {
          | (true, false) => mario.velocityX -. Constants.accelerationX
          | (false, true) => mario.velocityX +. Constants.accelerationX
          | _ => mario.velocityX *. Constants.friction
          }
        )
        |> clamp(
             ~min=(-1.0) *. Constants.maxVelocityX,
             ~max=Constants.maxVelocityX,
           );

      let positionX = mario.positionX +. velocityX *. deltaTime;
      {...mario, positionX, velocityX};
    };

    let keysToMovement = (time, keys: PressedKeys.t) => {
      let frameFromTime = int_of_float(time *. 6.) mod 3;

      if (keys |> PressedKeys.hasKey(Key.Left)) {
        Left(frameFromTime);
      } else if (keys |> PressedKeys.hasKey(Key.Right)) {
        Right(frameFromTime);
      } else {
        Idle;
      };
    };

    let updateMovement = (~time, keys: PressedKeys.t, mario: t) => {
      let movement = isJumping(mario) ? Jump : keysToMovement(time, keys);
      {...mario, movement};
    };

    let step = (~time, ~deltaTime, keys: list(Key.t), mario: t) =>
      mario
      |> applyGravity(~deltaTime)
      |> applyFriction(~deltaTime, keys)
      |> updateMovement(~time, keys);

    let jump = (mario: t) => {
      ...mario,
      velocityY: isJumping(mario) ? mario.velocityY : Constants.velocityY,
    };
  };

  type state = {
    mario: Mario.t,
    pressedKeys: list(Key.t),
    time: float,
  };

  let init = () => {mario: Mario.init(), time: 0., pressedKeys: []};

  type action =
    | KeyDown(Key.t)
    | KeyUp(Key.t)
    | Step(float);

  let shouldJump = (key, pressedKeys) =>
    key == Key.Up && !PressedKeys.hasKey(Key.Up, pressedKeys);

  let reducer = (action, state) =>
    switch (action) {
    | KeyDown(key) =>
      let mario =
        shouldJump(key, state.pressedKeys)
          ? Mario.jump(state.mario) : state.mario;

      let pressedKeys = PressedKeys.addKey(key, state.pressedKeys);
      {...state, pressedKeys, mario};
    | KeyUp(key) => {
        ...state,
        pressedKeys: PressedKeys.removeKey(key, state.pressedKeys),
      }

    | Step(deltaTime) =>
      let time = state.time +. deltaTime;
      let mario =
        Mario.step(~deltaTime, ~time, state.pressedKeys, state.mario);

      {...state, time, mario};
    };
};

module Mario = {
  open Assets.Mario;

  let image = (~movement: State.Mario.movement, ()) => {
    let (src, height, width) =
      switch (movement) {
      | Left(ind)
      | Right(ind) =>
        let image =
          switch (ind) {
          | 0 => Moving.image1
          | 1 => Moving.image2
          | 2 => Moving.image3
          | _ => Moving.image1
          };
        (image, Moving.height, Moving.width);
      | Idle => (Idle.image, Idle.height, Idle.width)
      | Jump => (Jump.image, Jump.height, Jump.width)
      };

    <Image src={getPathToAsset(src)} width height />;
  };

  let make = (~mario: State.Mario.t, ()) => {
    <Positioned
      left={mario.positionX |> int_of_float}
      bottom={mario.positionY |> int_of_float}>
      <image movement={mario.movement} />
    </Positioned>;
  };
};

let floor = () => {
  <Positioned bottom=0 left=0>
    <Stack width=Constants.width height=Constants.floorHeight>
      <Image
        src=Assets.Floor.image
        width=Constants.width
        height=Assets.Floor.height
        resizeMode=ImageResizeMode.Repeat
      />
      <Image
        src=Assets.Floor.image
        width=Constants.width
        height=Assets.Floor.height
        resizeMode=ImageResizeMode.Repeat
      />
    </Stack>
  </Positioned>;
};

module World = {
  let%component make = () => {
    let%hook (state, dispatch) =
      Hooks.reducer(~initialState=State.init(), State.reducer);

    let%hook () =
      Hooks.tick(~tickRate=Time.zero, t =>
        dispatch(Step(Time.toFloatSeconds(t)))
      );

    let containerStyle =
      Style.[
        position(`Absolute),
        justifyContent(`Center),
        alignItems(`Center),
        bottom(0),
        top(0),
        left(0),
        right(0),
      ];

    let%hook () =
      Hooks.effect(OnMount, () => {
        //Audio.playMusic(30.0, "mario.wav") |> ignore;
        None
      });

    <Clickable
      style=containerStyle
      onKeyUp={event => State.KeyUp(event |> Key.fromKeyEvent) |> dispatch}
      onKeyDown={(event: NodeEvents.keyEventParams) => {
        let key = Key.fromKeyEvent(event);
        switch (key) {
        | Up =>
          if (!State.PressedKeys.hasKey(Key.Up, state.pressedKeys)) {
            Audio.playSound(30.0, getPathToAsset("smb_jump-small.wav"));
            ignore();
          }
        | _ => ignore()
        };
        dispatch(KeyDown(key));
      }}>
      <floor />
      <Mario mario={state.mario} />
    </Clickable>;
  };
};

let init = app => {
  let _ = Revery.Log.listen((_, msg) => print_endline("LOG: " ++ msg));

  let win = App.createWindow(app, "Welcome to Revery!");

  let element = <AudioProvider> <World /> </AudioProvider>;

  let _ = UI.start(win, element);
  ();
};

App.start(init);
