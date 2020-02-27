open Revery;
open Revery.UI;
open Revery.UI.Components;

let getPathToAsset = assetName =>
  Environment.getExecutingDirectory() ++ assetName;

module Physics = {
  // g = 2 * H / t ^ 2
  let calcGravity = (~h, ~t) => (-2.0) *. h /. t ** 2.0;

  // V = 2 * H / t
  let calcJumpVelocity = (~h, ~t) => 2.0 *. h /. t;
};

module Constants = {
  // jump
  let jumpHeight = 200.0;
  let jumpDuration = 0.6;

  let gravity = Physics.calcGravity(~h=jumpHeight, ~t=jumpDuration);
  let velocityY = Physics.calcJumpVelocity(~h=jumpHeight, ~t=jumpDuration);

  // walk / run
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

module PressedKeys = {
  type t = list(Key.t);
  let hasKey = (key, list) => list |> List.exists(k => k == key);
  let addKey = (key, list) =>
    hasKey(key, list) ? list : List.cons(key, list);
  let removeKey = (key, list) => list |> List.filter(k => k != key);
};

module Direction = {
  type t =
    | Left
    | Right;
};

module State = {
  module Mario = {
    type movement =
      | Run(int)
      | Idle
      | Jump;

    type t = {
      positionX: float,
      positionY: float,
      velocityY: float,
      velocityX: float,
      movement,
      direction: Direction.t,
    };

    let minY = Constants.floorHeight |> float_of_int;
    let maxY = 200.0;

    let init = () => {
      positionX: 30.0,
      positionY: minY,
      velocityY: 0.0,
      velocityX: 0.0,
      movement: Idle,
      direction: Right,
    };

    let isJumping = mario => mario.positionY > minY;

    let isRunning = keys =>
      PressedKeys.hasKey(Key.Left, keys)
      || PressedKeys.hasKey(Key.Right, keys);

    let getDirectionFromKeys = pressedKeys =>
      switch (
        PressedKeys.hasKey(Key.Left, pressedKeys),
        PressedKeys.hasKey(Key.Right, pressedKeys),
      ) {
      | (true, false) => Some(Direction.Left)
      | (false, true) => Some(Right)
      | _ => None
      };

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
          switch (getDirectionFromKeys(keys)) {
          | Some(Direction.Left) => mario.velocityX -. Constants.accelerationX
          | Some(Right) => mario.velocityX +. Constants.accelerationX
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

    let updateMovement = (~time, keys: PressedKeys.t, mario: t) => {
      let movement =
        switch (isJumping(mario), isRunning(keys)) {
        | (true, _) => Jump
        | (false, true) =>
          let frameFromTime = int_of_float(time *. 6.) mod 3;
          Run(frameFromTime);
        | _ => Idle
        };

      {...mario, movement};
    };

    let updateDirection = (keys: PressedKeys.t, mario: t) => {
      let direction =
        switch (getDirectionFromKeys(keys)) {
        | Some(dir) => dir
        | None => mario.direction
        };

      {...mario, direction};
    };

    let step = (~time, ~deltaTime, keys: PressedKeys.t, mario: t) =>
      mario
      |> applyGravity(~deltaTime)
      |> applyFriction(~deltaTime, keys)
      |> updateMovement(~time, keys)
      |> updateDirection(keys);

    let jump = (mario: t) => {
      ...mario,
      velocityY: isJumping(mario) ? mario.velocityY : Constants.velocityY,
    };
  };

  type state = {
    mario: Mario.t,
    pressedKeys: PressedKeys.t,
    time: float,
  };

  let init = () => {mario: Mario.init(), time: 0., pressedKeys: []};

  type action =
    | KeyDown(Key.t)
    | KeyUp(Key.t)
    | Step(float);

  let shouldJump = (key, pressedKeys) =>
    key == Key.Up && !PressedKeys.hasKey(Key.Up, pressedKeys);

  let removeOppositeDirectionKey = (key, list) =>
    switch (key) {
    | Key.Left => PressedKeys.removeKey(Key.Right, list)
    | Right => PressedKeys.removeKey(Key.Left, list)
    | _ => list
    };

  let reducer = (action, state) =>
    switch (action) {
    | KeyDown(key) =>
      let mario =
        shouldJump(key, state.pressedKeys)
          ? Mario.jump(state.mario) : state.mario;

      let pressedKeys =
        state.pressedKeys
        |> PressedKeys.addKey(key)
        |> removeOppositeDirectionKey(key);

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

  let image = (~mario: State.Mario.t, ()) => {
    let (src, height, width) =
      switch (mario.movement) {
      | Run(ind) =>
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
    let style =
      switch (mario.direction) {
      | Left =>
        Style.[
          transform([
            Transform.ScaleX(-1.0),
            Transform.TranslateX(- Moving.width |> float_of_int),
          ]),
        ]
      | _ => Style.[]
      };
    <Image src={getPathToAsset(src)} width height style />;
  };

  let make = (~mario: State.Mario.t, ()) => {
    <Positioned
      left={mario.positionX |> int_of_float}
      bottom={mario.positionY |> int_of_float}>
      <image mario />
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

module SoundHandler = {
  let bgSound = "SuperMarioBros.wav" |> getPathToAsset;
  let jumpSound = "smb_jump-small.wav" |> getPathToAsset;

  let playBackgroundSound = () => Audio.playMusic(10.0, bgSound) |> ignore;
  let playJumpSound = () => Audio.playSound(30.0, jumpSound) |> ignore;
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
      Hooks.effect(
        OnMount,
        () => {
          SoundHandler.playBackgroundSound();
          None;
        },
      );

    <Clickable
      style=containerStyle
      onKeyUp={event => State.KeyUp(event |> Key.fromKeyEvent) |> dispatch}
      onKeyDown={(event: NodeEvents.keyEventParams) => {
        let key = Key.fromKeyEvent(event);
        // TODO: refactor into shouldPlaySound
        if (key == Up && !PressedKeys.hasKey(Key.Up, state.pressedKeys)) {
          SoundHandler.playJumpSound();
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
