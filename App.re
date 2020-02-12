open Revery;
open Revery.UI;
open Revery.UI.Components;

module AudioProvider = {
  let%component make = (~children, ()) => {
    let%hook () =
      Hooks.effect(
        OnMount,
        () => {
          Audio.initSound();
          Some(Audio.endSound);
        },
      );
    children;
  };
};

module Constants = {
  let gravity = (-500.0);
  let velocityY = 500.;
  let velocityX = 20.0;
  let friction = 0.8;

  let maxVelocityX = 200.0;
  let accelerationX = 100.0;
};

let clamp = (~min, ~max, value) => {
  value < min ? min : value > max ? max : value;
};

module Assets = {
  let frame1 = "0.gif";
  let frame2 = "1.gif";
  let frame3 = "2.gif";
};

module State = {
  type pressedKey =
    | Left
    | Right;

  module Mario = {
    type t = {
      positionX: float,
      positionY: float,
      velocityY: float,
      velocityX: float,
    };

    let minY = 30.0;
    let maxY = 200.0;

    let init = () => {
      positionX: 30.0,
      positionY: minY,
      velocityY: 0.0,
      velocityX: 0.0,
    };

    let isJumping = mario => mario.positionY > minY;

    let applyGravity = (time: float, key: option(pressedKey), mario: t) => {
      let velocityY = mario.velocityY +. Constants.gravity *. time;
      let positionY = mario.positionY +. velocityY *. time;

      let velocityX =
        switch (key) {
        | Some(direction) =>
          let newVelocity =
            switch (direction) {
            | Left => mario.velocityX -. Constants.accelerationX
            | Right => mario.velocityX +. Constants.accelerationX
            };
          clamp(
            ~min=(-1.0) *. Constants.maxVelocityX,
            ~max=Constants.maxVelocityX,
            newVelocity,
          );

        | None => mario.velocityX *. Constants.friction
        };

      let positionX = mario.positionX +. velocityX *. time;

      {
        velocityX,
        positionX,
        positionY: clamp(~min=minY, ~max=maxY, positionY),
        velocityY:
          positionY < minY ? 0.0 : positionY > maxY ? (-100.0) : velocityY,
      };
    };

    let jump = (mario: t) => {
      ...mario,
      velocityY: isJumping(mario) ? mario.velocityY : Constants.velocityY,
    };
  };
};

module Mario = {
  let gif = "mario.gif";

  let make = (~left, ~bottom, ()) => {
    <Positioned bottom left>
      <Image src=gif width=100 height=100 />
    </Positioned>;
  };
};

type audioState =
  | Playing
  | Paused
  | NotStarted;

type state = {
  audioState,
  mario: State.Mario.t,
  pressedKey: option(State.pressedKey),
};

type action =
  | Toggle
  | KeyDown(State.pressedKey)
  | KeyUp
  | Step(float)
  | Jump;

let reducer = (action, state) =>
  switch (action) {
  | Toggle =>
    let audioState =
      switch (state.audioState) {
      | Playing => Paused
      | Paused => Playing
      | NotStarted => Playing
      };
    {...state, audioState};
  | KeyDown(key) => {...state, pressedKey: Some(key)}
  | KeyUp => {...state, pressedKey: None}
  | Jump => {...state, mario: State.Mario.jump(state.mario)}
  | Step(time) => {
      ...state,
      mario: State.Mario.applyGravity(time, state.pressedKey, state.mario),
    }
  };

module SimpleButton = {
  let make = (~onClick, ~text, ()) => {
    let wrapperStyle =
      Style.[
        backgroundColor(Color.rgba(1., 1., 1., 0.1)),
        border(~width=2, ~color=Colors.white),
        margin(16),
      ];

    let textHeaderStyle =
      Style.[
        color(Colors.white),
        fontFamily("Roboto-Regular.ttf"),
        fontSize(20),
      ];

    <Clickable onClick>
      <View style=wrapperStyle>
        <Padding padding=4> <Text style=textHeaderStyle text /> </Padding>
      </View>
    </Clickable>;
  };
};

module MusicPlayer = {
  let%component make = () => {
    let%hook (state, dispatch) =
      Hooks.reducer(
        ~initialState={
          audioState: NotStarted,
          mario: State.Mario.init(),
          pressedKey: None,
        },
        reducer,
      );

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
    // Revery_UI.NodeEvents.keyEventParams
    <Clickable
      style=containerStyle
      onKeyUp={_ => dispatch(KeyUp)}
      onKeyDown={key => {
        if (key.keycode == Key.Keycode.right) {
          dispatch(KeyDown(Right));
        };
        if (key.keycode == 1073741906) {
          Audio.playSound(30.0, "smb_jump-small.wav") |> ignore;
          dispatch(Jump);
        };
        if (key.keycode == Key.Keycode.left) {
          dispatch(KeyDown(Left));
        };
      }}>
      // switch (key.keycode) {
      // | (Key.Keycode.right) =>
      // | _ => ignore()
      // }

        <Mario
          left={state.mario.positionX |> int_of_float}
          bottom={state.mario.positionY |> int_of_float}
        />
      </Clickable>;
    // <SimpleButton
    //   onClick={() => {
    //     Audio.playSound(30.0, "Checkie_Brown-Susie_the_Cat.wav") |> ignore;
    //     dispatch(Toggle);
    //   }}
    //   text="Play"
    // />
    // <SimpleButton
    //   onClick={() => {
    //     switch (state.audioState) {
    //     | Playing => Audio.pauseSound()
    //     | Paused => Audio.resumeSound()
    //     | _ => ignore()
    //     };
    //     dispatch(Toggle);
    //   }}
    //   text="Resume/Pause"
    // />
  };
};

let init = app => {
  let _ = Revery.Log.listen((_, msg) => print_endline("LOG: " ++ msg));

  let win = App.createWindow(app, "Welcome to Revery!");

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

  let innerStyle = Style.[flexDirection(`Row), alignItems(`FlexEnd)];

  let element = <AudioProvider> <MusicPlayer /> </AudioProvider>;

  let _ = UI.start(win, element);
  ();
};

App.start(init);
