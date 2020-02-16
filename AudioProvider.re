open Revery;
open Revery.UI;
open Revery.UI.Components;

let%component make = (~children: React.element(React.reveryNode), ()) => {
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
