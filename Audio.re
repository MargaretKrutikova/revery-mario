external endSound: unit => unit = "SDL_endAudioCAML";

external initSound: unit => unit = "SDL_initAudioCAML";

external playSound: (string, float) => unit = "SDL_playSoundCAML";

external pauseSound: unit => unit = "SDL_pauseSoundCAML";

external resumeSound: unit => unit = "SDL_resumeSoundCAML";

let playSound = (volume, path) =>
  Thread.create(() => playSound(path, volume), ());
