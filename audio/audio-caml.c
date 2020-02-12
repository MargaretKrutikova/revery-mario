#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <SDL2/SDL.h>

#include "audio-caml.h"
#include "audio.h"

CAMLprim value SDL_initAudioCAML()
{
  CAMLparam0();

  // Initialize SDL2 Audio only
  SDL_Init(SDL_INIT_AUDIO);

  // Initialize Simple-SDL2-Audio
  initAudio();

  CAMLreturn(Val_unit);
}

CAMLprim value SDL_endAudioCAML()
{
  CAMLparam0();

  // End Simple-SDL2-Audio
  endAudio();

  // End SDL2
  SDL_Quit();

  CAMLreturn(Val_unit);
}

CAMLprim value SDL_playSoundCAML(value name, value volumeScale)
{
  CAMLparam2(name, volumeScale);

  const char *filename = String_val(name);

  // the max sound is horribly loud, so adjust it by half
  double volume = (SDL_MIX_MAXVOLUME * Double_val(volumeScale)) / 2;
  caml_release_runtime_system();

  playSound(filename, volume);

  SDL_Delay(1000);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value SDL_playMusicCAML(value name, value volumeScale)
{
  CAMLparam2(name, volumeScale);

  const char *filename = String_val(name);

  // the max sound is horribly loud, so adjust it by half
  double volume = (SDL_MIX_MAXVOLUME * Double_val(volumeScale)) / 2;
  caml_release_runtime_system();

  playMusic(filename, volume);
  SDL_Delay(1000);

  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value SDL_pauseSoundCAML()
{
  CAMLparam0();

  pauseAudio();

  CAMLreturn(Val_unit);
}

CAMLprim value SDL_resumeSoundCAML()
{
  CAMLparam0();

  unpauseAudio();

  CAMLreturn(Val_unit);
}
