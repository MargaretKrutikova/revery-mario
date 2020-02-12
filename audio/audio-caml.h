#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "audio.h"

CAMLprim value SDL_initAudioCAML();
CAMLprim value SDL_endAudioCAML();
CAMLprim value SDL_playSoundCAML(value name, value volumeScale);
CAMLprim value SDL_pauseSoundCAML();
CAMLprim value SDL_resumeSoundCAML();
CAMLprim value SDL_playMusicCAML(value name, value volumeScale);
