module mixer
import sdl

#include <SDL_mixer.h>


pub const (
	MIX_CHANNEL_POST = -2
	MIX_MAX_VOLUME = C.MIX_MAX_VOLUME
	MIX_CHANNELS = 8
	MIX_DEFAULT_FREQUENCY = 22050
	MIX_DEFAULT_FORMAT = C.MIX_DEFAULT_FORMAT

    MIX_INIT_FLAC = 0x00000001
    MIX_INIT_MOD = 0x00000002
    MIX_INIT_MP3 = 0x00000008
    MIX_INIT_OGG = 0x00000010
    MIX_INIT_MID = 0x00000020
    MIX_INIT_OPUS = 0x00000040
)

// Structs
// MIX TODO: get this working as a return type
pub struct C.Mix_Chunk {
    allocated int
    abuf &byte // *UInt8
    alen u32
    volume byte       /* Per-sample volume, 0-128 */
}

pub struct C.Mix_Music {}


// Methods

// MIX
fn C.Mix_Init(flags int) int
fn C.Mix_OpenAudio(frequency int, format u16, channels int, chunksize int) int
fn C.Mix_CloseAudio()

fn C.Mix_LoadMUS(file byteptr) voidptr // *Mix_Music
fn C.Mix_LoadMUS_RW(src &SDL_RWops, freesrc int) voidptr // *Mix_Music
fn C.Mix_LoadWAV(file byteptr) voidptr // *Mix_Chunk
fn C.Mix_LoadWAV_RW(src &SDL_RWops, freesrc int) voidptr // *Mix_Chunk

// Music
fn C.Mix_FadeInMusic(music &Mix_Music, loops int, ms int) int
fn C.Mix_PlayMusic(music &SDL_AudioSpec, loops int) int
fn C.Mix_VolumeMusic(volume int) int
fn C.Mix_PauseMusic()
fn C.Mix_ResumeMusic()
fn C.Mix_RewindMusic()
fn C.Mix_SetMusicPosition(position f64) int
fn C.Mix_PausedMusic() int
fn C.Mix_HaltMusic() int
fn C.Mix_FadeOutMusic(ms int) int
fn C.Mix_HookMusicFinished(cb fn())
fn C.Mix_FreeMusic(music &Mix_Music)

// Channels
fn C.Mix_VolumeChunk(chunk &Mix_Chunk, volume int) int
fn C.Mix_PlayChannel(channel int, chunk &Mix_Chunk, loops int) int
fn C.Mix_FadeInChannel(channel int, chunk &Mix_Chunk, loops int, ms int) int
fn C.Mix_PlayChannelTimed(channel int, chunk &Mix_Chunk, loops int, ticks int) int
fn C.Mix_Pause(channel int)
fn C.Mix_Resume(channel int)
fn C.Mix_HaltChannel(channel int) int
fn C.Mix_ExpireChannel(channel int, ticks int) int
fn C.Mix_FadeOutChannel(channel int, ms int) int
fn C.Mix_ChannelFinished(cb fn (int))
fn C.Mix_Playing(channel int) int
fn C.Mix_Paused(channel int) int
fn C.Mix_GetChunk(channel int) voidptr //Mix_Chunk
fn C.Mix_FreeChunk(chunk &Mix_Chunk)
fn C.Mix_ReserveChannels(num int) int

// Groups
fn C.Mix_GroupChannel(which int, tag int) int
fn C.Mix_GroupChannels(from int, to int, tag int) int
fn C.Mix_GroupAvailable(tag int) int
fn C.Mix_GroupCount(tag int) int
fn C.Mix_GroupOldest(tag int) int
fn C.Mix_GroupNewer(tag int) int
fn C.Mix_FadeOutGroup(tag int, ms int) int
fn C.Mix_HaltGroup(tag int) int

// Effects
type EffectFunc fn (int, voidptr, int, voidptr) // int chan, void *stream, int len, void *udata
type EffectDone fn (int, voidptr) // int chan, void *udata

fn C.Mix_RegisterEffect(channel int, f EffectFunc, d EffectDone, arg voidptr) int
fn C.Mix_UnregisterEffect(channel int, f EffectFunc) int
fn C.Mix_UnregisterAllEffects(channel int) int
fn C.Mix_SetPanning(channel int, left byte, right byte) int
fn C.Mix_SetDistance(channel int, distance byte) int
fn C.Mix_SetPosition(channel int, angle i16, distance byte) int
fn C.Mix_SetReverseStereo(channel int, flip int) int

pub const (
  version = sdl.version // TODO: remove this hack to mark sdl as used; avoids warning
)
