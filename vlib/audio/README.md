# vlib/audio

Backend-independent audio for V. Pure-V core in `audio.v`/`spatial.v`, with
device output behind a small function-pointer interface.

## Core

- `Buffer` — interleaved 16-bit PCM with channels/sample rate.
- `load_wav`/`decode_wav` — 16-bit mono/stereo RIFF/WAVE (compressed formats
  are rejected with an error).
- `resample` — linear interpolation to a target sample rate.
- `gain`, `mix_into`, `generate_sine` — sample helpers.
- `Bus` — gain/pan/lowpass processing for interleaved float stereo.
- `Listener`, `PositionalVoice` — distance attenuation and stereo panning.
- `Stream` — chunked playback with optional looping.

## Codecs

- `adpcm_encode`/`adpcm_decode` — pure-V IMA ADPCM, 4 bits per sample (4:1).
- `encode_wav` — writes a 16-bit PCM WAV container.
- `load_ogg`/`decode_ogg_memory` — OGG/Vorbis decode via the bundled
  `encoding.vorbis` (stb_vorbis, native only).
- `load_compressed` — dispatches by extension between WAV and OGG.

MP3 and compressed encoding are not bundled in this checkout.

## Devices

- `null_device()` — headless, always available, no output.
- `sokol_device(cfg)` — native output through `sokol.audio` (`.c.v`).
- `webaudio_device(cfg)` — browser output through WebAudio (`.js.v`).
- `audio.Device` exposes `setup`, `push`, `set_volume`, `close`.

## gg integration

`gg.load_wav` decodes through `vlib/audio` and wraps the result in `gg.Sound`.
`gg.new_shared_audio_device()` and `gg.shared_audio_null_device()` expose the
shared device API from gg contexts.
