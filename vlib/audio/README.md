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

## Devices

- `null_device()` — headless, always available, no output.
- `sokol_device(cfg)` — native output through `sokol.audio` (`.c.v`).
- `audio.Device` exposes `setup`, `push`, `set_volume`, `close`.

## gg integration

`gg.load_wav` decodes through `vlib/audio` and wraps the result in `gg.Sound`.
`gg.new_shared_audio_device()` and `gg.shared_audio_null_device()` expose the
shared device API from gg contexts.
