-module(vbeam_audio).

-moduledoc """
Provides audio runtime bridging functions for V-on-BEAM programs.
""".







%% V sokol.audio runtime support for BEAM.
%% Provides audio output by piping PCM data to an external audio player via Erlang port.
%% Uses sox (play command), aplay (Linux), or afplay (macOS) as the audio backend.
%% State stored in process dictionary (sokol audio is global/singleton).

-export([setup/1, shutdown/0, push/2, is_valid/0]).
-export([sample_rate/0, buffer_frames/0, channels/0]).
-export([fclamp/3]).
-export([generate_tone/3]).

%% Setup audio output.
%% Desc is a map with optional keys: sample_rate, num_channels, buffer_frames.
-doc """
setup/1 is a public runtime entrypoint in `vbeam_audio`.
Parameters: `map()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec setup(map()) -> ok | {error, term()}.

setup(Desc) when is_map(Desc) ->
    SampleRate = maps:get(sample_rate, Desc, 44100),
    NumChannels = maps:get(num_channels, Desc, 1),
    BufferFrames = maps:get(buffer_frames, Desc, 2048),
    true = is_integer(SampleRate) andalso SampleRate > 0,
    true = is_integer(NumChannels) andalso NumChannels > 0,
    true = is_integer(BufferFrames) andalso BufferFrames > 0,
    Cmd = find_audio_cmd(SampleRate, NumChannels),
    case Cmd of
        false ->
            put(vbeam_audio_valid, false),
            {error, no_audio_backend};
        CmdStr ->
            Port = open_port({spawn, CmdStr}, [binary, stream, use_stdio]),
            put(vbeam_audio_port, Port),
            put(vbeam_audio_valid, true),
            put(vbeam_audio_sample_rate, SampleRate),
            put(vbeam_audio_channels, NumChannels),
            put(vbeam_audio_buffer_frames, BufferFrames),
            ok
    end.

%% Find available audio command.
%% Tries sox (play) first (cross-platform), then aplay (Linux).
-spec find_audio_cmd(pos_integer(), pos_integer()) -> string() | false.
find_audio_cmd(SampleRate, Channels) ->
    case os:find_executable("play") of
        false ->
            case os:find_executable("sox") of
                false ->
                    case os:find_executable("aplay") of
                        false -> false;
                        _ ->
                            lists:flatten(io_lib:format(
                                "aplay -t raw -f FLOAT_LE -r ~B -c ~B -q",
                                [SampleRate, Channels]))
                    end;
                _ ->
                    lists:flatten(io_lib:format(
                        "play -t raw -r ~B -e floating-point -b 32 -c ~B --no-show-progress -",
                        [SampleRate, Channels]))
            end;
        _ ->
            lists:flatten(io_lib:format(
                "play -t raw -r ~B -e floating-point -b 32 -c ~B --no-show-progress -",
                [SampleRate, Channels]))
    end.

%% Push audio frames (float32 PCM data as binary).
%% Returns number of frames actually pushed, or 0 on error.
-doc """
push/2 is a public runtime entrypoint in `vbeam_audio`.
Parameters: `binary()`, `non_neg_integer()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec push(binary(), non_neg_integer()) -> non_neg_integer().

push(Frames, NumFrames) when is_binary(Frames), is_integer(NumFrames), NumFrames >= 0 ->
    case get(vbeam_audio_port) of
        undefined -> 0;
        Port ->
            try
                port_command(Port, Frames),
                NumFrames
            catch _:_ -> 0
            end
    end;
push(_, _) -> 0.

%% Shutdown audio output, close port.
-doc """
shutdown/0 is a public runtime entrypoint in `vbeam_audio`.
No parameters.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec shutdown() -> ok.

shutdown() ->
    case get(vbeam_audio_port) of
        undefined -> ok;
        Port ->
            catch port_close(Port),
            erase(vbeam_audio_port),
            put(vbeam_audio_valid, false),
            ok
    end.

%% Query: is audio valid (setup succeeded)?
-spec is_valid() -> boolean().
is_valid() -> get(vbeam_audio_valid) =:= true.

%% Query: current sample rate.
-doc """
sample_rate/0 is a public runtime entrypoint in `vbeam_audio`.
No parameters.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec sample_rate() -> pos_integer().

sample_rate() ->
    case get(vbeam_audio_sample_rate) of
        undefined -> 44100;
        V -> V
    end.

%% Query: current buffer frames.
-spec buffer_frames() -> pos_integer().

buffer_frames() ->
    case get(vbeam_audio_buffer_frames) of
        undefined -> 2048;
        V -> V
    end.

%% Query: current number of channels.
-doc """
channels/0 is a public runtime entrypoint in `vbeam_audio`.
No parameters.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec channels() -> pos_integer().
channels() ->
    case get(vbeam_audio_channels) of
        undefined -> 1;
        V -> V
    end.

%% Helper: clamp float value to [Min, Max].
-spec fclamp(float(), float(), float()) -> float().
fclamp(Val, Min, Max)
  when is_number(Val), is_number(Min), is_number(Max), Min =< Max ->
    if Val < Min -> Min;
       Val > Max -> Max;
       true -> Val
    end.

%% Generate a sine wave tone for testing.
%% Returns binary of 32-bit float little-endian PCM samples.
-doc """
generate_tone/3 is a public runtime entrypoint in `vbeam_audio`.
Parameters: `number()`, `number()`, `pos_integer()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec generate_tone(number(), number(), pos_integer()) -> binary().
generate_tone(Frequency, Duration, SampleRate)
  when is_number(Frequency), Frequency > 0, is_number(Duration), Duration >= 0,
       is_integer(SampleRate), SampleRate > 0 ->
    NumSamples = round(Duration * SampleRate),
    generate_samples(Frequency, SampleRate, 0, NumSamples, <<>>).

generate_samples(_, _, N, Max, Acc) when N >= Max -> Acc;
generate_samples(Freq, SR, N, Max, Acc) ->
    Sample = math:sin(2 * math:pi() * Freq * N / SR) * 0.5,
    Bin = <<Sample:32/float-little>>,
    generate_samples(Freq, SR, N + 1, Max, <<Acc/binary, Bin/binary>>).






