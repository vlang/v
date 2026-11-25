#define _GNU_SOURCE
#include "native_codecs_probe.h"
#include <alsa/asoundlib.h>
#include <ass/ass.h>
#include <fribidi/fribidi.h>
#include <hb-ft.h>
#include <hb.h>
#include <libavcodec/avcodec.h>
#include <libavdevice/avdevice.h>
#include <libavfilter/avfilter.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
#include <libavformat/avformat.h>
#include <libavutil/channel_layout.h>
#include <libavutil/mathematics.h>
#include <libavutil/opt.h>
#include <libswresample/swresample.h>
#include <libswscale/swscale.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define CHECK(c) do { if (!(c)) return __LINE__; } while (0)

static int subtitle_and_shaping_probe(const char *font_path) {
    FriBidiChar logical[] = {0x05d0, 0x05d1, 0x05d2}, visual[3] = {0};
    FriBidiParType direction = FRIBIDI_PAR_RTL;
    CHECK(fribidi_log2vis(logical, 3, &direction, visual, NULL, NULL, NULL));
    CHECK(visual[0] == 0x05d2 && visual[1] == 0x05d1 && visual[2] == 0x05d0);
    FT_Library library = NULL;
    FT_Face face = NULL;
    CHECK(!FT_Init_FreeType(&library));
    CHECK(!FT_New_Face(library, font_path, 0, &face));
    CHECK(!FT_Set_Char_Size(face, 0, 28 * 64, 72, 72));
    hb_font_t *font = hb_ft_font_create_referenced(face);
    hb_buffer_t *buffer = hb_buffer_create();
    CHECK(font && buffer);
    hb_buffer_add_utf8(buffer, "\xd8\xb3\xd9\x84\xd8\xa7\xd9\x85", -1, 0, -1);
    hb_buffer_set_direction(buffer, HB_DIRECTION_RTL);
    hb_buffer_set_script(buffer, HB_SCRIPT_ARABIC);
    hb_buffer_set_language(buffer, hb_language_from_string("ar", -1));
    hb_shape(font, buffer, NULL, 0);
    unsigned count = 0;
    hb_glyph_info_t *glyphs = hb_buffer_get_glyph_infos(buffer, &count);
    hb_glyph_position_t *positions = hb_buffer_get_glyph_positions(buffer, NULL);
    CHECK(glyphs && positions && count >= 2 && count <= 4);
    for (unsigned i = 0; i < count; ++i)
        CHECK(glyphs[i].codepoint && positions[i].x_advance > 0);
    CHECK(glyphs[0].cluster > glyphs[count - 1].cluster);
    hb_buffer_destroy(buffer);
    hb_font_destroy(font);
    FT_Done_Face(face);
    FT_Done_FreeType(library);

    static const char script[] =
        "[Script Info]\nScriptType: v4.00+\nPlayResX: 320\nPlayResY: 180\n"
        "[V4+ Styles]\n"
        "Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, "
        "Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, "
        "Shadow, Alignment, MarginL, MarginR, MarginV, Encoding\n"
        "Style: Default,DejaVu Sans,26,&H00FFFFFF,&H000000FF,&H00000000,&H00000000,"
        "0,0,0,0,100,100,0,0,1,1,0,2,10,10,10,1\n"
        "[Events]\nFormat: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\n"
        "Dialogue: 0,0:00:00.00,0:00:02.00,Default,,0,0,0,,Native subtitles ";
    const char arabic[] = "\xd8\xb3\xd9\x84\xd8\xa7\xd9\x85\n";
    char contents[sizeof(script) + sizeof(arabic)];
    snprintf(contents, sizeof(contents), "%s%s", script, arabic);
    ASS_Library *ass = ass_library_init();
    CHECK(ass);
    ASS_Renderer *renderer = ass_renderer_init(ass);
    CHECK(renderer);
    ass_set_frame_size(renderer, 320, 180);
    ass_set_storage_size(renderer, 320, 180);
    ass_set_fonts(renderer, font_path, "DejaVu Sans", ASS_FONTPROVIDER_FONTCONFIG, NULL, 1);
    ASS_Track *track = ass_read_memory(ass, contents, strlen(contents), NULL);
    CHECK(track && track->n_events == 1);
    int changed = 0, images = 0, ink = 0, fill = 0, outline = 0;
    ASS_Image *image = ass_render_frame(renderer, track, 500, &changed);
    CHECK(image && changed);
    for (; image; image = image->next) {
        CHECK(image->w >= 0 && image->h >= 0);
        if (!image->w || !image->h) continue;
        CHECK(image->bitmap && image->stride >= image->w);
        CHECK(image->dst_x >= 0 && image->dst_y >= 0);
        CHECK(image->dst_x + image->w <= 320 && image->dst_y + image->h <= 180);
        for (int y = 0; y < image->h; ++y)
            for (int x = 0; x < image->w; ++x) ink += image->bitmap[y * image->stride + x] != 0;
        ++images;
        fill |= image->color == 0xffffff00u;
        outline |= image->color == 0;
    }
    CHECK(images > 0 && ink > 100 && fill && outline);
    ass_free_track(track);
    ass_renderer_done(renderer);
    ass_library_done(ass);
    return 0;
}

static int pcm_probe(void) {
    snd_config_t *config = NULL;
    snd_input_t *input = NULL;
    snd_pcm_t *pcm = NULL;
    const char text[] = "pcm.v_native_probe { type null }\n";
    CHECK(!snd_config_top(&config));
    CHECK(!snd_input_buffer_open(&input, text, strlen(text)));
    CHECK(!snd_config_load(config, input));
    CHECK(!snd_input_close(input));
    CHECK(!snd_pcm_open_lconf(&pcm, "v_native_probe", SND_PCM_STREAM_PLAYBACK, 0, config));
    CHECK(!snd_pcm_set_params(pcm, SND_PCM_FORMAT_S16_LE, SND_PCM_ACCESS_RW_INTERLEAVED,
                             2, 48000, 1, 20000));
    int16_t samples[480 * 2];
    for (unsigned i = 0; i < sizeof(samples) / sizeof(samples[0]); ++i)
        samples[i] = (int16_t)(sin((double)i / 15.0) * 6000);
    CHECK(snd_pcm_writei(pcm, samples, 480) == 480);
    CHECK(snd_pcm_avail_update(pcm) >= 0);
    CHECK(!snd_pcm_drain(pcm));
    CHECK(!snd_pcm_close(pcm));
    CHECK(!snd_config_delete(config));
    return 0;
}

static int filter_probe(AVFrame *frame, AVRational time_base) {
    AVFilterGraph *graph = avfilter_graph_alloc();
    AVFilterContext *source = NULL, *sink = NULL;
    char arguments[256];
    CHECK(graph);
    graph->nb_threads = 2;
    snprintf(arguments, sizeof(arguments),
        "video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=1/1",
        frame->width, frame->height, frame->format, time_base.num, time_base.den);
    CHECK(avfilter_graph_create_filter(&source, avfilter_get_by_name("buffer"),
                                      "in", arguments, NULL, graph) >= 0);
    CHECK(avfilter_graph_create_filter(&sink, avfilter_get_by_name("buffersink"),
                                      "out", NULL, NULL, graph) >= 0);
    AVFilterInOut *inputs = avfilter_inout_alloc(), *outputs = avfilter_inout_alloc();
    CHECK(inputs && outputs);
    outputs->name = av_strdup("in"); outputs->filter_ctx = source; outputs->pad_idx = 0;
    inputs->name = av_strdup("out"); inputs->filter_ctx = sink; inputs->pad_idx = 0;
    CHECK(avfilter_graph_parse_ptr(graph, "hflip,scale=32:24,format=pix_fmts=yuv420p", &inputs, &outputs, NULL) >= 0);
    CHECK(avfilter_graph_config(graph, NULL) >= 0);
    CHECK(av_buffersrc_add_frame_flags(source, frame, AV_BUFFERSRC_FLAG_KEEP_REF) >= 0);
    AVFrame *filtered = av_frame_alloc();
    CHECK(filtered && av_buffersink_get_frame(sink, filtered) >= 0);
    CHECK(filtered->width == 32 && filtered->height == 24);
    CHECK(filtered->format == AV_PIX_FMT_YUV420P);
    CHECK(filtered->data[0][2 * filtered->linesize[0] + 29] >
          filtered->data[0][2 * filtered->linesize[0] + 2] + 60);
    av_frame_free(&filtered);
    avfilter_inout_free(&inputs); avfilter_inout_free(&outputs);
    avfilter_graph_free(&graph);
    return 0;
}

struct decode_state {
    AVFormatContext *format;
    AVCodecContext *video, *audio;
    int video_stream, audio_stream;
    struct SwsContext *scaler;
    SwrContext *resampler;
    int frames, red_frames, blue_frames, audio_samples, audio_nonzero, seeked;
};

static int receive_frames(struct decode_state *state, AVCodecContext *codec, AVFrame *frame) {
    int received;
    while ((received = avcodec_receive_frame(codec, frame)) >= 0) {
        if (codec == state->video) {
            CHECK(frame->width == 64 && frame->height == 48);
            CHECK(frame->best_effort_timestamp != AV_NOPTS_VALUE);
            int blue = av_compare_ts(frame->best_effort_timestamp,
                state->format->streams[state->video_stream]->time_base, 1, (AVRational){1, 1}) >= 0;
            if (state->seeked) CHECK(blue);
            if (blue) ++state->blue_frames; else ++state->red_frames;
            state->scaler = sws_getCachedContext(state->scaler, 64, 48, frame->format,
                64, 48, AV_PIX_FMT_RGB24, SWS_BILINEAR, NULL, NULL, NULL);
            CHECK(state->scaler);
            uint8_t rgb[48 * 193];
            memset(rgb, 0xa5, sizeof(rgb));
            uint8_t *output[] = {rgb}; int strides[] = {193};
            CHECK(sws_scale(state->scaler, (const uint8_t * const *)frame->data,
                            frame->linesize, 0, 48, output, strides) == 48);
            for (int y = 0; y < 48; ++y) {
                for (int x = 0; x < 64; ++x) {
                    const uint8_t *pixel = rgb + y * 193 + x * 3;
                    if (x < 16 && y < 16) {
                        CHECK(pixel[0] >= 245 && pixel[1] >= 245 && pixel[2] <= 6);
                    } else if (blue) {
                        CHECK(pixel[2] >= 245 && pixel[0] <= 6 && pixel[1] <= 6);
                    } else {
                        CHECK(pixel[0] >= 245 && pixel[1] <= 6 && pixel[2] <= 6);
                    }
                }
                CHECK(rgb[y * 193 + 192] == 0xa5);
            }
            if (!state->frames && !state->seeked) {
                int result = filter_probe(frame, state->format->streams[state->video_stream]->time_base);
                if (result) return result;
            }
            if (state->seeked)
                CHECK(frame->best_effort_timestamp >= 0);
            ++state->frames;
        } else {
            if (!state->resampler) {
                AVChannelLayout stereo = AV_CHANNEL_LAYOUT_STEREO;
                CHECK(swr_alloc_set_opts2(&state->resampler, &stereo, AV_SAMPLE_FMT_S16, 48000,
                    &codec->ch_layout, frame->format, frame->sample_rate, 0, NULL) >= 0);
                CHECK(swr_init(state->resampler) >= 0);
            }
            int16_t samples[8192 * 2]; uint8_t *output[] = {(uint8_t *)samples};
            CHECK(frame->nb_samples <= 8192);
            int converted = swr_convert(state->resampler, output, 8192,
                (const uint8_t **)frame->extended_data, frame->nb_samples);
            CHECK(converted > 0);
            for (int i = 0; i < converted * 2; ++i) state->audio_nonzero += samples[i] != 0;
            state->audio_samples += converted;
        }
        av_frame_unref(frame);
    }
    CHECK(received == AVERROR(EAGAIN) || received == AVERROR_EOF);
    return 0;
}

static int decoding_probe(const char *video_path) {
    struct decode_state state = {0};
    CHECK(avdevice_version() > 0);
    CHECK(avformat_network_init() >= 0);
    CHECK(avformat_open_input(&state.format, video_path, NULL, NULL) >= 0);
    CHECK(avformat_find_stream_info(state.format, NULL) >= 0);
    const AVCodec *video = NULL, *audio = NULL;
    state.video_stream = av_find_best_stream(state.format, AVMEDIA_TYPE_VIDEO, -1, -1, &video, 0);
    state.audio_stream = av_find_best_stream(state.format, AVMEDIA_TYPE_AUDIO, -1, -1, &audio, 0);
    CHECK(state.video_stream >= 0 && state.audio_stream >= 0 && video && audio);
    state.video = avcodec_alloc_context3(video); state.audio = avcodec_alloc_context3(audio);
    CHECK(state.video && state.audio);
    CHECK(avcodec_parameters_to_context(state.video, state.format->streams[state.video_stream]->codecpar) >= 0);
    CHECK(avcodec_parameters_to_context(state.audio, state.format->streams[state.audio_stream]->codecpar) >= 0);
    state.video->pkt_timebase = state.format->streams[state.video_stream]->time_base;
    state.audio->pkt_timebase = state.format->streams[state.audio_stream]->time_base;
    state.video->thread_count = 2;
    CHECK(avcodec_open2(state.video, video, NULL) >= 0 && avcodec_open2(state.audio, audio, NULL) >= 0);
    AVPacket *packet = av_packet_alloc(); AVFrame *frame = av_frame_alloc();
    CHECK(packet && frame);
    for (int pass = 0; pass < 2; ++pass) {
        if (pass) {
            CHECK(av_seek_frame(state.format, -1, AV_TIME_BASE, AVSEEK_FLAG_BACKWARD) >= 0);
            avcodec_flush_buffers(state.video); avcodec_flush_buffers(state.audio);
            swr_free(&state.resampler);
            state.frames = state.red_frames = state.blue_frames = state.audio_samples = state.audio_nonzero = 0;
            state.seeked = 1;
        }
        int received;
        while ((received = av_read_frame(state.format, packet)) >= 0) {
            AVCodecContext *codec = packet->stream_index == state.video_stream ? state.video :
                                    packet->stream_index == state.audio_stream ? state.audio : NULL;
            if (codec) {
                CHECK(avcodec_send_packet(codec, packet) >= 0);
                int result = receive_frames(&state, codec, frame);
                if (result) return result;
            }
            av_packet_unref(packet);
        }
        CHECK(received == AVERROR_EOF);
        CHECK(avcodec_send_packet(state.video, NULL) >= 0 && avcodec_send_packet(state.audio, NULL) >= 0);
        int result = receive_frames(&state, state.video, frame);
        if (result) return result;
        result = receive_frames(&state, state.audio, frame);
        if (result) return result;
        CHECK(state.frames == (pass ? 8 : 16));
        CHECK(state.blue_frames == 8 && state.red_frames == (pass ? 0 : 8));
        CHECK(state.audio_samples >= (pass ? 1000 : 96000) && state.audio_nonzero > 1000);
    }
    swr_free(&state.resampler); sws_freeContext(state.scaler);
    av_frame_free(&frame); av_packet_free(&packet);
    avcodec_free_context(&state.video); avcodec_free_context(&state.audio);
    avformat_close_input(&state.format);
    CHECK(avformat_network_deinit() >= 0);
    return 0;
}

int32_t v_native_codecs_probe(const char *video_path, const char *font_path) {
    int result = subtitle_and_shaping_probe(font_path);
    if (result) return result;
    result = pcm_probe();
    if (result) return result;
    return decoding_probe(video_path);
}
