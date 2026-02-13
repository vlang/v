%% vbeam_sokol - Sokol graphics bridge for BEAM via wx + OpenGL
%%
%% Maps V's sokol API (sapp, gfx, sgl) to Erlang's built-in wx GUI toolkit
%% and gl OpenGL bindings. This enables V programs using sokol graphics to
%% run on the BEAM VM with real GPU-accelerated rendering.
%%
%% Architecture:
%%   - Single global gen_server (sokol is single-window)
%%   - wx Frame + wxGLCanvas for window management
%%   - All gfx.* calls map to gl:* OpenGL calls
%%   - All sgl.* immediate-mode calls map directly to gl:* (no gen_server)
%%   - Timer-driven render loop (~60fps) invokes V frame callback
%%   - wx events mapped to sokol Event maps
%%
%% Usage:
%%   vbeam_sokol:run(#{
%%     window_title => "My App",
%%     width => 800, height => 600,
%%     init_cb => fun() -> ... end,
%%     frame_cb => fun() -> ... end,
%%     cleanup_cb => fun() -> ... end,
%%     event_cb => fun(Event) -> ... end
%%   }).
-module(vbeam_sokol).
-behaviour(gen_server).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

%% Public API - sapp (application)
-export([run/1, width/0, height/0, quit/0, frame_count/0, frame_duration/0]).
-export([isvalid/0, dpi_scale/0, sample_count/0, color_format/0, depth_format/0]).
-export([show_keyboard/1, keyboard_shown/0, show_mouse/1, mouse_shown/0]).
-export([lock_mouse/1, mouse_locked/0, set_mouse_cursor/1]).
-export([request_quit/0, cancel_quit/0, consume_event/0]).
-export([toggle_fullscreen/0, is_fullscreen/0]).
-export([high_dpi/0]).

%% Public API - gfx (graphics)
-export([setup/1, shutdown/0, begin_pass/1, end_pass/0, commit/0]).
-export([create_clear_pass_action/4, create_default_pass/1]).
-export([make_buffer/1, make_image/1, make_sampler/1, make_shader/1, make_pipeline/1]).
-export([apply_pipeline/1, apply_bindings/1, apply_uniforms/3, draw/4]).
-export([apply_viewport/5, apply_scissor_rect/5]).
-export([destroy_buffer/1, destroy_image/1, destroy_sampler/1]).
-export([destroy_shader/1, destroy_pipeline/1]).
-export([update_buffer/2, append_buffer/2, update_image/2]).
-export([reset_state_cache/0, is_valid/0]).
-export([query_backend/0, query_features/0, query_limits/0]).

%% Public API - sgl (immediate mode) — direct gl calls for performance
-export([sgl_setup/1, sgl_shutdown/0, sgl_error/0, sgl_defaults/0]).
-export([sgl_viewport/5, sgl_scissor_rect/5]).
-export([sgl_enable_texture/0, sgl_disable_texture/0, sgl_texture/2]).
-export([sgl_begin_points/0, sgl_begin_lines/0, sgl_begin_line_strip/0]).
-export([sgl_begin_triangles/0, sgl_begin_triangle_strip/0, sgl_begin_quads/0]).
-export([sgl_v2f/2, sgl_v3f/3, sgl_v2f_t2f/4, sgl_v3f_t2f/5]).
-export([sgl_v2f_c3f/5, sgl_v2f_c4f/6, sgl_v2f_c3b/5, sgl_v2f_c4b/6]).
-export([sgl_v2f_c1i/3]).
-export([sgl_v3f_c3f/6, sgl_v3f_c4f/7, sgl_v3f_c3b/6, sgl_v3f_c4b/7]).
-export([sgl_v3f_c1i/4]).
-export([sgl_c3f/3, sgl_c4f/4, sgl_c3b/3, sgl_c4b/4, sgl_c1i/1]).
-export([sgl_t2f/2, sgl_point_size/1]).
-export([sgl_end/0, sgl_draw/0, sgl_context_draw/1]).
-export([sgl_matrix_mode_projection/0, sgl_matrix_mode_modelview/0]).
-export([sgl_matrix_mode_texture/0]).
-export([sgl_load_identity/0, sgl_push_matrix/0, sgl_pop_matrix/0]).
-export([sgl_load_matrix/1, sgl_load_transpose_matrix/1]).
-export([sgl_mult_matrix/1, sgl_mult_transpose_matrix/1]).
-export([sgl_translate/3, sgl_rotate/4, sgl_scale/3]).
-export([sgl_ortho/6, sgl_frustum/6, sgl_perspective/4, sgl_lookat/9]).
-export([sgl_push_pipeline/0, sgl_pop_pipeline/0]).
-export([sgl_load_default_pipeline/0, sgl_load_pipeline/1]).
-export([sgl_make_context/1, sgl_destroy_context/1]).
-export([sgl_set_context/1, sgl_get_context/0, sgl_default_context/0]).
-export([sgl_make_pipeline/1, sgl_destroy_pipeline/1]).
-export([sgl_rad/1, sgl_deg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Frame interval in ms (~60fps)
-define(FRAME_INTERVAL_MS, 16).

-record(state, {
    wx           :: term(),          %% wx object
    frame        :: term(),          %% wxFrame
    canvas       :: term(),          %% wxGLCanvas
    gl_ctx       :: term(),          %% wxGLContext
    width = 800  :: integer(),
    height = 600 :: integer(),
    title = "V Application" :: string(),
    frame_count = 0 :: non_neg_integer(),
    frame_start = 0 :: integer(),
    frame_duration = 0.0 :: float(),
    fullscreen = false :: boolean(),
    quit_requested = false :: boolean(),
    quit_cancelled = false :: boolean(),
    %% Callbacks (V functions)
    init_cb      :: fun(() -> term()) | undefined,
    frame_cb     :: fun(() -> term()) | undefined,
    cleanup_cb   :: fun(() -> term()) | undefined,
    event_cb     :: fun((map()) -> term()) | undefined,
    %% GFX Resources
    buffers = #{} :: #{integer() => integer()},      %% id -> GL buffer
    images = #{}  :: #{integer() => integer()},       %% id -> GL texture
    samplers = #{} :: #{integer() => term()},         %% id -> sampler config
    shaders = #{} :: #{integer() => integer()},       %% id -> GL program
    pipelines = #{} :: #{integer() => map()},         %% id -> pipeline config
    next_id = 1   :: integer(),
    %% Current render state
    current_pipeline :: integer() | undefined,
    current_bindings :: map() | undefined,
    %% Timer
    timer_ref     :: reference() | undefined
}).

%% ============================================================================
%% Public API - sapp (application lifecycle)
%% ============================================================================

%% @doc Run the application. Creates window and starts event loop.
%% This call blocks until the window is closed (matching sokol_app behavior).
-spec run(map()) -> ok.
run(Desc) ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [Desc], []),
    %% Block until window closes (sokol sapp.run() blocks)
    MonRef = monitor(process, Pid),
    receive {'DOWN', MonRef, _, _, _} -> ok end.

%% @doc Returns the current window width.
-spec width() -> integer().
width() ->
    try gen_server:call(?MODULE, width, 1000)
    catch _:_ -> 0
    end.

%% @doc Returns the current window height.
-spec height() -> integer().
height() ->
    try gen_server:call(?MODULE, height, 1000)
    catch _:_ -> 0
    end.

%% @doc Request application quit.
-spec quit() -> ok.
quit() ->
    gen_server:cast(?MODULE, quit).

%% @doc Returns the frame counter.
-spec frame_count() -> non_neg_integer().
frame_count() ->
    try gen_server:call(?MODULE, frame_count, 1000)
    catch _:_ -> 0
    end.

%% @doc Returns the duration of the last frame in seconds.
-spec frame_duration() -> float().
frame_duration() ->
    try gen_server:call(?MODULE, frame_duration, 1000)
    catch _:_ -> 0.0
    end.

%% @doc Returns true if the application context is valid.
-spec isvalid() -> boolean().
isvalid() ->
    try gen_server:call(?MODULE, isvalid, 1000)
    catch _:_ -> false
    end.

%% @doc Returns DPI scale factor.
-spec dpi_scale() -> number().
dpi_scale() ->
    try gen_server:call(?MODULE, dpi_scale, 1000)
    catch _:_ -> 1.0
    end.

-spec sample_count() -> non_neg_integer().
sample_count() -> 1.
-spec color_format() -> integer().
color_format() -> 1. %% RGBA8
-spec depth_format() -> integer().
depth_format() -> 2. %% DEPTH

-spec show_keyboard(boolean()) -> ok.
show_keyboard(_Visible) -> ok.
-spec keyboard_shown() -> boolean().
keyboard_shown() -> false.
-spec show_mouse(boolean()) -> ok.
show_mouse(_Visible) -> ok.
-spec mouse_shown() -> boolean().
mouse_shown() -> true.
-spec lock_mouse(boolean()) -> ok.
lock_mouse(_Locked) -> ok.
-spec mouse_locked() -> boolean().
mouse_locked() -> false.
-spec set_mouse_cursor(atom() | integer()) -> ok.
set_mouse_cursor(_Cursor) -> ok.
-spec high_dpi() -> boolean().
high_dpi() -> false.

-spec request_quit() -> ok.
request_quit() ->
    gen_server:cast(?MODULE, request_quit).

-spec cancel_quit() -> ok.
cancel_quit() ->
    gen_server:cast(?MODULE, cancel_quit).

-spec consume_event() -> ok.
consume_event() -> ok.

-spec toggle_fullscreen() -> ok.
toggle_fullscreen() ->
    gen_server:cast(?MODULE, toggle_fullscreen).

-spec is_fullscreen() -> boolean().
is_fullscreen() ->
    try gen_server:call(?MODULE, is_fullscreen, 1000)
    catch _:_ -> false
    end.

%% ============================================================================
%% Public API - gfx (graphics resource management)
%% ============================================================================

%% @doc Initialize the graphics subsystem.
-spec setup(map()) -> ok.
setup(_Desc) ->
    %% Graphics setup happens in gen_server init when OpenGL context is created.
    %% This is called by V code after the window exists, so it's a no-op here.
    ok.

%% @doc Shut down the graphics subsystem.
-spec shutdown() -> ok.
shutdown() ->
    ok.

%% @doc Returns true if gfx is initialized.
-spec is_valid() -> boolean().
is_valid() ->
    isvalid().

%% @doc Reset the internal state cache (for debugging).
-spec reset_state_cache() -> ok.
reset_state_cache() ->
    ok.

%% @doc Create a PassAction that clears to RGBA color.
%% Produces a map in the exact format begin_pass/1 expects.
-spec create_clear_pass_action(float(), float(), float(), float()) -> map().
create_clear_pass_action(R, G, B, A) ->
    #{action => #{colors => [#{clear_value => #{r => to_float(R), g => to_float(G),
                                                 b => to_float(B), a => to_float(A)}}]}}.

%% @doc Create a default Pass from a PassAction.
-spec create_default_pass(map()) -> map().
create_default_pass(Action) ->
    %% If Action already has the 'action' key, it IS a PassAction — wrap it.
    %% If it has 'colors' directly, it's the inner action map — wrap in pass.
    case maps:is_key(action, Action) of
        true -> Action;  %% Already a full Pass
        false -> #{action => Action}
    end.

%% @doc Begin a render pass with the given pass action.
%% Direct GL call — frame callback runs inside gen_server where GL context is current.
-spec begin_pass(map()) -> ok.
begin_pass(Pass) ->
    Action = maps:get(action, Pass, #{}),
    Colors = maps:get(colors, Action, []),
    case Colors of
        [First | _] when is_map(First) ->
            ClearVal = maps:get(clear_value, First, #{r => 0.0, g => 0.0, b => 0.0, a => 1.0}),
            R = to_float(maps:get(r, ClearVal, 0.0)),
            G = to_float(maps:get(g, ClearVal, 0.0)),
            B = to_float(maps:get(b, ClearVal, 0.0)),
            A = to_float(maps:get(a, ClearVal, 1.0)),
            gl:clearColor(R, G, B, A);
        _ ->
            gl:clearColor(0.0, 0.0, 0.0, 1.0)
    end,
    DepthAction = maps:get(depth, Action, #{}),
    DepthClear = to_float(maps:get(clear_value, DepthAction, 1.0)),
    gl:clearDepth(DepthClear),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT bor ?GL_STENCIL_BUFFER_BIT),
    ok.

%% @doc End the current render pass.
-spec end_pass() -> ok.
end_pass() -> ok.

%% @doc Commit the current frame (swap buffers).
%% Direct GL call — canvas stored in pdict by render_frame handler.
-spec commit() -> ok.
commit() ->
    case get(vbeam_sokol_canvas) of
        undefined -> gen_server:cast(?MODULE, commit);  %% fallback
        Canvas -> wxGLCanvas:swapBuffers(Canvas)
    end.

%% @doc Create a GPU buffer. Returns a buffer ID.
-spec make_buffer(map()) -> integer().
make_buffer(Desc) ->
    gen_server:call(?MODULE, {make_buffer, Desc}, 5000).

%% @doc Create a GPU image (texture). Returns an image ID.
-spec make_image(map()) -> integer().
make_image(Desc) ->
    gen_server:call(?MODULE, {make_image, Desc}, 5000).

%% @doc Create a sampler. Returns a sampler ID.
-spec make_sampler(map()) -> integer().
make_sampler(Desc) ->
    gen_server:call(?MODULE, {make_sampler, Desc}, 5000).

%% @doc Create a shader (compile GLSL). Returns a shader ID.
-spec make_shader(map()) -> integer().
make_shader(Desc) ->
    gen_server:call(?MODULE, {make_shader, Desc}, 5000).

%% @doc Create a pipeline (render state). Returns a pipeline ID.
-spec make_pipeline(map()) -> integer().
make_pipeline(Desc) ->
    gen_server:call(?MODULE, {make_pipeline, Desc}, 5000).

%% @doc Apply (bind) a pipeline for rendering.
-spec apply_pipeline(integer()) -> ok.
apply_pipeline(PipId) ->
    gen_server:cast(?MODULE, {apply_pipeline, PipId}).

%% @doc Apply vertex/index buffer bindings.
-spec apply_bindings(map()) -> ok.
apply_bindings(Bindings) ->
    gen_server:cast(?MODULE, {apply_bindings, Bindings}).

%% @doc Apply uniform data to a shader stage.
-spec apply_uniforms(atom(), integer(), binary() | map()) -> ok.
apply_uniforms(Stage, UbIndex, Data) ->
    gen_server:cast(?MODULE, {apply_uniforms, Stage, UbIndex, Data}).

%% @doc Issue a draw call.
-spec draw(integer(), integer(), integer(), integer()) -> ok.
draw(BaseElement, NumElements, NumInstances, _Reserved) ->
    gen_server:cast(?MODULE, {draw, BaseElement, NumElements, NumInstances}).

%% @doc Set the viewport rectangle.
-spec apply_viewport(number(), number(), number(), number(), number() | undefined) -> ok.
apply_viewport(X, Y, W, H, _OriginTopLeft) ->
    gen_server:cast(?MODULE, {apply_viewport, X, Y, W, H}).

%% @doc Set the scissor rectangle.
-spec apply_scissor_rect(number(), number(), number(), number(), number() | undefined) -> ok.
apply_scissor_rect(X, Y, W, H, _OriginTopLeft) ->
    gen_server:cast(?MODULE, {apply_scissor_rect, X, Y, W, H}).

%% @doc Destroy a buffer resource.
-spec destroy_buffer(integer()) -> ok.
destroy_buffer(Id) ->
    gen_server:cast(?MODULE, {destroy_buffer, Id}).

%% @doc Destroy an image resource.
-spec destroy_image(integer()) -> ok.
destroy_image(Id) ->
    gen_server:cast(?MODULE, {destroy_image, Id}).

%% @doc Destroy a sampler resource.
-spec destroy_sampler(integer()) -> ok.
destroy_sampler(Id) ->
    gen_server:cast(?MODULE, {destroy_sampler, Id}).

%% @doc Destroy a shader resource.
-spec destroy_shader(integer()) -> ok.
destroy_shader(Id) ->
    gen_server:cast(?MODULE, {destroy_shader, Id}).

%% @doc Destroy a pipeline resource.
-spec destroy_pipeline(integer()) -> ok.
destroy_pipeline(Id) ->
    gen_server:cast(?MODULE, {destroy_pipeline, Id}).

%% @doc Update buffer data.
-spec update_buffer(integer(), binary()) -> ok.
update_buffer(BufId, Data) ->
    gen_server:cast(?MODULE, {update_buffer, BufId, Data}).

%% @doc Append data to a buffer. Returns byte offset of appended data.
-spec append_buffer(integer(), binary()) -> integer().
append_buffer(BufId, Data) ->
    gen_server:call(?MODULE, {append_buffer, BufId, Data}, 5000).

%% @doc Update image data.
-spec update_image(integer(), binary()) -> ok.
update_image(_ImgId, _Data) ->
    ok.

%% @doc Query the rendering backend (always OpenGL for wx).
-spec query_backend() -> atom().
query_backend() -> gl.

-spec query_features() -> map().
query_features() -> #{}.

-spec query_limits() -> map().
query_limits() -> #{}.

%% ============================================================================
%% Public API - sgl (immediate mode rendering)
%%
%% These map directly to OpenGL immediate-mode calls for maximum performance.
%% No gen_server round-trip needed since gl calls go to the driver directly.
%% ============================================================================

%% @doc Setup SGL.
-spec sgl_setup(map()) -> ok.
sgl_setup(_Desc) -> ok.

%% @doc Shutdown SGL.
-spec sgl_shutdown() -> ok.
sgl_shutdown() -> ok.

%% @doc Query SGL error state.
-spec sgl_error() -> integer().
sgl_error() -> 0. %% no_error

%% Reset SGL state to defaults
-spec sgl_defaults() -> ok.
sgl_defaults() ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    ok.

%% Viewport and scissor
-spec sgl_viewport(number(), number(), number(), number(), term()) -> ok.
sgl_viewport(X, Y, W, H, _OriginTopLeft) ->
    gl:viewport(X, Y, W, H).

-spec sgl_scissor_rect(number(), number(), number(), number(), term()) -> ok.
sgl_scissor_rect(X, Y, W, H, _OriginTopLeft) ->
    gl:enable(?GL_SCISSOR_TEST),
    gl:scissor(X, Y, W, H).

%% Texture control
-spec sgl_enable_texture() -> ok.
sgl_enable_texture() ->
    gl:enable(?GL_TEXTURE_2D).

-spec sgl_disable_texture() -> ok.
sgl_disable_texture() ->
    gl:disable(?GL_TEXTURE_2D).

-spec sgl_texture(integer(), term()) -> ok.
sgl_texture(ImgId, _SamplerId) ->
    %% Bind the GL texture associated with this image ID
    case get({sokol_image, ImgId}) of
        undefined -> ok;
        GlTex -> gl:bindTexture(?GL_TEXTURE_2D, GlTex)
    end.

%% Begin primitive drawing
-spec sgl_begin_points() -> ok.
sgl_begin_points() -> gl:'begin'(?GL_POINTS).
-spec sgl_begin_lines() -> ok.
sgl_begin_lines() -> gl:'begin'(?GL_LINES).
-spec sgl_begin_line_strip() -> ok.
sgl_begin_line_strip() -> gl:'begin'(?GL_LINE_STRIP).
-spec sgl_begin_triangles() -> ok.
sgl_begin_triangles() -> gl:'begin'(?GL_TRIANGLES).
-spec sgl_begin_triangle_strip() -> ok.
sgl_begin_triangle_strip() -> gl:'begin'(?GL_TRIANGLE_STRIP).
-spec sgl_begin_quads() -> ok.
sgl_begin_quads() -> gl:'begin'(?GL_QUADS).

%% Vertex submission
-spec sgl_v2f(number(), number()) -> ok.
sgl_v2f(X, Y) -> gl:vertex2f(X, Y).
-spec sgl_v3f(number(), number(), number()) -> ok.
sgl_v3f(X, Y, Z) -> gl:vertex3f(X, Y, Z).

-spec sgl_v2f_t2f(number(), number(), number(), number()) -> ok.
sgl_v2f_t2f(X, Y, U, V) ->
    gl:texCoord2f(U, V),
    gl:vertex2f(X, Y).

-spec sgl_v3f_t2f(number(), number(), number(), number(), number()) -> ok.
sgl_v3f_t2f(X, Y, Z, U, V) ->
    gl:texCoord2f(U, V),
    gl:vertex3f(X, Y, Z).

-spec sgl_v2f_c3f(number(), number(), number(), number(), number()) -> ok.
sgl_v2f_c3f(X, Y, R, G, B) ->
    gl:color3f(R, G, B),
    gl:vertex2f(X, Y).

-spec sgl_v2f_c4f(number(), number(), number(), number(), number(), number()) -> ok.
sgl_v2f_c4f(X, Y, R, G, B, A) ->
    gl:color4f(R, G, B, A),
    gl:vertex2f(X, Y).

-spec sgl_v2f_c3b(number(), number(), number(), number(), number()) -> ok.
sgl_v2f_c3b(X, Y, R, G, B) ->
    gl:color3ub(R, G, B),
    gl:vertex2f(X, Y).

-spec sgl_v2f_c4b(number(), number(), number(), number(), number(), number()) -> ok.
sgl_v2f_c4b(X, Y, R, G, B, A) ->
    gl:color4ub(R, G, B, A),
    gl:vertex2f(X, Y).

-spec sgl_v2f_c1i(number(), number(), integer()) -> ok.
sgl_v2f_c1i(X, Y, RGBA) ->
    R = (RGBA bsr 24) band 16#FF,
    G = (RGBA bsr 16) band 16#FF,
    B = (RGBA bsr 8) band 16#FF,
    A = RGBA band 16#FF,
    gl:color4ub(R, G, B, A),
    gl:vertex2f(X, Y).

-spec sgl_v3f_c3f(number(), number(), number(), number(), number(), number()) -> ok.
sgl_v3f_c3f(X, Y, Z, R, G, B) ->
    gl:color3f(R, G, B),
    gl:vertex3f(X, Y, Z).

-spec sgl_v3f_c4f(number(), number(), number(), number(), number(), number(), number()) -> ok.
sgl_v3f_c4f(X, Y, Z, R, G, B, A) ->
    gl:color4f(R, G, B, A),
    gl:vertex3f(X, Y, Z).

-spec sgl_v3f_c3b(number(), number(), number(), number(), number(), number()) -> ok.
sgl_v3f_c3b(X, Y, Z, R, G, B) ->
    gl:color3ub(R, G, B),
    gl:vertex3f(X, Y, Z).

-spec sgl_v3f_c4b(number(), number(), number(), number(), number(), number(), number()) -> ok.
sgl_v3f_c4b(X, Y, Z, R, G, B, A) ->
    gl:color4ub(R, G, B, A),
    gl:vertex3f(X, Y, Z).

-spec sgl_v3f_c1i(number(), number(), number(), integer()) -> ok.
sgl_v3f_c1i(X, Y, Z, RGBA) ->
    R = (RGBA bsr 24) band 16#FF,
    G = (RGBA bsr 16) band 16#FF,
    B = (RGBA bsr 8) band 16#FF,
    A = RGBA band 16#FF,
    gl:color4ub(R, G, B, A),
    gl:vertex3f(X, Y, Z).

%% Color setting
-spec sgl_c3f(number(), number(), number()) -> ok.
sgl_c3f(R, G, B) -> gl:color3f(R, G, B).
-spec sgl_c4f(number(), number(), number(), number()) -> ok.
sgl_c4f(R, G, B, A) -> gl:color4f(R, G, B, A).
-spec sgl_c3b(integer(), integer(), integer()) -> ok.
sgl_c3b(R, G, B) -> gl:color3ub(R, G, B).
-spec sgl_c4b(integer(), integer(), integer(), integer()) -> ok.
sgl_c4b(R, G, B, A) -> gl:color4ub(R, G, B, A).
-spec sgl_c1i(integer()) -> ok.
sgl_c1i(RGBA) ->
    R = (RGBA bsr 24) band 16#FF,
    G = (RGBA bsr 16) band 16#FF,
    B = (RGBA bsr 8) band 16#FF,
    A = RGBA band 16#FF,
    gl:color4ub(R, G, B, A).

%% Texture coordinate setting
-spec sgl_t2f(number(), number()) -> ok.
sgl_t2f(U, V) -> gl:texCoord2f(U, V).

%% Point size
-spec sgl_point_size(number()) -> ok.
sgl_point_size(S) -> gl:pointSize(S).

%% End primitive and draw
-spec sgl_end() -> ok.
sgl_end() -> gl:'end'().
-spec sgl_draw() -> ok.
sgl_draw() -> ok.  %% Actual buffer swap happens in commit()
-spec sgl_context_draw(integer()) -> ok.
sgl_context_draw(_Ctx) -> ok.

%% Matrix operations
-spec sgl_matrix_mode_projection() -> ok.
sgl_matrix_mode_projection() -> gl:matrixMode(?GL_PROJECTION).
-spec sgl_matrix_mode_modelview() -> ok.
sgl_matrix_mode_modelview() -> gl:matrixMode(?GL_MODELVIEW).
-spec sgl_matrix_mode_texture() -> ok.
sgl_matrix_mode_texture() -> gl:matrixMode(?GL_TEXTURE).

-spec sgl_load_identity() -> ok.
sgl_load_identity() -> gl:loadIdentity().
-spec sgl_push_matrix() -> ok.
sgl_push_matrix() -> gl:pushMatrix().
-spec sgl_pop_matrix() -> ok.
sgl_pop_matrix() -> gl:popMatrix().

-spec sgl_load_matrix([number()]) -> ok.
sgl_load_matrix(M) when is_list(M), length(M) >= 16 ->
    %% M is a 16-element list (column-major 4x4 matrix)
    [M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15|_] = M,
    gl:loadMatrixf({M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15});
sgl_load_matrix(_) -> ok.

-spec sgl_load_transpose_matrix([number()]) -> ok.
sgl_load_transpose_matrix(M) when is_list(M), length(M) >= 16 ->
    [M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15|_] = M,
    gl:loadTransposeMatrixf({M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15});
sgl_load_transpose_matrix(_) -> ok.

-spec sgl_mult_matrix([number()]) -> ok.
sgl_mult_matrix(M) when is_list(M), length(M) >= 16 ->
    [M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15|_] = M,
    gl:multMatrixf({M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15});
sgl_mult_matrix(_) -> ok.

-spec sgl_mult_transpose_matrix([number()]) -> ok.
sgl_mult_transpose_matrix(M) when is_list(M), length(M) >= 16 ->
    [M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15|_] = M,
    gl:multTransposeMatrixf({M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15});
sgl_mult_transpose_matrix(_) -> ok.

-spec sgl_translate(number(), number(), number()) -> ok.
sgl_translate(X, Y, Z) -> gl:translatef(X, Y, Z).
-spec sgl_rotate(number(), number(), number(), number()) -> ok.
sgl_rotate(AngleRad, X, Y, Z) ->
    %% sokol_gl uses radians, OpenGL uses degrees
    gl:rotatef(AngleRad * 57.2957795131, X, Y, Z).
-spec sgl_scale(number(), number(), number()) -> ok.
sgl_scale(X, Y, Z) -> gl:scalef(X, Y, Z).

-spec sgl_ortho(number(), number(), number(), number(), number(), number()) -> ok.
sgl_ortho(L, R, B, T, N, F) -> gl:ortho(L, R, B, T, N, F).
-spec sgl_frustum(number(), number(), number(), number(), number(), number()) -> ok.
sgl_frustum(L, R, B, T, N, F) -> gl:frustum(L, R, B, T, N, F).
-spec sgl_perspective(number(), number(), number(), number()) -> ok.
sgl_perspective(FovY, Aspect, ZNear, ZFar) ->
    glu:perspective(FovY * 57.2957795131, Aspect, ZNear, ZFar).
-spec sgl_lookat(number(), number(), number(), number(), number(), number(), number(), number(), number()) -> ok.
sgl_lookat(EyeX, EyeY, EyeZ, CenterX, CenterY, CenterZ, UpX, UpY, UpZ) ->
    glu:lookAt(EyeX, EyeY, EyeZ, CenterX, CenterY, CenterZ, UpX, UpY, UpZ).

%% Pipeline stack (no-ops for immediate mode, pipeline state managed via GL directly)
-spec sgl_push_pipeline() -> ok.
sgl_push_pipeline() -> ok.
-spec sgl_pop_pipeline() -> ok.
sgl_pop_pipeline() -> ok.
-spec sgl_load_default_pipeline() -> ok.
sgl_load_default_pipeline() -> ok.
-spec sgl_load_pipeline(integer()) -> ok.
sgl_load_pipeline(_Pip) -> ok.

%% Context management (single-context for now)
-spec sgl_make_context(map()) -> integer().
sgl_make_context(_Desc) -> 1.
-spec sgl_destroy_context(integer()) -> ok.
sgl_destroy_context(_Ctx) -> ok.
-spec sgl_set_context(integer()) -> ok.
sgl_set_context(_Ctx) -> ok.
-spec sgl_get_context() -> integer().
sgl_get_context() -> 1.
-spec sgl_default_context() -> integer().
sgl_default_context() -> 1.

%% Pipeline management for SGL
-spec sgl_make_pipeline(map()) -> integer().
sgl_make_pipeline(_Desc) -> 1.
-spec sgl_destroy_pipeline(integer()) -> ok.
sgl_destroy_pipeline(_Pip) -> ok.

%% Utility
-spec sgl_rad(number()) -> float().
sgl_rad(Deg) -> Deg * 0.01745329252.
 -spec sgl_deg(number()) -> float().
sgl_deg(Rad) -> Rad * 57.2957795131.

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

%% @doc Initialize sokol gen_server process.
-spec init([map()]) -> {ok, #state{}}.
init([Desc]) ->
    Wx = wx:new(),
    Title = maps:get(window_title, Desc, "V Application"),
    W = maps:get(width, Desc, 800),
    H = maps:get(height, Desc, 600),

    Frame = wxFrame:new(Wx, -1, Title, [{size, {W, H}}]),

    %% Create OpenGL canvas with double-buffering and depth buffer
    GLAttrs = [{attribList, [?WX_GL_RGBA,
                             ?WX_GL_DOUBLEBUFFER,
                             ?WX_GL_DEPTH_SIZE, 16,
                             0]}],
    Canvas = wxGLCanvas:new(Frame, GLAttrs),
    Ctx = wxGLContext:new(Canvas),
    wxGLCanvas:setCurrent(Canvas, Ctx),

    %% Subscribe to window and input events
    wxFrame:connect(Frame, close_window),
    wxGLCanvas:connect(Canvas, paint, [{skip, true}]),
    wxGLCanvas:connect(Canvas, size),
    wxGLCanvas:connect(Canvas, key_down),
    wxGLCanvas:connect(Canvas, key_up),
    wxGLCanvas:connect(Canvas, char),
    wxGLCanvas:connect(Canvas, left_down),
    wxGLCanvas:connect(Canvas, left_up),
    wxGLCanvas:connect(Canvas, right_down),
    wxGLCanvas:connect(Canvas, right_up),
    wxGLCanvas:connect(Canvas, middle_down),
    wxGLCanvas:connect(Canvas, middle_up),
    wxGLCanvas:connect(Canvas, motion),
    wxGLCanvas:connect(Canvas, mousewheel),
    wxGLCanvas:connect(Canvas, enter_window),
    wxGLCanvas:connect(Canvas, leave_window),

    wxFrame:show(Frame),

    %% Initialize default OpenGL state
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:viewport(0, 0, W, H),

    %% Call V's init callback
    InitCb = maps:get(init_cb, Desc, undefined),
    safe_callback(InitCb),

    %% Start render timer (~60fps)
    Timer = erlang:send_after(?FRAME_INTERVAL_MS, self(), render_frame),

    State = #state{
        wx = Wx,
        frame = Frame,
        canvas = Canvas,
        gl_ctx = Ctx,
        width = W,
        height = H,
        title = Title,
        init_cb = InitCb,
        frame_cb = maps:get(frame_cb, Desc, undefined),
        cleanup_cb = maps:get(cleanup_cb, Desc, undefined),
        event_cb = maps:get(event_cb, Desc, undefined),
        timer_ref = Timer,
        frame_start = erlang:monotonic_time(microsecond)
    },
    {ok, State}.

%% --- Synchronous calls ---

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call(width, _From, State) ->
    {reply, State#state.width, State};

handle_call(height, _From, State) ->
    {reply, State#state.height, State};

handle_call(frame_count, _From, State) ->
    {reply, State#state.frame_count, State};

handle_call(frame_duration, _From, State) ->
    {reply, State#state.frame_duration, State};

handle_call(isvalid, _From, State) ->
    {reply, State#state.canvas =/= undefined, State};

handle_call(dpi_scale, _From, #state{canvas = Canvas} = State) ->
    Scale = try wxGLCanvas:getContentScaleFactor(Canvas)
            catch _:_ -> 1.0
            end,
    {reply, Scale, State};

handle_call(is_fullscreen, _From, State) ->
    {reply, State#state.fullscreen, State};

%% --- make_buffer ---
handle_call({make_buffer, Desc}, _From, State) ->
    [BufId] = gl:genBuffers(1),
    Data = maps:get(data, Desc, <<>>),
    BufType = case maps:get(type, Desc, vertex) of
        index -> ?GL_ELEMENT_ARRAY_BUFFER;
        _ -> ?GL_ARRAY_BUFFER
    end,
    Usage = case maps:get(usage, Desc, immutable) of
        immutable -> ?GL_STATIC_DRAW;
        dynamic -> ?GL_DYNAMIC_DRAW;
        stream -> ?GL_STREAM_DRAW;
        _ -> ?GL_STATIC_DRAW
    end,
    DataBin = ensure_binary(Data),
    gl:bindBuffer(BufType, BufId),
    gl:bufferData(BufType, byte_size(DataBin), DataBin, Usage),
    gl:bindBuffer(BufType, 0),
    Id = State#state.next_id,
    Buffers = maps:put(Id, #{gl_id => BufId, type => BufType, usage => Usage,
                              size => byte_size(DataBin)}, State#state.buffers),
    {reply, Id, State#state{buffers = Buffers, next_id = Id + 1}};

%% --- make_image ---
handle_call({make_image, Desc}, _From, State) ->
    [TexId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TexId),
    Width = maps:get(width, Desc, 1),
    Height = maps:get(height, Desc, 1),
    %% Set default texture parameters
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP_TO_EDGE),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP_TO_EDGE),
    %% Upload pixel data if provided
    PixelData = maps:get(pixel_data, Desc, <<>>),
    PixelBin = ensure_binary(PixelData),
    case byte_size(PixelBin) of
        0 ->
            %% Allocate empty texture
            gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, Width, Height, 0,
                          ?GL_RGBA, ?GL_UNSIGNED_BYTE, 0);
        _ ->
            gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, Width, Height, 0,
                          ?GL_RGBA, ?GL_UNSIGNED_BYTE, PixelBin)
    end,
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    Id = State#state.next_id,
    %% Store mapping in process dictionary for sgl_texture fast access
    put({sokol_image, Id}, TexId),
    Images = maps:put(Id, #{gl_id => TexId, width => Width, height => Height},
                      State#state.images),
    {reply, Id, State#state{images = Images, next_id = Id + 1}};

%% --- make_sampler ---
handle_call({make_sampler, Desc}, _From, State) ->
    Id = State#state.next_id,
    Samplers = maps:put(Id, Desc, State#state.samplers),
    {reply, Id, State#state{samplers = Samplers, next_id = Id + 1}};

%% --- make_shader ---
handle_call({make_shader, Desc}, _From, State) ->
    VsSrc = maps:get(vs_source, Desc, ""),
    FsSrc = maps:get(fs_source, Desc, ""),
    Program = compile_shader_program(VsSrc, FsSrc),
    Id = State#state.next_id,
    Shaders = maps:put(Id, #{gl_id => Program, desc => Desc}, State#state.shaders),
    {reply, Id, State#state{shaders = Shaders, next_id = Id + 1}};

%% --- make_pipeline ---
handle_call({make_pipeline, Desc}, _From, State) ->
    Id = State#state.next_id,
    Pipelines = maps:put(Id, Desc, State#state.pipelines),
    {reply, Id, State#state{pipelines = Pipelines, next_id = Id + 1}};

%% --- append_buffer ---
handle_call({append_buffer, BufId, Data}, _From, State) ->
    case maps:find(BufId, State#state.buffers) of
        {ok, #{gl_id := GlBuf, type := BufType}} ->
            DataBin = ensure_binary(Data),
            gl:bindBuffer(BufType, GlBuf),
            %% Use glBufferSubData to append
            %% For simplicity, append at offset 0 (like update)
            gl:bufferSubData(BufType, 0, byte_size(DataBin), DataBin),
            gl:bindBuffer(BufType, 0),
            {reply, 0, State};
        error ->
            {reply, 0, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% --- Asynchronous casts ---

-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.
handle_cast(quit, State) ->
    {stop, normal, State};

handle_cast(request_quit, State) ->
    fire_event(State#state.event_cb, #{type => quit_requested}),
    {noreply, State#state{quit_requested = true}};

handle_cast(cancel_quit, State) ->
    {noreply, State#state{quit_cancelled = true}};

handle_cast(toggle_fullscreen, #state{frame = Frame, fullscreen = FS} = State) ->
    NewFS = not FS,
    Style = case NewFS of
        true -> wxFrame:showFullScreen(Frame, true), true;
        false -> wxFrame:showFullScreen(Frame, false), false
    end,
    {noreply, State#state{fullscreen = Style}};

%% --- begin_pass/end_pass/commit are now direct calls (see public API) ---
%% Kept as fallback for external callers not in gen_server process
handle_cast(commit, #state{canvas = Canvas} = State) ->
    wxGLCanvas:swapBuffers(Canvas),
    {noreply, State};

%% --- apply_pipeline ---
handle_cast({apply_pipeline, PipId}, State) ->
    case maps:find(PipId, State#state.pipelines) of
        {ok, PipDesc} ->
            %% Apply shader if specified
            ShaderId = maps:get(shader, PipDesc, undefined),
            case ShaderId of
                undefined -> ok;
                _ ->
                    case maps:find(ShaderId, State#state.shaders) of
                        {ok, #{gl_id := Program}} -> gl:useProgram(Program);
                        error -> ok
                    end
            end,
            %% Apply depth state
            case maps:get(depth, PipDesc, #{}) of
                #{write_enabled := true} -> gl:enable(?GL_DEPTH_TEST);
                _ -> ok
            end,
            %% Apply cull mode
            case maps:get(cull_mode, PipDesc, none) of
                none -> gl:disable(?GL_CULL_FACE);
                front -> gl:enable(?GL_CULL_FACE), gl:cullFace(?GL_FRONT);
                back -> gl:enable(?GL_CULL_FACE), gl:cullFace(?GL_BACK)
            end,
            %% Apply blend state from colors
            apply_blend_state(maps:get(colors, PipDesc, [])),
            {noreply, State#state{current_pipeline = PipId}};
        error ->
            {noreply, State}
    end;

%% --- apply_bindings ---
handle_cast({apply_bindings, Bindings}, State) ->
    %% Bind vertex buffers
    VBufs = maps:get(vertex_buffers, Bindings, []),
    bind_vertex_buffers(VBufs, State#state.buffers, 0),
    %% Bind index buffer
    case maps:get(index_buffer, Bindings, undefined) of
        undefined -> ok;
        IBufId ->
            case maps:find(IBufId, State#state.buffers) of
                {ok, #{gl_id := GlBuf}} ->
                    gl:bindBuffer(?GL_ELEMENT_ARRAY_BUFFER, GlBuf);
                error -> ok
            end
    end,
    %% Bind textures
    bind_images(maps:get(vs, Bindings, #{}), maps:get(fs, Bindings, #{}), State),
    {noreply, State#state{current_bindings = Bindings}};

%% --- apply_uniforms ---
handle_cast({apply_uniforms, _Stage, _UbIndex, Data}, State) ->
    %% For modern GL, uniforms are uploaded via glUniform* calls.
    %% The Data binary/map is interpreted based on the current pipeline's shader.
    case State#state.current_pipeline of
        undefined -> ok;
        PipId ->
            case maps:find(PipId, State#state.pipelines) of
                {ok, PipDesc} ->
                    ShaderId = maps:get(shader, PipDesc, undefined),
                    case ShaderId of
                        undefined -> ok;
                        _ ->
                            case maps:find(ShaderId, State#state.shaders) of
                                {ok, #{gl_id := Program}} ->
                                    upload_uniforms(Program, Data);
                                error -> ok
                            end
                    end;
                error -> ok
            end
    end,
    {noreply, State};

%% --- draw ---
handle_cast({draw, BaseElement, NumElements, NumInstances}, State) ->
    case NumInstances > 1 of
        true ->
            gl:drawArraysInstanced(?GL_TRIANGLES, BaseElement, NumElements, NumInstances);
        false ->
            gl:drawArrays(?GL_TRIANGLES, BaseElement, NumElements)
    end,
    {noreply, State};

%% --- apply_viewport ---
handle_cast({apply_viewport, X, Y, W, H}, State) ->
    gl:viewport(X, Y, W, H),
    {noreply, State};

%% --- apply_scissor_rect ---
handle_cast({apply_scissor_rect, X, Y, W, H}, State) ->
    gl:enable(?GL_SCISSOR_TEST),
    gl:scissor(X, Y, W, H),
    {noreply, State};

%% --- destroy_buffer ---
handle_cast({destroy_buffer, Id}, State) ->
    case maps:find(Id, State#state.buffers) of
        {ok, #{gl_id := GlBuf}} ->
            gl:deleteBuffers(1, [GlBuf]),
            {noreply, State#state{buffers = maps:remove(Id, State#state.buffers)}};
        error ->
            {noreply, State}
    end;

%% --- destroy_image ---
handle_cast({destroy_image, Id}, State) ->
    case maps:find(Id, State#state.images) of
        {ok, #{gl_id := TexId}} ->
            gl:deleteTextures(1, [TexId]),
            erase({sokol_image, Id}),
            {noreply, State#state{images = maps:remove(Id, State#state.images)}};
        error ->
            {noreply, State}
    end;

%% --- destroy_sampler ---
handle_cast({destroy_sampler, Id}, State) ->
    {noreply, State#state{samplers = maps:remove(Id, State#state.samplers)}};

%% --- destroy_shader ---
handle_cast({destroy_shader, Id}, State) ->
    case maps:find(Id, State#state.shaders) of
        {ok, #{gl_id := Program}} ->
            gl:deleteProgram(Program),
            {noreply, State#state{shaders = maps:remove(Id, State#state.shaders)}};
        error ->
            {noreply, State}
    end;

%% --- destroy_pipeline ---
handle_cast({destroy_pipeline, Id}, State) ->
    {noreply, State#state{pipelines = maps:remove(Id, State#state.pipelines)}};

%% --- update_buffer ---
handle_cast({update_buffer, BufId, Data}, State) ->
    case maps:find(BufId, State#state.buffers) of
        {ok, #{gl_id := GlBuf, type := BufType}} ->
            DataBin = ensure_binary(Data),
            gl:bindBuffer(BufType, GlBuf),
            gl:bufferSubData(BufType, 0, byte_size(DataBin), DataBin),
            gl:bindBuffer(BufType, 0);
        error -> ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --- Info messages (events, render timer) ---

%% Render frame timer
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.
handle_info(render_frame, #state{canvas = Canvas, gl_ctx = Ctx,
                                  frame_cb = FrameCb} = State) ->
    wxGLCanvas:setCurrent(Canvas, Ctx),
    %% Store canvas in pdict so commit() can swap buffers directly
    put(vbeam_sokol_canvas, Canvas),
    Now = erlang:monotonic_time(microsecond),
    Duration = (Now - State#state.frame_start) / 1000000.0,

    %% Call V's frame callback
    safe_callback(FrameCb),

    %% Schedule next frame
    Timer = erlang:send_after(?FRAME_INTERVAL_MS, self(), render_frame),
    {noreply, State#state{
        frame_count = State#state.frame_count + 1,
        frame_duration = Duration,
        frame_start = Now,
        timer_ref = Timer
    }};

%% --- Window close ---
handle_info(#wx{event = #wxClose{}}, State) ->
    safe_callback(State#state.cleanup_cb),
    {stop, normal, State};

%% --- Paint event ---
handle_info(#wx{event = #wxPaint{}}, #state{canvas = Canvas} = State) ->
    %% Must create DC even if we don't use it, to validate the window
    DC = wxPaintDC:new(Canvas),
    wxPaintDC:destroy(DC),
    {noreply, State};

%% --- Resize ---
handle_info(#wx{event = #wxSize{size = {W, H}}}, State) ->
    gl:viewport(0, 0, W, H),
    fire_event(State#state.event_cb, #{
        type => resized,
        frame_width => W,
        frame_height => H
    }),
    {noreply, State#state{width = W, height = H}};

%% --- Key events ---
handle_info(#wx{event = #wxKey{type = key_down, keyCode = Key,
                                rawCode = _Raw, rawFlags = _Flags}}, State) ->
    fire_event(State#state.event_cb, #{
        type => key_down,
        key_code => wx_keycode_to_sokol(Key)
    }),
    {noreply, State};

handle_info(#wx{event = #wxKey{type = key_up, keyCode = Key}}, State) ->
    fire_event(State#state.event_cb, #{
        type => key_up,
        key_code => wx_keycode_to_sokol(Key)
    }),
    {noreply, State};

handle_info(#wx{event = #wxKey{type = char, keyCode = Key}}, State) ->
    fire_event(State#state.event_cb, #{
        type => char,
        char_code => Key
    }),
    {noreply, State};

%% --- Mouse button events ---
handle_info(#wx{event = #wxMouse{type = left_down, x = X, y = Y}}, State) ->
    fire_event(State#state.event_cb, #{
        type => mouse_down, mouse_button => 0,
        mouse_x => float(X), mouse_y => float(Y)
    }),
    {noreply, State};

handle_info(#wx{event = #wxMouse{type = left_up, x = X, y = Y}}, State) ->
    fire_event(State#state.event_cb, #{
        type => mouse_up, mouse_button => 0,
        mouse_x => float(X), mouse_y => float(Y)
    }),
    {noreply, State};

handle_info(#wx{event = #wxMouse{type = right_down, x = X, y = Y}}, State) ->
    fire_event(State#state.event_cb, #{
        type => mouse_down, mouse_button => 1,
        mouse_x => float(X), mouse_y => float(Y)
    }),
    {noreply, State};

handle_info(#wx{event = #wxMouse{type = right_up, x = X, y = Y}}, State) ->
    fire_event(State#state.event_cb, #{
        type => mouse_up, mouse_button => 1,
        mouse_x => float(X), mouse_y => float(Y)
    }),
    {noreply, State};

handle_info(#wx{event = #wxMouse{type = middle_down, x = X, y = Y}}, State) ->
    fire_event(State#state.event_cb, #{
        type => mouse_down, mouse_button => 2,
        mouse_x => float(X), mouse_y => float(Y)
    }),
    {noreply, State};

handle_info(#wx{event = #wxMouse{type = middle_up, x = X, y = Y}}, State) ->
    fire_event(State#state.event_cb, #{
        type => mouse_up, mouse_button => 2,
        mouse_x => float(X), mouse_y => float(Y)
    }),
    {noreply, State};

%% --- Mouse motion ---
handle_info(#wx{event = #wxMouse{type = motion, x = X, y = Y}}, State) ->
    fire_event(State#state.event_cb, #{
        type => mouse_move,
        mouse_x => float(X), mouse_y => float(Y)
    }),
    {noreply, State};

%% --- Mouse scroll ---
handle_info(#wx{event = #wxMouse{type = mousewheel, wheelRotation = Rot,
                                  wheelDelta = Delta}}, State) ->
    ScrollY = case Delta of
        0 -> 0.0;
        _ -> float(Rot) / float(Delta)
    end,
    fire_event(State#state.event_cb, #{
        type => mouse_scroll,
        scroll_x => 0.0, scroll_y => ScrollY
    }),
    {noreply, State};

%% --- Mouse enter/leave ---
handle_info(#wx{event = #wxMouse{type = enter_window}}, State) ->
    fire_event(State#state.event_cb, #{type => mouse_enter}),
    {noreply, State};

handle_info(#wx{event = #wxMouse{type = leave_window}}, State) ->
    fire_event(State#state.event_cb, #{type => mouse_leave}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc gen_server callback: cleanup.
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{cleanup_cb = CleanupCb, timer_ref = Timer,
                           frame = Frame, wx = Wx}) ->
    %% Cancel render timer
    case Timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    %% Call cleanup callback
    safe_callback(CleanupCb),
    %% Destroy wx resources
    catch wxFrame:destroy(Frame),
    catch wx:destroy(Wx),
    ok.

%% ============================================================================
%% Internal helpers
%% ============================================================================

%% Safely invoke a callback function, catching errors
safe_callback(undefined) -> ok;
safe_callback(Fun) when is_function(Fun, 0) ->
    try Fun()
    catch
        Class:Reason:Stack ->
            io:format("vbeam_sokol: callback error ~p:~p~n~p~n",
                      [Class, Reason, Stack])
    end;
safe_callback(_) -> ok.

%% Fire an event to the V event callback
fire_event(undefined, _Event) -> ok;
fire_event(EventCb, Event) when is_function(EventCb, 1) ->
    try EventCb(Event)
    catch
        Class:Reason:Stack ->
            io:format("vbeam_sokol: event callback error ~p:~p~n~p~n",
                      [Class, Reason, Stack])
    end;
fire_event(_, _) -> ok.

%% Ensure value is a binary
ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(List) when is_list(List) -> list_to_binary(List);
ensure_binary(_) -> <<>>.

%% Convert various types to float
to_float(F) when is_float(F) -> F;
to_float(I) when is_integer(I) -> float(I);
to_float(_) -> 0.0.

%% Compile a GLSL shader program from vertex + fragment source
compile_shader_program(VsSrc, FsSrc) ->
    VsSrcStr = to_string(VsSrc),
    FsSrcStr = to_string(FsSrc),
    case VsSrcStr of
        "" ->
            %% No shader source — return a dummy program ID
            0;
        _ ->
            Vs = gl:createShader(?GL_VERTEX_SHADER),
            gl:shaderSource(Vs, [VsSrcStr]),
            gl:compileShader(Vs),
            check_shader_compile(Vs, "vertex"),

            Fs = gl:createShader(?GL_FRAGMENT_SHADER),
            gl:shaderSource(Fs, [FsSrcStr]),
            gl:compileShader(Fs),
            check_shader_compile(Fs, "fragment"),

            Program = gl:createProgram(),
            gl:attachShader(Program, Vs),
            gl:attachShader(Program, Fs),
            gl:linkProgram(Program),
            check_program_link(Program),

            gl:deleteShader(Vs),
            gl:deleteShader(Fs),
            Program
    end.

%% Check shader compilation status
check_shader_compile(Shader, Type) ->
    case gl:getShaderiv(Shader, ?GL_COMPILE_STATUS) of
        ?GL_TRUE -> ok;
        _ ->
            InfoLog = gl:getShaderInfoLog(Shader, 4096),
            io:format("vbeam_sokol: ~s shader compilation failed:~n~s~n",
                      [Type, InfoLog])
    end.

%% Check program link status
check_program_link(Program) ->
    case gl:getProgramiv(Program, ?GL_LINK_STATUS) of
        ?GL_TRUE -> ok;
        _ ->
            InfoLog = gl:getProgramInfoLog(Program, 4096),
            io:format("vbeam_sokol: shader program link failed:~n~s~n",
                      [InfoLog])
    end.

%% Convert to string (handles binary, list, char pointer)
to_string(S) when is_binary(S) -> binary_to_list(S);
to_string(S) when is_list(S) -> S;
to_string(_) -> "".

%% Upload uniforms to the current shader program
upload_uniforms(Program, Data) when is_map(Data) ->
    %% Map of uniform_name => value
    maps:foreach(fun(Name, Value) ->
        NameStr = to_string(Name),
        Loc = gl:getUniformLocation(Program, NameStr),
        case Loc of
            -1 -> ok; %% Uniform not found
            _ -> set_uniform(Loc, Value)
        end
    end, Data);
upload_uniforms(_Program, _Data) ->
    %% Binary uniform data — would need layout info to interpret
    ok.

%% Set a uniform value based on its type
set_uniform(Loc, V) when is_float(V) ->
    gl:uniform1f(Loc, V);
set_uniform(Loc, V) when is_integer(V) ->
    gl:uniform1i(Loc, V);
set_uniform(Loc, {X, Y}) ->
    gl:uniform2f(Loc, to_float(X), to_float(Y));
set_uniform(Loc, {X, Y, Z}) ->
    gl:uniform3f(Loc, to_float(X), to_float(Y), to_float(Z));
set_uniform(Loc, {X, Y, Z, W}) ->
    gl:uniform4f(Loc, to_float(X), to_float(Y), to_float(Z), to_float(W));
set_uniform(Loc, M) when is_list(M), length(M) =:= 16 ->
    [M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15] = M,
    gl:uniformMatrix4fv(Loc, 1, ?GL_FALSE,
        [{M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15}]);
set_uniform(_Loc, _) ->
    ok.

%% Bind vertex buffers from binding list
bind_vertex_buffers([], _AllBufs, _Idx) -> ok;
bind_vertex_buffers([BufId | Rest], AllBufs, Idx) ->
    case maps:find(BufId, AllBufs) of
        {ok, #{gl_id := GlBuf}} ->
            gl:bindBuffer(?GL_ARRAY_BUFFER, GlBuf),
            %% Enable vertex attrib for this buffer index
            gl:enableVertexAttribArray(Idx),
            %% Default: assume float vertices, stride 0, offset 0
            %% Real applications would configure this via pipeline layout
            gl:vertexAttribPointer(Idx, 3, ?GL_FLOAT, ?GL_FALSE, 0, 0);
        error -> ok
    end,
    bind_vertex_buffers(Rest, AllBufs, Idx + 1).

%% Bind images (textures) from VS and FS bindings
bind_images(VsBindings, FsBindings, State) ->
    VsImages = maps:get(images, VsBindings, []),
    FsImages = maps:get(images, FsBindings, []),
    bind_image_list(VsImages, State#state.images, 0),
    bind_image_list(FsImages, State#state.images, length(VsImages)).

bind_image_list([], _AllImages, _TexUnit) -> ok;
bind_image_list([ImgId | Rest], AllImages, TexUnit) ->
    case maps:find(ImgId, AllImages) of
        {ok, #{gl_id := TexId}} ->
            gl:activeTexture(?GL_TEXTURE0 + TexUnit),
            gl:bindTexture(?GL_TEXTURE_2D, TexId);
        error -> ok
    end,
    bind_image_list(Rest, AllImages, TexUnit + 1).

%% Apply blend state from pipeline color targets
apply_blend_state([]) -> ok;
apply_blend_state([First | _]) ->
    Blend = maps:get(blend, First, #{}),
    case maps:get(enabled, Blend, false) of
        true ->
            gl:enable(?GL_BLEND),
            SrcRGB = blend_factor(maps:get(src_factor_rgb, Blend, one)),
            DstRGB = blend_factor(maps:get(dst_factor_rgb, Blend, zero)),
            SrcAlpha = blend_factor(maps:get(src_factor_alpha, Blend, one)),
            DstAlpha = blend_factor(maps:get(dst_factor_alpha, Blend, zero)),
            gl:blendFuncSeparate(SrcRGB, DstRGB, SrcAlpha, DstAlpha);
        false ->
            ok
    end.

%% Convert sokol blend factor to GL constant
blend_factor(zero) -> ?GL_ZERO;
blend_factor(one) -> ?GL_ONE;
blend_factor(src_color) -> ?GL_SRC_COLOR;
blend_factor(one_minus_src_color) -> ?GL_ONE_MINUS_SRC_COLOR;
blend_factor(src_alpha) -> ?GL_SRC_ALPHA;
blend_factor(one_minus_src_alpha) -> ?GL_ONE_MINUS_SRC_ALPHA;
blend_factor(dst_color) -> ?GL_DST_COLOR;
blend_factor(one_minus_dst_color) -> ?GL_ONE_MINUS_DST_COLOR;
blend_factor(dst_alpha) -> ?GL_DST_ALPHA;
blend_factor(one_minus_dst_alpha) -> ?GL_ONE_MINUS_DST_ALPHA;
blend_factor(_) -> ?GL_ONE.

%% Map wx keycodes to sokol keycodes (integer values matching V's KeyCode enum)
wx_keycode_to_sokol(Key) ->
    case Key of
        ?WXK_SPACE -> 32;
        ?WXK_ESCAPE -> 256;
        ?WXK_RETURN -> 257;
        ?WXK_TAB -> 258;
        ?WXK_BACK -> 259;        %% backspace
        ?WXK_INSERT -> 260;
        ?WXK_DELETE -> 261;
        ?WXK_RIGHT -> 262;
        ?WXK_LEFT -> 263;
        ?WXK_DOWN -> 264;
        ?WXK_UP -> 265;
        ?WXK_PAGEUP -> 266;
        ?WXK_PAGEDOWN -> 267;
        ?WXK_HOME -> 268;
        ?WXK_END -> 269;
        ?WXK_CAPITAL -> 280;     %% caps_lock
        ?WXK_SCROLL -> 281;      %% scroll_lock
        ?WXK_NUMLOCK -> 282;
        ?WXK_PRINT -> 283;       %% print_screen
        ?WXK_PAUSE -> 284;
        ?WXK_F1 -> 290;
        ?WXK_F2 -> 291;
        ?WXK_F3 -> 292;
        ?WXK_F4 -> 293;
        ?WXK_F5 -> 294;
        ?WXK_F6 -> 295;
        ?WXK_F7 -> 296;
        ?WXK_F8 -> 297;
        ?WXK_F9 -> 298;
        ?WXK_F10 -> 299;
        ?WXK_F11 -> 300;
        ?WXK_F12 -> 301;
        ?WXK_NUMPAD0 -> 320;
        ?WXK_NUMPAD1 -> 321;
        ?WXK_NUMPAD2 -> 322;
        ?WXK_NUMPAD3 -> 323;
        ?WXK_NUMPAD4 -> 324;
        ?WXK_NUMPAD5 -> 325;
        ?WXK_NUMPAD6 -> 326;
        ?WXK_NUMPAD7 -> 327;
        ?WXK_NUMPAD8 -> 328;
        ?WXK_NUMPAD9 -> 329;
        ?WXK_NUMPAD_DECIMAL -> 330;
        ?WXK_NUMPAD_DIVIDE -> 331;
        ?WXK_NUMPAD_MULTIPLY -> 332;
        ?WXK_NUMPAD_SUBTRACT -> 333;
        ?WXK_NUMPAD_ADD -> 334;
        ?WXK_NUMPAD_ENTER -> 335;
        ?WXK_SHIFT -> 340;       %% left_shift
        ?WXK_CONTROL -> 341;     %% left_control
        ?WXK_ALT -> 342;         %% left_alt
        ?WXK_WINDOWS_LEFT -> 343; %% left_super
        ?WXK_MENU -> 348;
        %% ASCII characters map directly (A=65, etc.)
        K when K >= 65, K =< 90 -> K;  %% A-Z
        K when K >= 48, K =< 57 -> K;  %% 0-9
        K when K >= 32, K =< 126 -> K; %% printable ASCII
        _ -> 0  %% invalid
    end.
