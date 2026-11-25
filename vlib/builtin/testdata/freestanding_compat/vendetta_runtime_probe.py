"""Exercise Vendetta playback in a private Xvfb display using a silent local clip."""
import argparse
import ctypes
import json
import os
from pathlib import Path
import selectors
import signal
import socket
import struct
import subprocess
import tempfile
import time
import zlib

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('binary', type=Path)
parser.add_argument('--project', required=True, type=Path)
parser.add_argument('--libc-free', action='store_true')
parser.add_argument('--file-dialog', action='store_true')
parser.add_argument('--audio-output', default='null')
parser.add_argument('--renderer', choices=['softpipe', 'llvmpipe'], default='llvmpipe')
parser.add_argument('--output', required=True, type=Path)
args = parser.parse_args()


def close_window(display, window):
    # Xvfb has no window manager to translate _NET_CLOSE_WINDOW. Send the
    # same WM_DELETE_WINDOW client message directly, preserving app cleanup.
    class Message(ctypes.Structure):
        _fields_ = [('type', ctypes.c_int), ('serial', ctypes.c_ulong),
            ('send_event', ctypes.c_int), ('display', ctypes.c_void_p),
            ('window', ctypes.c_ulong), ('message_type', ctypes.c_ulong),
            ('format', ctypes.c_int), ('data', ctypes.c_long * 5)]

    class Event(ctypes.Union):
        _fields_ = [('message', Message), ('padding', ctypes.c_long * 24)]

    x11 = ctypes.CDLL('libX11.so.6')
    x11.XOpenDisplay.argtypes = [ctypes.c_char_p]
    x11.XOpenDisplay.restype = ctypes.c_void_p
    x11.XInternAtom.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_int]
    x11.XInternAtom.restype = ctypes.c_ulong
    x11.XSendEvent.argtypes = [ctypes.c_void_p, ctypes.c_ulong, ctypes.c_int,
        ctypes.c_long, ctypes.POINTER(Event)]
    x11.XCloseDisplay.argtypes = [ctypes.c_void_p]
    connection = x11.XOpenDisplay(display.encode())
    assert connection, 'could not connect close request to private display'
    try:
        event = Event()
        event.message.type = 33  # ClientMessage
        event.message.display = connection
        event.message.window = int(window)
        event.message.message_type = x11.XInternAtom(connection, b'WM_PROTOCOLS', 0)
        event.message.format = 32
        event.message.data[0] = x11.XInternAtom(connection, b'WM_DELETE_WINDOW', 0)
        assert x11.XSendEvent(connection, int(window), 0, 0, ctypes.byref(event))
    finally:
        x11.XCloseDisplay(connection)


def png(path, pixels, width, height):
    def chunk(kind, data):
        return struct.pack('!I', len(data)) + kind + data + struct.pack('!I', zlib.crc32(kind + data))
    rows = b''.join(b'\0' + pixels[y * width * 3:(y + 1) * width * 3] for y in range(height))
    path.write_bytes(b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('!IIBBBBB', width, height, 8, 2, 0, 0, 0))
        + chunk(b'IDAT', zlib.compress(rows)) + chunk(b'IEND', b''))


with tempfile.TemporaryDirectory(prefix='vendetta-runtime-') as temporary:
    root = Path(temporary)
    environment = os.environ.copy()
    environment.pop('LD_PRELOAD', None)
    environment.pop('DBUS_SESSION_BUS_ADDRESS', None)
    environment.pop('WAYLAND_DISPLAY', None)
    for name in ('CONFIG', 'CACHE', 'DATA', 'STATE'):
        directory = root / name.lower()
        directory.mkdir(mode=0o700)
        environment[f'XDG_{name}_HOME'] = str(directory)
    (root / 'runtime').mkdir(mode=0o700)
    environment['XDG_RUNTIME_DIR'] = str(root / 'runtime')
    # Existing settings must never be migrated into this test's configuration.
    (root / 'config/vendetta').mkdir()
    (root / 'config/vendetta/settings.json').write_text(json.dumps({'open_mode': 'play'}))
    environment.update(LIBGL_ALWAYS_SOFTWARE='1', GALLIUM_DRIVER=args.renderer,
        LP_NUM_THREADS='1', OMP_NUM_THREADS='1', GSK_RENDERER='cairo', GTK_A11Y='none',
        GSETTINGS_BACKEND='memory', NO_AT_BRIDGE='1', GIO_USE_VFS='local', GDK_BACKEND='x11')
    server = player = bus = clipboard = None
    ipc = None
    channel = None
    log = (root / 'player.log').open('w+')
    server_log = (root / 'xvfb.log').open('w+')
    bus_log = (root / 'dbus.log').open('w+')

    def checked(command, **options):
        result = subprocess.run(list(map(str, command)), env=environment,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=25, **options)
        assert result.returncode == 0, (command, result.returncode, result.stderr.decode(errors='replace'))
        return result.stdout

    try:
        media = root / 'colors.mkv'
        checked(['ffmpeg', '-nostdin', '-v', 'error', '-filter_threads', '1',
            '-f', 'lavfi', '-i', 'testsrc2=size=320x180:rate=12,hue=h=90*t',
            '-f', 'lavfi', '-i', 'anullsrc=channel_layout=stereo:sample_rate=48000',
            '-t', '4', '-c:v', 'ffv1', '-level', '3', '-g', '1', '-c:a', 'pcm_s16le',
            '-threads', '1', media])
        read_fd, write_fd = os.pipe()
        try:
            server = subprocess.Popen(['Xvfb', '-displayfd', str(write_fd), '-screen', '0',
                '1280x800x24', '-nolisten', 'tcp'], pass_fds=[write_fd],
                stdout=server_log, stderr=subprocess.STDOUT, env=environment, start_new_session=True)
        finally:
            os.close(write_fd)
        try:
            with selectors.DefaultSelector() as selector:
                selector.register(read_fd, selectors.EVENT_READ)
                assert selector.select(timeout=10), 'Xvfb startup timed out'
                display = os.read(read_fd, 32).decode().strip()
        finally:
            os.close(read_fd)
        assert display.isdecimal(), display
        environment['DISPLAY'] = ':' + display
        # Portal-backed GTK choosers must also belong to this private desktop.
        # Start their bus after DISPLAY/XDG paths are set, so activation cannot
        # touch the user's session bus or open a chooser on the user's display.
        bus = subprocess.Popen(['dbus-daemon', '--session', '--nofork', '--nopidfile',
            '--print-address=1'], env=environment, stdout=subprocess.PIPE,
            stderr=bus_log, start_new_session=True)
        with selectors.DefaultSelector() as selector:
            selector.register(bus.stdout, selectors.EVENT_READ)
            assert selector.select(timeout=5), 'private session bus startup timed out'
            address = bus.stdout.readline().decode().strip()
        assert address.startswith('unix:'), address
        environment['DBUS_SESSION_BUS_ADDRESS'] = address
        renderer = checked(['glxinfo', '-B']).decode()
        assert args.renderer in renderer.lower(), renderer
        help_text = checked([args.binary.resolve(), '--help'], cwd=args.project).decode()
        assert 'Vendetta' in help_text and '--no-preview' in help_text, help_text
        ipc_path = root / 'player.sock'
        command = [str(args.binary.resolve()), '--mute', '--no-hwdec', '--no-preview',
            '--no-smooth-bar', '--loop', '--ao', args.audio_output,
            '--mpv-opt', 'config=no', '--mpv-opt', 'load-scripts=no',
            '--mpv-opt', 'vd-lavc-threads=1', '--mpv-opt', 'input-ipc-server=' + str(ipc_path), str(media)]
        player = subprocess.Popen(command, env=environment, cwd=args.project,
            stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
        deadline = time.monotonic() + 20
        while True:
            if player.poll() is not None:
                raise RuntimeError('player exited before IPC startup')
            ipc = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            try:
                ipc.connect(str(ipc_path))
                break
            except OSError:
                ipc.close()
                ipc = None
                if time.monotonic() >= deadline:
                    raise TimeoutError('player IPC startup')
                time.sleep(.05)
        ipc.settimeout(5)
        channel = ipc.makefile('rwb', buffering=0)
        request_id = 0

        def request(command, optional=False):
            global request_id
            request_id += 1
            channel.write((json.dumps({'command': command, 'request_id': request_id}) + '\n').encode())
            while True:
                line = channel.readline()
                if not line:
                    raise EOFError('player IPC closed')
                result = json.loads(line)
                if result.get('request_id') == request_id:
                    if optional and result.get('error') != 'success':
                        return None
                    assert result.get('error') == 'success', result
                    return result.get('data')

        deadline = time.monotonic() + 15
        while request(['get_property', 'video-out-params'], True) is None:
            assert time.monotonic() < deadline, 'video output was not configured'
            time.sleep(.05)
        assert request(['get_property', 'mute']) is True
        deadline = time.monotonic() + 15
        while True:
            actual_ao = request(['get_property', 'current-ao'], True)
            audio = request(['get_property', 'audio-params'], True)
            if actual_ao == args.audio_output and audio is not None:
                break
            assert time.monotonic() < deadline, ('audio output did not initialize', actual_ao)
            time.sleep(.05)
        video = request(['get_property', 'video-params'])
        assert video['w'] == 320 and video['h'] == 180, video
        assert audio['samplerate'] == 48000, audio
        maps = Path(f'/proc/{player.pid}/maps').read_text()
        if args.libc_free:
            assert not any('/libc.so' in line or '/libpthread.so' in line for line in maps.splitlines()), maps
            assert 'libmpv-vlibc.so' in maps, maps
        rss = int(next(line.split()[1] for line in Path(f'/proc/{player.pid}/status').read_text().splitlines() if line.startswith('VmRSS:')))
        # Sokol's X11 window does not publish _NET_WM_PID. This private display
        # contains only this player; identify its visible title instead.
        windows = checked(['xdotool', 'search', '--onlyvisible', '--name',
            'Vendetta|colors[.]mkv']).decode().splitlines()
        assert windows, 'no visible player window'
        window = windows[0]
        geometry = dict(line.split('=', 1) for line in checked(['xdotool', 'getwindowgeometry', '--shell', window]).decode().splitlines() if '=' in line)
        width, height = int(geometry['WIDTH']), int(geometry['HEIGHT'])
        x, y = int(geometry['X']), int(geometry['Y'])
        request(['set_property', 'pause', True])

        def frame(position):
            request(['seek', position, 'absolute+exact'])
            deadline = time.monotonic() + 5
            while True:
                current = request(['get_property', 'time-pos'], True)
                if current is not None and abs(current - position) < .15 and not request(['get_property', 'seeking']):
                    break
                assert time.monotonic() < deadline, ('seek did not complete', current)
                time.sleep(.05)
            # Vendetta intentionally caps paints of paused video at one per
            # second (render_wake_relay). IPC seek completion precedes that paint.
            time.sleep(1.2)
            pixels = checked(['ffmpeg', '-nostdin', '-v', 'error', '-f', 'x11grab', '-draw_mouse', '0',
                '-video_size', f'{width}x{height}', '-i', f'{environment["DISPLAY"]}+{x},{y}',
                '-frames:v', '1', '-threads', '1', '-f', 'rawvideo', '-pix_fmt', 'rgb24', 'pipe:1'])
            assert len(pixels) == width * height * 3
            return pixels

        if args.file_dialog:
            # Exercise the application's actual Ctrl+O -> ui2 -> os.exec -> zenity
            # path. A separate filename proves the selection reached libmpv.
            selection = root / 'selected ü clip.mkv'
            selection.write_bytes(media.read_bytes())

            def dialog_window():
                deadline = time.monotonic() + 10
                while time.monotonic() < deadline:
                    for pid in Path(f'/proc/{player.pid}/task/{player.pid}/children').read_text().split():
                        comm = Path(f'/proc/{pid}/comm')
                        if comm.exists() and comm.read_text().strip() == 'zenity':
                            # GtkFileChooserNative may live in a portal process
                            # instead of zenity. Both are on the private bus.
                            result = subprocess.run(['xdotool', 'search', '--onlyvisible',
                                '--name', '^Open video$'],
                                env=environment, capture_output=True, text=True, timeout=2)
                            if result.returncode == 0:
                                return result.stdout.splitlines()[0]
                    time.sleep(.05)
                raise TimeoutError('Vendetta did not open the native file dialog')

            checked(['xdotool', 'windowfocus', window, 'key', 'ctrl+o'])
            dialog = dialog_window()
            checked(['xdotool', 'windowfocus', dialog, 'key', 'ctrl+l'])
            time.sleep(.2)
            # Clipboard UTF-8 avoids xdotool silently omitting characters that
            # are absent from Xvfb's minimal keyboard map.
            clipboard = subprocess.Popen(['xclip', '-selection', 'clipboard', '-in', '-quiet'],
                env=environment, stdin=subprocess.PIPE, stdout=log, stderr=log,
                start_new_session=True)
            clipboard.stdin.write(str(selection).encode())
            clipboard.stdin.close()
            deadline = time.monotonic() + 5
            while True:
                received = subprocess.run(['xclip', '-selection', 'clipboard', '-out',
                    '-target', 'UTF8_STRING'], env=environment, capture_output=True, timeout=2)
                if received.returncode == 0 and received.stdout == str(selection).encode():
                    break
                if clipboard.poll() is not None or time.monotonic() >= deadline:
                    raise AssertionError(('file chooser clipboard', received.stdout, received.stderr))
                time.sleep(.02)
            checked(['xdotool', 'key', 'ctrl+a', 'ctrl+v'])
            time.sleep(.2)
            checked(['xdotool', 'key', 'ctrl+a', 'ctrl+c'])
            # The chooser must acquire the selection, otherwise reading it back
            # could return the original xclip contents even if paste failed.
            assert clipboard.wait(timeout=5) == 0
            entered = checked(['xclip', '-selection', 'clipboard', '-out', '-target',
                'UTF8_STRING']).decode()
            assert entered == str(selection), ('file chooser entry', entered, str(selection))
            checked(['xdotool', 'key', 'Return'])
            time.sleep(.3)
            remaining = subprocess.run(['xdotool', 'search', '--onlyvisible', '--name', '^Open video$'],
                env=environment, capture_output=True, text=True, timeout=2)
            if remaining.returncode == 0:
                # GTK may accept the filename completion before activating Open.
                checked(['xdotool', 'key', 'Return'])
            deadline = time.monotonic() + 10
            while request(['get_property', 'path'], True) != str(selection):
                assert time.monotonic() < deadline, 'file selection did not reach libmpv'
                time.sleep(.05)
            checked(['xdotool', 'windowfocus', window, 'key', 'ctrl+o'])
            dialog = dialog_window()
            checked(['xdotool', 'windowfocus', dialog, 'key', 'Escape'])
            deadline = time.monotonic() + 5
            while Path(f'/proc/{player.pid}/task/{player.pid}/children').read_text().strip():
                assert time.monotonic() < deadline, 'cancelled dialog child was not reaped'
                time.sleep(.05)
            assert request(['get_property', 'path']) == str(selection), 'cancel changed the current file'
            print('Native dialog selection and cancellation passed', flush=True)
        request(['set_property', 'pause', True])

        first = frame(.25)
        second = frame(1.25)
        args.output.parent.mkdir(parents=True, exist_ok=True)
        png(args.output.with_name(args.output.stem + '-first.png'), first, width, height)
        png(args.output.with_suffix('.png'), second, width, height)
        delta = 0
        samples = 0
        for row in range(height // 2 - 40, height // 2 + 40):
            start = (row * width + width // 2 - 50) * 3
            end = start + 100 * 3
            delta += sum(abs(a - b) for a, b in zip(first[start:end], second[start:end]))
            samples += end - start
        assert delta / samples > 2, ('video pixels did not change after seek', delta / samples,
            'whole-window mean difference', sum(abs(a-b) for a,b in zip(first, second)) / len(first),
            'frame range', min(second), max(second))
        request(['set_property', 'pause', False])
        initial = request(['get_property', 'time-pos'])
        time.sleep(.4)
        assert request(['get_property', 'time-pos']) > initial, 'playback did not resume'
        args.output.parent.mkdir(parents=True, exist_ok=True)
        screenshot = args.output.with_suffix('.png')
        png(screenshot, second, width, height)
        channel.close()
        channel = None
        ipc.close()
        ipc = None
        close_window(environment['DISPLAY'], window)
        assert player.wait(timeout=10) == 0, 'player did not close cleanly'
        result = {'audio_output': args.audio_output, 'libc_free': args.libc_free, 'file_dialog': args.file_dialog,
            'video': video, 'audio': audio, 'rss_kib': rss, 'pixel_delta': delta / samples,
            'screenshot': str(screenshot), 'renderer': renderer}
        args.output.write_text(json.dumps(result, indent=2) + '\n')
        dialog_result = ', native dialog selection/cancellation' if args.file_dialog else ''
        print('Vendetta help, private display/settings, video/audio decoding, rendered seek frames'
            + dialog_result + ', resumed playback and close passed')
    except BaseException:
        log.seek(0)
        print(log.read())
        server_log.seek(0)
        print(server_log.read())
        bus_log.seek(0)
        print(bus_log.read())
        if 'DISPLAY' in environment and server is not None and server.poll() is None:
            tree = subprocess.run(['xwininfo', '-root', '-tree'], env=environment,
                capture_output=True, text=True, timeout=3)
            print(tree.stdout)
            try:
                pixels = checked(['ffmpeg', '-nostdin', '-v', 'error', '-f', 'x11grab',
                    '-video_size', '1280x800', '-i', environment['DISPLAY'], '-frames:v', '1',
                    '-threads', '1', '-f', 'rawvideo', '-pix_fmt', 'rgb24', 'pipe:1'])
                png(args.output.with_name(args.output.stem + '-failure.png'), pixels, 1280, 800)
            except Exception as error:
                print('Could not capture failure display:', error)
        raise
    finally:
        if channel is not None:
            channel.close()
        if ipc is not None:
            ipc.close()
        for process in (player, clipboard, bus, server):
            if process is not None and process.poll() is None:
                os.killpg(process.pid, signal.SIGTERM)
                try:
                    process.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()
        log.close()
        server_log.close()
        if bus is not None:
            bus.stdout.close()
        bus_log.close()
