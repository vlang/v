"""Exercise native media prerequisites with private files and live process maps."""
import argparse
import os
from pathlib import Path
import resource
import selectors
import signal
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('binary', type=Path)
parser.add_argument('--hosted', action='store_true', help='run the same assertions against host libraries')
args = parser.parse_args()


def read_line(process, timeout):
    with selectors.DefaultSelector() as selector:
        selector.register(process.stdout, selectors.EVENT_READ)
        if not selector.select(timeout=timeout):
            raise TimeoutError('media fixture output')
        return process.stdout.readline().strip()


def check_maps(process):
    maps = Path(f'/proc/{process.pid}/maps').read_text()
    for line in maps.splitlines():
        if any(name in line for name in ('/libc.so', '/libpthread.so', '/libstdc++.so.6')):
            raise AssertionError('native media fixture loaded a host runtime: ' + line)
    for library in ('avcodec', 'avformat', 'avfilter', 'avdevice', 'avutil',
                    'swscale', 'swresample', 'ass', 'harfbuzz', 'fribidi', 'asound', 'm'):
        assert 'lib' + library + '-vlibc.so' in maps, (library, maps)


with tempfile.TemporaryDirectory(prefix='vlibc native codecs ') as temporary:
    root = Path(temporary)
    video = root / 'video \u00e9.mkv'
    subprocess.run(['ffmpeg', '-hide_banner', '-loglevel', 'error', '-nostdin',
        '-filter_threads', '1', '-filter_complex_threads', '1',
        '-f', 'lavfi', '-i', 'color=c=red:s=64x48:r=8:d=2,drawbox=c=blue:t=fill:enable=gte(t\\,1),drawbox=x=0:y=0:w=16:h=16:c=yellow:t=fill',
        '-f', 'lavfi', '-i', 'sine=frequency=1000:sample_rate=48000:duration=2',
        '-threads', '1', '-c:v', 'ffv1', '-g', '8', '-level', '3', '-pix_fmt', 'yuv420p',
        '-c:a', 'pcm_s16le', '-shortest', str(video)], check=True, timeout=30)
    font = subprocess.check_output(['fc-match', '-f', '%{file}', 'DejaVu Sans'], text=True)
    if not Path(font).is_file():
        raise RuntimeError('the media probe requires a real DejaVu Sans font')
    environment = os.environ.copy()
    for key in ('LD_PRELOAD', 'LD_LIBRARY_PATH'):
        environment.pop(key, None)
    environment.update(XDG_CACHE_HOME=str(root / 'cache'), FONTCONFIG_FILE='/etc/fonts/fonts.conf')
    gate = root / 'release'
    environment['V_NATIVE_CODECS_GATE'] = str(gate)
    stderr = root / 'stderr.log'
    with stderr.open('w') as errors:
        process = subprocess.Popen([str(args.binary.resolve()), str(video), font],
            env=environment, stdout=subprocess.PIPE, stderr=errors, text=True, start_new_session=True,
            preexec_fn=lambda: resource.setrlimit(resource.RLIMIT_CORE, (0, 0)))
        try:
            assert read_line(process, 15) == 'native codec fixture ready'
            if not args.hosted:
                check_maps(process)
            gate.touch()
            output = read_line(process, 45)
            if output != 'native shaping, subtitles, PCM, filtering, decoding and seeking passed':
                process.wait(timeout=5)
                raise RuntimeError(output + '\n' + stderr.read_text())
            if not args.hosted:
                check_maps(process)
            gate.unlink()
            trailing, _ = process.communicate(timeout=10)
            if process.returncode or trailing:
                raise RuntimeError(str(process.returncode) + '\n' + trailing + stderr.read_text())
        finally:
            if process.poll() is None:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
    print(('hosted' if args.hosted else 'libc-free') +
          ' native shaping, subtitles, PCM, filtering, decoding and seeking passed')
