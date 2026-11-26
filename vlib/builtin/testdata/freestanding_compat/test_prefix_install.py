"""Check publication without downloading or building native dependencies."""
from pathlib import Path
import json
import os
import platform
import subprocess
import sys
import tempfile
import unittest

from prefix_install import PrefixInstall


class PrefixInstallTest(unittest.TestCase):
    @unittest.skipUnless(sys.platform == 'linux', 'Linux installation lock')
    def test_build_directory_lock_is_shared_between_processes(self):
        import fcntl

        with tempfile.TemporaryDirectory() as directory:
            child = ('from pathlib import Path\n'
                     'from prefix_install import lock_build_directories\n'
                     'import sys\n'
                     'lock_build_directories(Path(sys.argv[1]))\n'
                     'print("locked", flush=True)\n'
                     'sys.stdin.readline()\n')
            with subprocess.Popen([sys.executable, '-c', child, directory],
                                  cwd=Path(__file__).resolve().parent, text=True,
                                  stdin=subprocess.PIPE, stdout=subprocess.PIPE) as builder:
                fd = os.open(directory, os.O_RDONLY | os.O_DIRECTORY)
                try:
                    self.assertEqual(builder.stdout.readline().strip(), 'locked')
                    with self.assertRaises(BlockingIOError):
                        fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
                finally:
                    builder.communicate(input='finish\n', timeout=5)
                    os.close(fd)
                self.assertEqual(builder.returncode, 0)

    def test_failed_builders_preserve_the_registered_prefix(self):
        architecture = {'x86_64': 'amd64', 'aarch64': 'arm64'}.get(platform.machine())
        if sys.platform != 'linux' or not architecture:
            self.skipTest('native package builders require Linux amd64 or arm64')
        fixture = Path(__file__).resolve().parent
        for profile in ('x11', 'vpac'):
            with self.subTest(profile=profile), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                store = root / 'vlibc/.store'
                previous = store / 'previous'
                previous.mkdir(parents=True)
                (previous / 'vlibc-target').write_text(f'vlibc-native-v1\nlinux\n{architecture}\n')
                (previous / 'libexample').write_text('published')
                publication = store.parent / f'linux-{architecture}-shared'
                publication.symlink_to(previous)
                work = root / 'work'
                cache = work if profile == 'x11' else work / 'sources'
                cache.mkdir(parents=True)
                manifest = json.loads((fixture / f'{profile}_sources.json').read_text())
                suffix = '-source.tar' if profile == 'x11' else '-vpac-source.tar'
                for package in [*manifest, 'openlibm']:
                    (cache / (package + suffix)).write_bytes(b'invalid archive')
                command = [sys.executable, fixture / f'build_{profile}.py', work, '--install']
                if profile == 'x11':
                    command.append('--shared')
                run = subprocess.run(command, env=dict(os.environ, VMODULES=str(root), CC='cc'),
                                     stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
                self.assertNotEqual(run.returncode, 0)
                self.assertRegex(run.stdout, 'SHA-256|checksum mismatch')
                self.assertEqual(publication.resolve(), previous)
                self.assertEqual((publication / 'libexample').read_text(), 'published')
                self.assertEqual(set(store.iterdir()), {previous})

    def test_republication_preserves_the_previous_release(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            store = root / '.store'
            store.mkdir()
            publication = root / 'linux-amd64-shared'
            first = PrefixInstall(publication, 'same-recipe')
            (first.prefix / 'libexample').write_text('first')
            first.publish('.receipt', 'version')
            consumer = publication.resolve()

            # An interrupted installer leaves both the registration and its old files intact.
            failed = subprocess.run([sys.executable, '-c',
                'from pathlib import Path\n'
                'from prefix_install import PrefixInstall\n'
                'import sys\n'
                'attempt = PrefixInstall(Path(sys.argv[1]), "same-recipe")\n'
                '(attempt.prefix / "libexample").write_text("partial")\n'
                'sys.exit(7)\n', str(publication)], cwd=Path(__file__).resolve().parent)
            self.assertEqual(failed.returncode, 7)
            self.assertEqual(set(store.iterdir()), {consumer})
            self.assertEqual((publication / 'libexample').read_text(), 'first')

            second = PrefixInstall(publication, 'same-recipe')
            (second.prefix / 'libexample').write_text('second')
            self.assertEqual(publication.resolve(), consumer)
            self.assertEqual((publication / 'libexample').read_text(), 'first')
            second.publish('.receipt', 'version')
            self.assertEqual(publication.resolve(), second.prefix)
            self.assertEqual((publication / 'libexample').read_text(), 'second')
            self.assertEqual((consumer / 'libexample').read_text(), 'first')


if __name__ == '__main__':
    unittest.main()
