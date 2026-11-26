"""Regression checks for the Linux native profile's build-time and live ELF audits."""
import ctypes
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from mapped_libraries import mapped_libraries, system_libc


class ElfProfileTests(unittest.TestCase):
    def test_only_declared_dependencies_satisfy_imports(self):
        audit = Path(__file__).with_name('audit_native_symbols.py')
        with tempfile.TemporaryDirectory() as temporary:
            prefix = Path(temporary)
            lib = prefix / 'lib'
            lib.mkdir()

            def compile(source, *flags):
                subprocess.run(['cc', '-x', 'c', '-', *flags], input=source,
                               text=True, check=True, cwd=prefix)

            compile('int supplied(void) { return 42; }', '-shared', '-fPIC', '-nostdlib',
                    '-o', lib / 'libsupply-vlibc.so')
            consumer = 'extern int supplied(void); int read_it(void) { return supplied(); }'
            common = ['-shared', '-fPIC', '-nostdlib', '-o', lib / 'libuse-vlibc.so']
            compile(consumer, *common)
            exe = prefix / 'demo'
            compile('extern int read_it(void); int main(void) { return read_it() != 42; }',
                    '-Llib', '-luse-vlibc', '-Wl,--allow-shlib-undefined,-rpath,$ORIGIN/lib',
                    '-o', exe)
            cmd = [sys.executable, audit, prefix, exe]
            rejected = subprocess.run(cmd, capture_output=True, text=True)
            self.assertNotEqual(rejected.returncode, 0)
            self.assertIn('supplied: libuse-vlibc.so', rejected.stdout)
            compile(consumer, *common, '-Llib', '-lsupply-vlibc', '-Wl,-rpath,$ORIGIN')
            subprocess.run(cmd, check=True, capture_output=True)
            subprocess.run([exe], check=True)

    def test_reads_the_soname_of_a_mapped_versioned_file(self):
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / 'libextra-2.33.so'
            subprocess.run(['cc', '-shared', '-fPIC', '-nostdlib', '-x', 'c', '-',
                            '-Wl,-soname,libextra.so.6', '-o', path],
                           input='int probe(void) { return 1; }', text=True, check=True)
            handle = ctypes.CDLL(str(path))
            self.assertEqual(handle.probe(), 1)
            self.assertIn('libextra.so.6', mapped_libraries(os.getpid()))
            self.assertTrue(system_libc({'libc.so.6'}))
            self.assertTrue(system_libc({'libpthread.so.0'}))
            self.assertFalse(system_libc({'libcrypto-vlibc.so'}))


if __name__ == '__main__':
    unittest.main()
