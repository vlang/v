#!/usr/bin/env python3
"""Exercise FreeType download retries and checksum enforcement without building."""
import hashlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
import subprocess
import tempfile
import threading
import unittest


class FreeTypeDownloadTests(unittest.TestCase):
    def download(self, failures=0, corrupt=False):
        payload = b"FreeType download fixture"
        requests = []

        class Handler(BaseHTTPRequestHandler):
            def do_GET(self):
                requests.append(self.path)
                if len(requests) <= failures:
                    self.close_connection = True
                    return
                self.send_response(200)
                self.end_headers()
                self.wfile.write(b"corrupt" if corrupt else payload)

            def log_message(self, *args):
                pass

        server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        try:
            source = Path(__file__).with_name("build_msan_freetype.sh").read_text()
            # Execute the real download/checksum stage with a local fixture.
            source = source.split('\ntar -xJf ', 1)[0]
            source = source.replace(
                "12991c4e55c506dd7f9b765933e62fd2be2e06d421505d7950a132e4f1bb484d",
                hashlib.sha256(payload).hexdigest(),
            ).replace(
                "https://download.savannah.gnu.org/releases/freetype/",
                f"http://127.0.0.1:{server.server_port}/",
            )
            with tempfile.TemporaryDirectory() as directory:
                result = subprocess.run(
                    ["bash", "-c", source, "download-test", directory],
                    capture_output=True, text=True, timeout=30,
                )
            return result, requests
        finally:
            server.shutdown()
            server.server_close()
            thread.join()

    def test_empty_replies_are_retried(self):
        result, requests = self.download(failures=2)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(requests), 3)
        self.assertIn(": OK", result.stdout)

    def test_persistent_failure_is_reported(self):
        result, requests = self.download(failures=10)
        self.assertEqual(result.returncode, 52, result.stderr)
        self.assertEqual(len(requests), 4)
        self.assertNotIn(": OK", result.stdout)

    def test_corrupt_archive_is_rejected(self):
        result, requests = self.download(corrupt=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(len(requests), 1)
        self.assertIn("FAILED", result.stdout)


if __name__ == "__main__":
    unittest.main()
