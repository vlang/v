"""Build and publish native dependency profiles on Linux."""
import atexit
import os
from pathlib import Path
import shutil
import tempfile


def lock_build_directories(*directories):
    if os.name != 'posix':
        return
    import fcntl

    # Configure/make also modify the source cache. Take locks in a fixed order
    # when work, source cache or installation store is shared by two builders.
    for directory in sorted({path.resolve() for path in directories}):
        directory.mkdir(parents=True, exist_ok=True)
        fd = os.open(directory, os.O_DIRECTORY | os.O_RDONLY)
        atexit.register(os.close, fd)
        fcntl.flock(fd, fcntl.LOCK_EX)


class PrefixInstall:
    """Keep the permanent build path private until its dependency audit finishes."""

    def __init__(self, publication, name):
        self.publication = publication
        store = publication.parent / '.store'
        self.prefix = Path(tempfile.mkdtemp(prefix=name + '-', dir=store))
        atexit.register(self._discard)

    def _discard(self):
        shutil.rmtree(self.prefix, ignore_errors=True)

    def publish(self, receipt, recipe):
        (self.prefix / receipt).write_text(recipe + '\n')
        link = self.publication.with_name(self.publication.name + '.new-' + str(os.getpid()))
        try:
            link.symlink_to(self.prefix, target_is_directory=True)
            # Once visible, a caller may retain this exact directory even if a
            # different release subsequently becomes the public default.
            atexit.unregister(self._discard)
            try:
                link.replace(self.publication)
            except OSError:
                self._discard()
                raise
        finally:
            link.unlink(missing_ok=True)
