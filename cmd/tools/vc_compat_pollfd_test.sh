#!/bin/sh
set -eu

root=$(CDPATH= cd -P "$(dirname "$0")/../.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM

cat > "$work/old_vc.c" <<'EOF'
#include <poll.h>
struct pollfd {
    int fd;
    short events;
    short revents;
};
int main(void) {
    struct pollfd p = {-1, POLLIN, 0};
    return p.fd == -1 ? 0 : 1;
}
EOF

# The unfiltered snapshot must fail on a system whose poll.h owns pollfd.
if "${CC:-cc}" -std=c99 -fsyntax-only "$work/old_vc.c" >"$work/baseline.log" 2>&1; then
    echo 'The baseline no longer reproduces a pollfd redefinition' >&2
    exit 1
fi
awk -f "$root/cmd/tools/vc_compat.awk" "$work/old_vc.c" > "$work/filtered.c"
grep -F '#if defined(_WIN32)' "$work/filtered.c" >/dev/null
"${CC:-cc}" -std=c99 -Werror -o "$work/test" "$work/filtered.c"
"$work/test"

# Exercise the real FIFO/awk/compiler pipeline, not just direct awk output.
"$root/cmd/tools/vc_compat.sh" "$work/old_vc.c" "${CC:-cc}" -std=c99 -Werror -o "$work/test_streamed"
"$work/test_streamed"

cat > "$work/bsd_types.c" <<'EOF'
struct timespec { long tv_sec; long tv_nsec; };
struct itimerspec {
    struct timespec it_interval;
    struct timespec it_value;
};
typedef union { unsigned char _opaque[128]; long long _align; } sigset_t;
EOF
awk -f "$root/cmd/tools/vc_compat.awk" "$work/bsd_types.c" > "$work/bsd_filtered.c"
grep -F '#if !defined(__FreeBSD__)' "$work/bsd_filtered.c" >/dev/null
grep -F '#if !defined(__OpenBSD__)' "$work/bsd_filtered.c" >/dev/null
test "$(grep -c '^#endif$' "$work/bsd_filtered.c")" -eq 3
