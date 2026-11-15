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
grep -F '#if defined(_WIN32) || __V_HEADERLESS_LIBC' "$work/filtered.c" >/dev/null
"${CC:-cc}" -std=c99 -Werror -o "$work/test" "$work/filtered.c"
"$work/test"

# Exercise the real FIFO/awk/compiler pipeline, not just direct awk output.
sh "$root/cmd/tools/vc_compat.sh" "$work/old_vc.c" "${CC:-cc}" -std=c99 -Werror -o "$work/test_streamed"
"$work/test_streamed"

# Regenerated snapshots already guard this include. Leave them unchanged.
cat > "$work/guarded_poll.c" <<'EOF'
#if !__V_HEADERLESS_LIBC
#include <poll.h>
#endif
EOF
awk -f "$root/cmd/tools/vc_compat.awk" "$work/guarded_poll.c" > "$work/guarded_filtered.c"
cmp "$work/guarded_poll.c" "$work/guarded_filtered.c"

# The released vc snapshot uses select() without including sys/select.h.
# Reject implicit declarations under strict C99 both before and after filtering.
cat > "$work/select_vc.c" <<'EOF'
#include <poll.h>
int os__select_probe(int fd) {
    fd_set read_set;
    struct timeval ts = {0, 0};
    FD_ZERO(&read_set);
    FD_SET(fd, &read_set);
    int res = select(fd + 1, &read_set, 0, 0, &ts);
    return res;
}
EOF
if "${CC:-cc}" -std=c99 -Wall -Wextra -Werror -fsyntax-only "$work/select_vc.c" >"$work/select_baseline.log" 2>&1; then
    echo 'Baseline unexpectedly declares select without sys/select.h' >&2
    exit 1
fi
awk -f "$root/cmd/tools/vc_compat.awk" "$work/select_vc.c" > "$work/select_filtered.c"
grep -Fx '#include <sys/select.h>' "$work/select_filtered.c" >/dev/null
"${CC:-cc}" -std=c99 -Wall -Wextra -Werror -fsyntax-only "$work/select_filtered.c"

cat > "$work/bsd_types.c" <<'EOF'
struct timespec { long tv_sec; long tv_nsec; };
struct itimerspec {
    struct timespec it_interval;
    struct timespec it_value;
};
typedef union { unsigned char _opaque[128]; long long _align; } sigset_t;
EOF
awk -f "$root/cmd/tools/vc_compat.awk" "$work/bsd_types.c" > "$work/bsd_filtered.c"
grep -F '#if !defined(__FreeBSD__) || __V_HEADERLESS_LIBC' "$work/bsd_filtered.c" >/dev/null
grep -F '#if !defined(__OpenBSD__)' "$work/bsd_filtered.c" >/dev/null
test "$(grep -c '^#endif$' "$work/bsd_filtered.c")" -eq 3

# FreeBSD needs the snapshot's time types in headerless mode, and the native
# definitions in header mode. Model the latter without loading foreign headers
# so both branches can also be checked on other hosts.
cat > "$work/bsd_time.c" <<'EOF'
#if !__V_HEADERLESS_LIBC
typedef struct timespec { long tv_sec; long tv_nsec; } native_timespec;
typedef struct itimerspec { struct timespec it_interval; struct timespec it_value; } native_itimerspec;
#endif
#if __V_HEADERLESS_LIBC
struct timespec { long tv_sec; long tv_nsec; };
struct itimerspec {
    struct timespec it_interval;
    struct timespec it_value;
};
#endif
int main(void) {
    struct timespec req = {0, 1000000};
    struct itimerspec timer = {req, req};
    return timer.it_value.tv_nsec == 1000000 ? 0 : 1;
}
EOF
for headerless in 0 1; do
    sh "$root/cmd/tools/vc_compat.sh" "$work/bsd_time.c" "${CC:-cc}" \
        -U__FreeBSD__ -D__FreeBSD__ -D__V_HEADERLESS_LIBC="$headerless" -std=c99 -Werror \
        -o "$work/bsd_time_$headerless"
    "$work/bsd_time_$headerless"
done

# The new libc-owned poll helper must preserve the observable readiness
# transition on a pipe. Test the actual header used by os/fd.c.v.
cat > "$work/poll_runtime.c" <<'EOF'
#include "vlib/os/fd_poll.h"
#include <unistd.h>
int main(void) {
    int p[2];
    if (pipe(p) != 0) return 1;
    if (v_os_fd_poll_revents(p[0], POLLIN) != 0) return 2;
    if (write(p[1], "x", 1) != 1) return 3;
    if ((v_os_fd_poll_revents(p[0], POLLIN) & POLLIN) == 0) return 4;
    char c = 0;
    if (read(p[0], &c, 1) != 1 || c != 'x') return 5;
    if (v_os_fd_poll_revents(p[0], POLLIN) != 0) return 6;
    if (close(p[0]) != 0) return 7;
    // The C helper must expose native POLLNVAL for a closed descriptor.
    if ((v_os_fd_poll_revents(p[0], POLLIN) & POLLNVAL) == 0) return 8;
    if (close(p[1]) != 0) return 9;
    return 0;
}
EOF
"${CC:-cc}" -std=c99 -Wall -Wextra -Werror -I"$root" -o "$work/poll_runtime" "$work/poll_runtime.c"
"$work/poll_runtime"

# A headerless snapshot has already declared libc types before it uses os's
# bindings. Native headers would redefine them, and its old pollfd used V's
# pointer-sized int instead of C's int. Exercise both libc modes at runtime.
cat > "$work/headerless_vc.c" <<'C_EOF'
#ifndef __V_HEADERLESS_LIBC
#define __V_HEADERLESS_LIBC 1
#endif
#include <stddef.h>
#include <poll.h>
#if __V_HEADERLESS_LIBC
typedef struct fd_set { unsigned long bits[128 / sizeof(unsigned long)]; } fd_set;
struct timeval { long tv_sec; long tv_usec; };
#endif
typedef _Bool bool;
typedef int i32;
typedef short i16;
typedef long __v_int;
#if __V_HEADERLESS_LIBC
struct pollfd {
	__v_int fd;
	i16 events;
	i16 revents;
};
#endif
int pipe(int*);
long write(int, const void*, size_t);
int close(int);
bool os__fd_is_pending(__v_int fd) {
    fd_set read_set = {0};
    struct timeval timeout = {0, 0};
    return select(fd + 1, &read_set, 0, 0, &timeout) > 0;
}
int main(void) {
    int fds[2];
    if (sizeof(struct pollfd) != 8 || pipe(fds) != 0) return 1;
    if (os__fd_is_pending(fds[0])) return 2;
    struct pollfd descriptor = {fds[0], POLLIN, 0};
    if (poll(&descriptor, 1, 0) != 0) return 3;
    if (write(fds[1], "x", 1) != 1) return 4;
    if (poll(&descriptor, 1, 0) != 1 || !(descriptor.revents & POLLIN)) return 5;
    close(fds[0]);
    close(fds[1]);
    return 0;
}
C_EOF
if "${CC:-cc}" -std=c99 -Werror -fsyntax-only "$work/headerless_vc.c" >"$work/headerless_baseline.log" 2>&1; then
    echo 'Baseline unexpectedly accepts the headerless snapshot' >&2
    exit 1
fi
for mode in 0 1; do
    sh "$root/cmd/tools/vc_compat.sh" "$work/headerless_vc.c" "${CC:-cc}" \
        -std=c99 -Wall -Wextra -Werror -D__V_HEADERLESS_LIBC="$mode" -o "$work/headerless_vc"
    "$work/headerless_vc"
done

# The new helper must work without pulling any libc headers into the standalone
# preamble. Poison those include paths so a transitive include fails the check.
mkdir -p "$work/poison/sys"
for header in poll.h sys/poll.h sys/select.h features.h; do
    echo '#error Native libc header reached the headerless helper' > "$work/poison/$header"
done
cat > "$work/headerless_runtime.c" <<'C_EOF'
#define __V_HEADERLESS_LIBC 1
#include <stddef.h>
typedef struct fd_set { unsigned long bits[128 / sizeof(unsigned long)]; } fd_set;
struct timeval { long tv_sec; long tv_usec; };
#include "vlib/os/fd_poll.h"
int pipe(int*);
long write(int, const void*, size_t);
int close(int);
int main(void) {
    int fds[2];
    if (sizeof(struct v_os_pollfd) != 8 || pipe(fds) != 0) return 1;
    if (v_os_fd_poll_revents(fds[0], 1) != 0) return 2;
    if (write(fds[1], "x", 1) != 1) return 3;
    if (!(v_os_fd_poll_revents(fds[0], 1) & 1)) return 4;
    close(fds[0]);
    if (!(v_os_fd_poll_revents(fds[0], 1) & 0x20)) return 5;
    close(fds[1]);
    return 0;
}
C_EOF
"${CC:-cc}" -std=c99 -Wall -Wextra -Werror -I"$work/poison" -I"$root" \
    -o "$work/headerless_runtime" "$work/headerless_runtime.c"
"$work/headerless_runtime"

# Syntax-check the Bionic count ABI without claiming execution on Android.
cat >> "$work/headerless_runtime.c" <<'C_EOF'
_Static_assert(__builtin_types_compatible_p(v_os_nfds_t, unsigned int), "Bionic nfds_t");
C_EOF
"${CC:-cc}" -std=c11 -Wall -Wextra -Werror -D__ANDROID__ -U__APPLE__ \
    -I"$work/poison" -I"$root" -fsyntax-only "$work/headerless_runtime.c"
