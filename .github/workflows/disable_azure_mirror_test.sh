#!/bin/bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/../.." && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/apt/sources.list.d"

# Exercise the real script without sudo privileges or system apt changes.
cat >"$work/bin/sudo" <<'EOF'
#!/bin/bash
set -euo pipefail
if [ "${V_CI_TEST_FAIL_COMMAND:-}" = "$1" ]; then
    exit 42
fi
exec "$@"
EOF
chmod +x "$work/bin/sudo"
# DNS behavior is deterministic and never depends on the test runner's network.
cat >"$work/bin/getent" <<'EOF'
#!/bin/bash
set -euo pipefail
[ "$#" -eq 2 ] && [ "$1" = ahosts ] || exit 99
printf '%s\n' "$2" >>"$V_CI_TEST_DNS_LOG"
case " ${V_CI_TEST_DNS_FAILURES:-} " in
    *" $2 "*) exit 2 ;;
esac
EOF
cat >"$work/bin/timeout" <<'EOF'
#!/bin/bash
set -euo pipefail
[ "$#" -eq 5 ] && [ "$1" = --kill-after=1s ] && [ "$2" = 5s ] || exit 99
[ "$3" = getent ] && [ "$4" = ahosts ] || exit 99
if [ "${V_CI_TEST_DNS_TIMEOUT:-}" = "$5" ]; then
    printf 'timeout:%s\n' "$5" >>"$V_CI_TEST_DNS_LOG"
    exit 124
fi
shift 2
exec "$@"
EOF
chmod +x "$work/bin/getent" "$work/bin/timeout"
export PATH="$work/bin:$PATH"
export V_CI_TEST_DNS_LOG="$work/dns.log"
unset V_CI_TEST_DNS_FAILURES V_CI_TEST_DNS_TIMEOUT

cat >"$work/apt/apt-mirrors.txt" <<'EOF'
http://azure.archive.ubuntu.com/ubuntu/ priority:1
https://azure.archive.ubuntu.com/ubuntu/ priority:2
http://security.ubuntu.com/ubuntu/ priority:3
https://custom.example/ubuntu/ priority:4
EOF
cat >"$work/apt/sources.list" <<'EOF'
deb [signed-by=/usr/share/keyrings/ubuntu-archive-keyring.gpg] http://archive.ubuntu.com/ubuntu noble main
EOF
cat >"$work/apt/sources.list.d/ubuntu.sources" <<'EOF'
Types: deb
URIs: http://azure.archive.ubuntu.com/ubuntu/ http://security.ubuntu.com/ubuntu/
Suites: noble noble-updates noble-security
Components: main universe
Signed-By: /usr/share/keyrings/ubuntu-archive-keyring.gpg

Types: deb
URIs: mirror+file:/etc/apt/apt-mirrors.txt
Suites: noble-backports
Components: main
EOF
cat >"$work/apt/sources.list.d/custom.list" <<'EOF'
deb https://custom.example/ubuntu noble main
deb http://archive.ubuntu.com.example/ubuntu noble main
deb https://security.ubuntu.com.example/ubuntu noble main
EOF
cp "$work/apt/sources.list.d/custom.list" "$work/custom.expected"
printf '%s\n' 'deb https://packages.microsoft.com/repos/azure-cli noble main' \
    >"$work/apt/sources.list.d/azure-cli.list"
printf '%s\n' 'URIs: https://packages.microsoft.com/repos/azure-cli' \
    >"$work/apt/sources.list.d/azure-cli.sources"

V_CI_APT_ROOT="$work/apt" bash "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$work/setup.log"

cat >"$work/mirrors.expected" <<'EOF'
https://archive.ubuntu.com/ubuntu/ priority:1
https://archive.ubuntu.com/ubuntu/ priority:2
https://security.ubuntu.com/ubuntu/ priority:3
https://custom.example/ubuntu/ priority:4
EOF
cmp "$work/mirrors.expected" "$work/apt/apt-mirrors.txt"
grep -Fxq 'deb [signed-by=/usr/share/keyrings/ubuntu-archive-keyring.gpg] https://archive.ubuntu.com/ubuntu noble main' \
    "$work/apt/sources.list"
grep -Fxq 'URIs: https://archive.ubuntu.com/ubuntu/ https://security.ubuntu.com/ubuntu/' \
    "$work/apt/sources.list.d/ubuntu.sources"
grep -Fxq 'Suites: noble noble-updates noble-security' "$work/apt/sources.list.d/ubuntu.sources"
grep -Fxq 'Components: main universe' "$work/apt/sources.list.d/ubuntu.sources"
grep -Fxq 'Signed-By: /usr/share/keyrings/ubuntu-archive-keyring.gpg' \
    "$work/apt/sources.list.d/ubuntu.sources"
grep -Fxq 'URIs: mirror+file:/etc/apt/apt-mirrors.txt' "$work/apt/sources.list.d/ubuntu.sources"
cmp "$work/custom.expected" "$work/apt/sources.list.d/custom.list"
test ! -e "$work/apt/sources.list.d/azure-cli.list"
test ! -e "$work/apt/sources.list.d/azure-cli.sources"
test -f "$work/apt/sources.list.d/azure-cli.list.disabled"
test -f "$work/apt/sources.list.d/azure-cli.sources.disabled"

apt-config -c "$work/apt/apt.conf.d/80-v-ci-network" dump >"$work/config.dump"
grep -Fxq 'Acquire::Retries "3";' "$work/config.dump"
grep -Fxq 'Acquire::http::Timeout "30";' "$work/config.dump"
grep -Fxq 'Acquire::https::Timeout "30";' "$work/config.dump"
grep -Fxq 'APT::Update::Error-Mode "any";' "$work/config.dump"

# Callers may run the helper again after the cache action has configured apt.
cp -R "$work/apt" "$work/once"
V_CI_APT_ROOT="$work/apt" bash "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$work/again.log"
diff -ru "$work/once" "$work/apt"

# Either canonical hostname failing (including a bounded timeout) switches only
# Ubuntu URLs. Suites, components, signing keys and custom repositories stay put.
for scenario in archive security both timeout; do
    failures=
    timed_out=
    case "$scenario" in
        archive) failures=archive.ubuntu.com ;;
        security) failures=security.ubuntu.com ;;
        both) failures='archive.ubuntu.com security.ubuntu.com' ;;
        timeout) timed_out=archive.ubuntu.com ;;
    esac
    root="$work/fallback-$scenario"
    cp -R "$work/apt" "$root"
    cp -R "$root" "$root.expected"
    for source in "$root.expected/apt-mirrors.txt" "$root.expected/sources.list" \
        "$root.expected"/sources.list.d/*.list "$root.expected"/sources.list.d/*.sources; do
        [ -f "$source" ] || continue
        sed -i -E 's@https?://(archive|security)\.ubuntu\.com/@https://mirror.umd.edu/@g' "$source"
    done
    V_CI_APT_ROOT="$root" V_CI_TEST_DNS_FAILURES="$failures" V_CI_TEST_DNS_TIMEOUT="$timed_out" \
        bash "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$root.log" 2>&1
    diff -ru "$root.expected" "$root"
    grep -Fxq 'APT DNS FALLBACK: https://mirror.umd.edu/ubuntu/' "$root.log"
    if [ "$scenario" = timeout ]; then
        grep -Fxq 'APT DNS FAILED: archive.ubuntu.com (status 124)' "$root.log"
    fi
    # The fallback configuration is idempotent and does not probe absent sources.
    : >"$V_CI_TEST_DNS_LOG"
    V_CI_APT_ROOT="$root" V_CI_TEST_DNS_FAILURES='archive.ubuntu.com security.ubuntu.com mirror.umd.edu' \
        bash "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$root.again.log"
    diff -ru "$root.expected" "$root"
    test ! -s "$V_CI_TEST_DNS_LOG"
done

# No reachable mirror must fail before modifying the existing configuration.
cp -R "$work/apt" "$work/all-dns-fail"
if V_CI_APT_ROOT="$work/all-dns-fail" \
    V_CI_TEST_DNS_FAILURES='archive.ubuntu.com security.ubuntu.com mirror.umd.edu' \
    bash "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$work/all-dns-fail.log" 2>&1; then
    echo 'apt setup hid a complete DNS outage' >&2
    exit 1
fi
diff -ru "$work/apt" "$work/all-dns-fail"
grep -Fxq 'APT DNS FAILED: mirror.umd.edu (status 2)' "$work/all-dns-fail.log"

# Minimal images may omit either legacy file and the sources directory entirely.
mkdir "$work/empty"
: >"$V_CI_TEST_DNS_LOG"
V_CI_APT_ROOT="$work/empty" bash "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$work/empty.log"
test -f "$work/empty/apt.conf.d/80-v-ci-network"
test ! -s "$V_CI_TEST_DNS_LOG"

# Commented Ubuntu examples and hostname lookalikes must not trigger DNS probes.
mkdir "$work/custom-only"
cp "$work/custom.expected" "$work/custom-only/sources.list"
printf '%s\n' '# deb http://archive.ubuntu.com/ubuntu noble main' >>"$work/custom-only/sources.list"
V_CI_APT_ROOT="$work/custom-only" \
    V_CI_TEST_DNS_FAILURES='archive.ubuntu.com security.ubuntu.com mirror.umd.edu' \
    bash "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$work/custom-only.log"
test ! -s "$V_CI_TEST_DNS_LOG"

# Ubuntu's minimal containers run as root and do not ship sudo. Exercise that
# branch with an isolated PATH and writable fixtures, without host privileges.
mkdir "$work/no-sudo-bin" "$work/root-container"
for command in awk sed cat mkdir tee; do
    ln -s "$(command -v "$command")" "$work/no-sudo-bin/$command"
done
cp "$work/bin/getent" "$work/bin/timeout" "$work/no-sudo-bin/"
cat >"$work/no-sudo-bin/id" <<'EOF'
#!/bin/bash
printf '0\n'
EOF
chmod +x "$work/no-sudo-bin/id"
cp "$work/apt/sources.list" "$work/root-container/sources.list"
V_CI_APT_ROOT="$work/root-container" PATH="$work/no-sudo-bin" \
    "$BASH" "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$work/root-container.log"
cmp "$work/apt/sources.list" "$work/root-container/sources.list"
test -f "$work/root-container/apt.conf.d/80-v-ci-network"
cat >"$work/no-sudo-bin/id" <<'EOF'
#!/bin/bash
printf '1000\n'
EOF
if V_CI_APT_ROOT="$work/root-container" PATH="$work/no-sudo-bin" \
    "$BASH" "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$work/no-privileges.log" 2>&1; then
    echo 'apt setup accepted a non-root environment without sudo' >&2
    exit 1
fi
grep -Fxq 'APT setup requires sudo or a root container' "$work/no-privileges.log"

# A failed source rewrite or config write must stop the prerequisite step.
for command in sed tee; do
    mkdir "$work/fail-$command"
    cp "$work/apt/sources.list" "$work/fail-$command/sources.list"
    if V_CI_APT_ROOT="$work/fail-$command" V_CI_TEST_FAIL_COMMAND="$command" \
        bash "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$work/fail-$command.log" 2>&1; then
        echo "apt setup hid a failed $command" >&2
        exit 1
    else
        status=$?
        test "$status" -eq 42
    fi
done

echo 'apt mirror setup contracts passed'
