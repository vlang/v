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
export PATH="$work/bin:$PATH"

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

# Minimal images may omit either legacy file and the sources directory entirely.
mkdir "$work/empty"
V_CI_APT_ROOT="$work/empty" bash "$repo_root/.github/workflows/disable_azure_mirror.sh" >"$work/empty.log"
test -f "$work/empty/apt.conf.d/80-v-ci-network"

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
