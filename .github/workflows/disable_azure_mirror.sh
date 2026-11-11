#!/bin/bash
set -euo pipefail

# V_CI_APT_ROOT lets the shell contract test use an isolated apt configuration.
apt_root="${V_CI_APT_ROOT:-/etc/apt}"

as_root() {
    if command -v sudo >/dev/null 2>&1; then
        sudo "$@"
    elif [ "$(id -u)" = 0 ]; then
        "$@"
    else
        echo 'APT setup requires sudo or a root container' >&2
        return 1
    fi
}

sources=()
for source in "$apt_root/apt-mirrors.txt" "$apt_root/sources.list" \
    "$apt_root"/sources.list.d/*.list "$apt_root"/sources.list.d/*.sources; do
    [ -f "$source" ] && sources+=("$source")
done

has_source_host() {
    [ "${#sources[@]}" -gt 0 ] || return 1
    awk -v pattern="$1" '
        { sub(/#.*/, ""); if ($0 ~ pattern) found = 1 }
        END { exit !found }
    ' "${sources[@]}"
}

resolves() {
    local status
    if timeout --kill-after=1s 5s getent ahosts "$1" >/dev/null 2>&1; then
        echo "APT DNS OK: $1"
        return 0
    else
        status=$?
        echo "APT DNS FAILED: $1 (status $status)" >&2
        return "$status"
    fi
}

archive_host=archive.ubuntu.com
security_host=security.ubuntu.com
need_fallback=false
if has_source_host 'https?://(azure[.]archive|archive)[.]ubuntu[.]com/'; then
    resolves "$archive_host" || need_fallback=true
fi
if has_source_host 'https?://security[.]ubuntu[.]com/'; then
    resolves "$security_host" || need_fallback=true
fi
if "$need_fallback"; then
    # Launchpad lists this exact HTTPS URL as an enabled, official Ubuntu mirror:
    # https://launchpad.net/ubuntu/+mirror/mirror.umd.edu-archive
    # APT still authenticates all metadata/packages with the existing Ubuntu keyring.
    fallback_host=mirror.umd.edu
    if ! resolves "$fallback_host"; then
        echo 'No resolvable Ubuntu archive mirror; leaving apt sources unchanged' >&2
        exit 1
    fi
    archive_host="$fallback_host"
    security_host="$fallback_host"
    echo "APT DNS FALLBACK: https://$fallback_host/ubuntu/"
fi

# Hosted images use either mirror lists, one-line sources, or deb822 sources.
# Rewrite only Ubuntu archive URLs; preserve suites, components and signing keys.
for source in "${sources[@]}"; do
    echo "CONFIGURE APT SOURCE: $source"
    as_root sed -i -E \
        -e "s@https?://(azure\\.archive|archive)\\.ubuntu\\.com/@https://$archive_host/@g" \
        -e "s@https?://security\\.ubuntu\\.com/@https://$security_host/@g" \
        "$source"
    cat "$source"
done

for source in "$apt_root/sources.list.d/azure-cli.list" \
    "$apt_root/sources.list.d/azure-cli.sources"; do
    if [ -f "$source" ]; then
        echo "DISABLE OPTIONAL APT SOURCE: $source"
        as_root mv "$source" "$source.disabled"
    fi
done

# A partial update normally exits successfully, so `v retry` cannot retry it.
# Keep prerequisite failures visible and bound downloads before the outer retry.
as_root mkdir -p "$apt_root/apt.conf.d"
as_root tee "$apt_root/apt.conf.d/80-v-ci-network" >/dev/null <<'EOF'
Acquire::Retries "3";
Acquire::http::Timeout "30";
Acquire::https::Timeout "30";
APT::Update::Error-Mode "any";
EOF
