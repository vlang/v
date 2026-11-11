#!/bin/bash
set -euo pipefail

# V_CI_APT_ROOT lets the shell contract test use an isolated apt configuration.
apt_root="${V_CI_APT_ROOT:-/etc/apt}"

# Hosted images use either mirror lists, one-line sources, or deb822 sources.
# Rewrite only Ubuntu archive URLs; preserve suites, components and signing keys.
for source in "$apt_root/apt-mirrors.txt" "$apt_root/sources.list" \
    "$apt_root"/sources.list.d/*.list "$apt_root"/sources.list.d/*.sources; do
    [ -f "$source" ] || continue
    echo "CONFIGURE APT SOURCE: $source"
    sudo sed -i -E \
        -e 's@https?://azure\.archive\.ubuntu\.com/@https://archive.ubuntu.com/@g' \
        -e 's@http://(archive|security)\.ubuntu\.com/@https://\1.ubuntu.com/@g' \
        "$source"
    cat "$source"
done

for source in "$apt_root/sources.list.d/azure-cli.list" \
    "$apt_root/sources.list.d/azure-cli.sources"; do
    if [ -f "$source" ]; then
        echo "DISABLE OPTIONAL APT SOURCE: $source"
        sudo mv "$source" "$source.disabled"
    fi
done

# A partial update normally exits successfully, so `v retry` cannot retry it.
# Keep prerequisite failures visible and bound downloads before the outer retry.
sudo mkdir -p "$apt_root/apt.conf.d"
sudo tee "$apt_root/apt.conf.d/80-v-ci-network" >/dev/null <<'EOF'
Acquire::Retries "3";
Acquire::http::Timeout "30";
Acquire::https::Timeout "30";
APT::Update::Error-Mode "any";
EOF
