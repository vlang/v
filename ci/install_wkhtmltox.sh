#!/bin/sh
# Install the C interop library for the guest's distribution and architecture.
set -eu

. /etc/os-release
case "$ID" in
    debian)
        version=0.12.6.1-3
        distribution=bookworm
        ;;
    ubuntu)
        version=0.12.6.1-2
        distribution=jammy
        ;;
    *)
        echo "Unsupported wkhtmltox distribution: $ID" >&2
        exit 1
        ;;
esac
architecture=$(dpkg --print-architecture)
package="wkhtmltox_${version}.${distribution}_${architecture}.deb"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
wget --quiet -O "$work/$package" \
    "https://github.com/wkhtmltopdf/packaging/releases/download/$version/$package"
# apt resolves the package's font, image and SSL dependencies on fresh guests.
sudo apt-get install --quiet -y "$work/$package"
