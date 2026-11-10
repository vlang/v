#!/usr/bin/env bash
# Publish only after every bootstrap platform passes. Reruns keep existing assets;
# recompiling the same commit may produce different embedded build timestamps.
set -euo pipefail

sha=${1:?Expected the full compiler commit SHA}
artifacts=${2:?Expected the downloaded artifact directory}
if [[ ! $sha =~ ^[0-9a-f]{40}$ ]]; then
  echo 'Expected a full lowercase Git commit SHA' >&2
  exit 1
fi
if [[ $(git rev-parse HEAD) != "$sha" ]]; then
  echo 'The publication checkout does not match the compiler commit' >&2
  exit 1
fi
artifacts=$(cd "$artifacts" && pwd)
repository=medvednikov/v2
tag="vprod-$sha"
platforms=(linux-x64 macos-arm64 macos-x64 windows-x64)

# Validate every input before creating a release or uploading any file.
for platform in "${platforms[@]}"; do
  asset="$sha-$platform"
  test -f "$artifacts/$asset"
  test -f "$artifacts/$asset.sha256"
  (cd "$artifacts" && sha256sum --check --strict "$asset.sha256")
done

if ! gh release view "$tag" --repo "$repository" --json tagName >/dev/null 2>&1; then
  # The source commit need not exist in the destination repository. The release
  # tag identifies it; the notes link to the authoritative source checkout.
  gh release create "$tag" --repo "$repository" --prerelease --latest=false \
    --title "Optimized V compiler $sha" \
    --notes "Optimized CI compilers from https://github.com/vlang/v/commit/$sha. Each binary is named by its full source commit and platform, with a SHA-256 checksum." \
    || gh release view "$tag" --repo "$repository" --json tagName >/dev/null
fi

download_dir=$(mktemp -d "${RUNNER_TEMP:-${TMPDIR:-/tmp}}/publish-vprod.XXXXXX")
trap 'rm -rf -- "$download_dir"' EXIT

asset_exists() {
  local name=$1 names
  names=$(gh release view "$tag" --repo "$repository" --json assets --jq '.assets[].name')
  grep -Fqx -- "$name" <<< "$names"
}

for platform in "${platforms[@]}"; do
  asset="$sha-$platform"
  if asset_exists "$asset"; then
    echo "Reusing the published compiler $asset"
  else
    # No --clobber: another run may publish the same commit concurrently.
    gh release upload "$tag" "$artifacts/$asset" --repo "$repository" \
      || asset_exists "$asset"
  fi

  verify_dir="$download_dir/$platform"
  mkdir "$verify_dir"
  gh release download "$tag" --repo "$repository" --pattern "$asset" --dir "$verify_dir"
  if ! asset_exists "$asset.sha256"; then
    # Also recover an interrupted upload that published the binary but not its
    # checksum. Hash the actual published bytes, never a fresh local rebuild.
    (cd "$verify_dir" && sha256sum "$asset" > "$asset.sha256")
    gh release upload "$tag" "$verify_dir/$asset.sha256" --repo "$repository" \
      || asset_exists "$asset.sha256"
    rm "$verify_dir/$asset.sha256"
  fi
  gh release download "$tag" --repo "$repository" --pattern "$asset.sha256" --dir "$verify_dir"
  (cd "$verify_dir" && sha256sum --check --strict "$asset.sha256")
done
