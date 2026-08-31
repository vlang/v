#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd -P)"
recipe_names=(
  thirdparty-linux-amd64_tcc.sh
  thirdparty-macos-amd64_tcc.sh
  thirdparty-macos-arm64_tcc.sh
  thirdparty-freebsd-amd64_tcc.sh
  thirdparty-openbsd-amd64_tcc.sh
)

fail() {
  echo "tccbin workflow preservation test: $*" >&2
  exit 1
}

for recipe_name in "${recipe_names[@]}"; do
  recipe_path="$script_dir/$recipe_name"
  test -x "$recipe_path" || fail "$recipe_name is not executable"
  test "$(grep -Fc 'TCCBIN_DEFER_COMMIT must be exactly 0 or 1' "$recipe_path")" -eq 1 \
    || fail "$recipe_name does not have one closed defer-mode parser"
  test "$(grep -Fc 'managed tccbin bundles require TCCBIN_DEFER_COMMIT=1' "$recipe_path")" -eq 1 \
    || fail "$recipe_name does not refuse legacy writes to managed bundles"
  test "$(grep -Fc 'rsync -a --delete "$TINYCC_SOURCE/$TCC_FOLDER/" "$TCC_OUTPUT_ROOT/"' "$recipe_path")" -eq 1 \
    || fail "$recipe_name does not use the exact deferred trailing-slash copy"
  test "$(grep -Fc 'mktemp -d "$stage_parent/.tccbin-' "$recipe_path")" -eq 2 \
    || fail "$recipe_name does not create two private sibling directories"
  test "$(grep -Fc 'mv "$TCC_OUTPUT_ROOT" "$TCCBIN_STAGE_ROOT"' "$recipe_path")" -eq 1 \
    || fail "$recipe_name does not have one atomic final promotion"
  if grep -Eq 'control_scan|deferred (output|stage) contains a forbidden control path' \
    "$recipe_path"; then
    fail "$recipe_name must leave raw payload policy to the authenticated preflight"
  fi
  if grep -Fq 'tinycc/$TCC_FOLDER/*' "$recipe_path"; then
    fail "$recipe_name still uses a shell glob as an rsync source"
  fi
  test "$(grep -Ec '^[[:space:]]*(run_git |git )add [.]$' "$recipe_path")" -eq 1 \
    || fail "$recipe_name must contain one legacy add"
  test "$(grep -Ec '^[[:space:]]*(run_git |git )commit -m ' "$recipe_path")" -eq 1 \
    || fail "$recipe_name must contain one legacy commit"
  if grep -Eq 'thirdparty/tcc\.original/\.git' "$recipe_path"; then
    fail "$recipe_name must never copy candidate Git metadata"
  fi
done

test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT HUP INT TERM

shim_dir="$test_root/shims"
mkdir -p "$shim_dir"
real_rsync="$(command -v rsync)"
real_mv="$(command -v mv)"

cat > "$shim_dir/rsync" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
destination=
for argument in "$@"; do
  destination=$argument
done
case "$destination" in
  */.tccbin-output.*|*/.tccbin-output.*/)
    if test "${FIXTURE_RSYNC_FAIL:-0}" = 1; then
      mkdir -p "$destination"
      printf 'partial output\n' > "$destination/partial-output"
      exit 74
    fi
    ;;
esac
exec "$FIXTURE_REAL_RSYNC" "$@"
EOF
chmod 755 "$shim_dir/rsync"

cat > "$shim_dir/mv" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
destination=
for argument in "$@"; do
  destination=$argument
done
if test "${FIXTURE_MV_FAIL:-0}" = 1 \
  && test "$destination" = "$FIXTURE_FINAL_STAGE_ROOT"; then
  exit 75
fi
exec "$FIXTURE_REAL_MV" "$@"
EOF
chmod 755 "$shim_dir/mv"

cat > "$shim_dir/make" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
install=false
for argument in "$@"; do
  if test "$argument" = install; then
    install=true
  fi
done
if test "${FIXTURE_BUILD_FAIL:-0}" = 1; then
  exit 72
fi
if test "$install" != true; then
  exit 0
fi
prefix="$(cat .fixture-prefix)"
case "$prefix" in
  /*) install_root="$prefix" ;;
  *) install_root="$PWD/$prefix" ;;
esac
mkdir -p "$install_root/include" "$install_root/lib"
cat > "$install_root/tcc" <<'TCCEOF'
#!/usr/bin/env sh
if test "${1:-}" = --version; then
  echo 'fixture TinyCC 1.0'
else
  echo 'fixture TinyCC diagnostics' >&2
fi
exit 0
TCCEOF
chmod 755 "$install_root/tcc"
printf 'fixture-header\n' > "$install_root/include/stddef.h"
printf 'fixture-library\n' > "$install_root/lib/libtcc.a"
if test "${FIXTURE_RAW_CONTROLS:-0}" = 1; then
  mkdir -p "$install_root/.github" "$install_root/automation" \
    "$install_root/nested/.github" "$install_root/nested/automation"
  printf 'root github\n' > "$install_root/.github/injected.yml"
  printf 'root automation\n' > "$install_root/automation/injected.txt"
  printf 'nested github\n' > "$install_root/nested/.github/injected.yml"
  printf 'nested automation\n' > "$install_root/nested/automation/injected.txt"
  printf 'raw collision\n' > "$install_root/.tccbin-control-scan"
  chmod 640 "$install_root/.tccbin-control-scan"
  ln -s "$FIXTURE_EXTERNAL_TARGET" "$install_root/nested/.tccbin-control-scan"
fi
EOF
chmod 755 "$shim_dir/make"
ln -s make "$shim_dir/gmake"

cat > "$shim_dir/cc" <<'EOF'
#!/usr/bin/env sh
if test "${1:-}" = --version; then
  echo 'fixture cc 1.0'
fi
exit 0
EOF
chmod 755 "$shim_dir/cc"

git_argv_runner="$test_root/git_argv.sh"
cat > "$git_argv_runner" <<'EOF'
parse_git_argv() { :; }
require_git_executable() { command -v git >/dev/null 2>&1; }
run_git() { command git "$@"; }
EOF
chmod 644 "$git_argv_runner"

tinycc_repo="$test_root/tinycc-source"
mkdir -p "$tinycc_repo/include"
cat > "$tinycc_repo/configure" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
if test "${FIXTURE_CONFIGURE_FAIL:-0}" = 1; then
  exit 71
fi
prefix=
for argument in "$@"; do
  case "$argument" in
    --prefix=*) prefix=${argument#--prefix=} ;;
  esac
done
test -n "$prefix"
printf '%s\n' "$prefix" > .fixture-prefix
EOF
chmod 755 "$tinycc_repo/configure"
printf 'fixture source header\n' > "$tinycc_repo/include/fixture.h"
git -C "$tinycc_repo" init -q
git -C "$tinycc_repo" config user.name fixture
git -C "$tinycc_repo" config user.email fixture@example.invalid
git -C "$tinycc_repo" add .
git -C "$tinycc_repo" commit -q -m fixture
tinycc_sha="$(git -C "$tinycc_repo" rev-parse HEAD)"

create_v_root() {
  local root=$1
  local managed=$2
  local candidate="$root/thirdparty/tcc"
  mkdir -p "$root/vlib/v" "$candidate/.github/workflows" "$candidate/lib"
  printf 'fixture\n' > "$root/vlib/v/compiler_errors_test.v"
  printf 'sentinel: true\n' > "$candidate/.github/workflows/sentinel.yml"
  chmod 640 "$candidate/.github/workflows/sentinel.yml"
  printf 'ignored-control\n' > "$candidate/ignored-control"
  printf 'ignored-control\n' > "$candidate/.gitignore"
  printf '* text eol=lf\n' > "$candidate/.gitattributes"
  printf 'base readme\n' > "$candidate/README.md"
  printf 'old recipe\n' > "$candidate/build.sh"
  printf 'old payload\n' > "$candidate/obsolete.txt"
  printf 'gc overlay\n' > "$candidate/lib/libgc.a"
  printf 'build metadata\n' > "$candidate/lib/build-overlay.txt"
  printf 'patch control\n' > "$candidate/control.patch"
  printf 'transform control\n' > "$candidate/control-transform.patch"
  ln -s control.patch "$candidate/control-link"
  if test "$managed" = true; then
    mkdir -p "$candidate/automation"
    printf '{"schema":"fixture"}\n' > "$candidate/automation/bundle-manifest.json"
  fi
  git -C "$candidate" init -q
  git -C "$candidate" config user.name fixture
  git -C "$candidate" config user.email fixture@example.invalid
  git -C "$candidate" add .
  git -C "$candidate" commit -q -m base
}

candidate_fingerprint() {
  local candidate=$1
  (
    cd "$candidate"
    git rev-parse HEAD
    git ls-files -s
    git status --porcelain=v1 --ignored=matching -uall
    find . -path './.git' -prune -o \( -type f -o -type l \) -print \
      | LC_ALL=C sort \
      | while IFS= read -r path; do
          if test -L "$path"; then
            printf 'link %s %s\n' "$path" "$(readlink "$path")"
          else
            printf 'file %s %s %s\n' "$path" "$(stat -c '%a' "$path")" \
              "$(sha256sum "$path" | awk '{print $1}')"
          fi
        done
  )
}

run_recipe() {
  local recipe_path=$1
  local v_root=$2
  local defer_mode=$3
  local stage_root=$4
  local source_repo=$5
  local tcc_folder=$6
  shift 6
  (
    cd "$v_root"
    export PATH="$shim_dir:$PATH"
    export BUILD_CMD=fixture-build
    export CC="$shim_dir/cc"
    export GIT_ALLOW_PROTOCOL=file
    export GIT_ARGV_RUNNER="$git_argv_runner"
    export GIT_NO_LAZY_FETCH=1
    export GIT_TERMINAL_PROMPT=0
    export FIXTURE_REAL_RSYNC="$real_rsync"
    export FIXTURE_REAL_MV="$real_mv"
    export FIXTURE_FINAL_STAGE_ROOT="$stage_root"
    export TCC_COMMIT="$tinycc_sha"
    export TCC_FOLDER="$tcc_folder"
    export TCC_REPO="$source_repo"
    export TCCBIN_DEFER_COMMIT="$defer_mode"
    export MACOSX_DEPLOYMENT_TARGET=10.13
    if test "$stage_root" = __unset__; then
      unset TCCBIN_STAGE_ROOT
    else
      export TCCBIN_STAGE_ROOT="$stage_root"
    fi
    bash "$recipe_path" "$@"
  )
}

expect_recipe_failure() {
  local log=$1
  local expected_diagnostic=$2
  shift 2
  if "$@" > "$log" 2>&1; then
    cat "$log" >&2
    fail "recipe unexpectedly succeeded"
  fi
  diagnostic_count="$(grep -Fxc -- "$expected_diagnostic" "$log" || true)"
  test "$diagnostic_count" -eq 1 || {
    cat "$log" >&2
    fail "recipe failure did not emit exactly one expected diagnostic: $expected_diagnostic"
  }
}

managed_root="$test_root/managed-v"
create_v_root "$managed_root" true
managed_candidate="$managed_root/thirdparty/tcc"
managed_before="$test_root/managed-before.txt"
candidate_fingerprint "$managed_candidate" > "$managed_before"
mkdir -p "$managed_root/tinycc" "$managed_root/thirdparty/tcc.original"
printf 'must survive\n' > "$managed_root/tinycc/preflight-sentinel"
printf 'must survive\n' > "$managed_root/thirdparty/tcc.original/preflight-sentinel"

stage_parent="$test_root/stages"
mkdir -p "$stage_parent"
external_collision_target="$test_root/external-collision-target"
printf 'external target must stay unchanged\n' > "$external_collision_target"
external_collision_before="$(sha256sum "$external_collision_target" | awk '{print $1}')"

export FIXTURE_RAW_CONTROLS=1
export FIXTURE_EXTERNAL_TARGET="$external_collision_target"
for recipe_name in "${recipe_names[@]}"; do
  recipe_path="$script_dir/$recipe_name"
  case_name=${recipe_name%.sh}
  stage_root="$stage_parent/$case_name"
  run_recipe "$recipe_path" "$managed_root" 1 "$stage_root" "$tinycc_repo" \
    thirdparty/tcc > "$test_root/$case_name.defer.log" 2>&1

  candidate_fingerprint "$managed_candidate" > "$test_root/$case_name.after.txt"
  cmp -s "$managed_before" "$test_root/$case_name.after.txt" \
    || fail "$recipe_name changed the managed candidate in deferred mode"
  test -f "$managed_root/tinycc/preflight-sentinel" \
    || fail "$recipe_name touched the V-root TinyCC directory in deferred mode"
  test -f "$managed_root/thirdparty/tcc.original/preflight-sentinel" \
    || fail "$recipe_name touched the candidate backup path in deferred mode"
  test -x "$stage_root/tcc.exe" || fail "$recipe_name did not produce staged tcc.exe"
  test "$(cat "$stage_root/build_source_hash.txt")" = "$tinycc_sha" \
    || fail "$recipe_name staged the wrong TinyCC source hash"
  test -n "$(sha256sum "$stage_root/tcc.exe" | awk '{print $1}')" \
    || fail "$recipe_name did not produce a hashable tcc.exe"
  test ! -e "$stage_root/build.sh" || fail "$recipe_name leaked its recipe into payload"
  test ! -e "$stage_root/control.patch" || fail "$recipe_name copied a patch control"
  test ! -e "$stage_root/control-transform.patch" \
    || fail "$recipe_name copied a transform control"
  test "$(cat "$stage_root/.github/injected.yml")" = 'root github' \
    || fail "$recipe_name changed a raw root .github control"
  test "$(cat "$stage_root/automation/injected.txt")" = 'root automation' \
    || fail "$recipe_name changed a raw root automation control"
  test "$(cat "$stage_root/nested/.github/injected.yml")" = 'nested github' \
    || fail "$recipe_name changed a raw nested .github control"
  test "$(cat "$stage_root/nested/automation/injected.txt")" = 'nested automation' \
    || fail "$recipe_name changed a raw nested automation control"
  test "$(cat "$stage_root/.tccbin-control-scan")" = 'raw collision' \
    || fail "$recipe_name changed the raw collision file"
  test "$(stat -c '%a' "$stage_root/.tccbin-control-scan")" = 640 \
    || fail "$recipe_name changed the raw collision file mode"
  test -L "$stage_root/nested/.tccbin-control-scan" \
    || fail "$recipe_name followed the raw collision symlink"
  test "$(readlink "$stage_root/nested/.tccbin-control-scan")" = \
    "$external_collision_target" \
    || fail "$recipe_name changed the raw collision symlink target"
  test "$(sha256sum "$external_collision_target" | awk '{print $1}')" = \
    "$external_collision_before" \
    || fail "$recipe_name mutated the external raw collision target"
  residual_siblings=("$stage_parent"/.tccbin-build.* "$stage_parent"/.tccbin-output.*)
  for residual_sibling in "${residual_siblings[@]}"; do
    test ! -e "$residual_sibling" && test ! -L "$residual_sibling" \
      || fail "$recipe_name left a private sibling after atomic promotion"
  done
done
unset FIXTURE_RAW_CONTROLS FIXTURE_EXTERNAL_TARGET

candidate_fingerprint "$managed_candidate" > "$test_root/managed-after-all.txt"
cmp -s "$managed_before" "$test_root/managed-after-all.txt" \
  || fail "deferred matrix changed managed candidate state"

mkdir "$stage_parent/physical-parent"
ln -s "$stage_parent/physical-parent" "$stage_parent/linked-parent"
for recipe_name in "${recipe_names[@]}"; do
  recipe_path="$script_dir/$recipe_name"
  case_name=${recipe_name%.sh}
  invalid_stage="$stage_parent/$case_name.invalid-mode"
  preexisting_stage="$stage_parent/$case_name.preexisting"
  symlink_stage="$stage_parent/$case_name.symlink"

  expect_recipe_failure "$test_root/$case_name.invalid-mode.log" \
    'TCCBIN_DEFER_COMMIT must be exactly 0 or 1' \
    run_recipe "$recipe_path" "$managed_root" 2 "$invalid_stage" "$tinycc_repo" \
    thirdparty/tcc
  test ! -e "$invalid_stage"

  expect_recipe_failure "$test_root/$case_name.missing-stage.log" \
    'deferred builds require TCCBIN_STAGE_ROOT' \
    run_recipe "$recipe_path" "$managed_root" 1 __unset__ "$tinycc_repo" thirdparty/tcc

  expect_recipe_failure "$test_root/$case_name.relative-stage.log" \
    'TCCBIN_STAGE_ROOT must be an absolute physical path' \
    run_recipe "$recipe_path" "$managed_root" 1 relative-stage "$tinycc_repo" \
    thirdparty/tcc

  mkdir "$preexisting_stage"
  expect_recipe_failure "$test_root/$case_name.preexisting-stage.log" \
    'TCCBIN_STAGE_ROOT must not already exist' \
    run_recipe "$recipe_path" "$managed_root" 1 "$preexisting_stage" "$tinycc_repo" \
    thirdparty/tcc

  ln -s "$stage_parent/missing-$case_name" "$symlink_stage"
  expect_recipe_failure "$test_root/$case_name.symlink-stage.log" \
    'TCCBIN_STAGE_ROOT must not already exist' \
    run_recipe "$recipe_path" "$managed_root" 1 "$symlink_stage" "$tinycc_repo" \
    thirdparty/tcc

  expect_recipe_failure "$test_root/$case_name.linked-parent.log" \
    'TCCBIN_STAGE_ROOT must name a physical, normalized child' \
    run_recipe "$recipe_path" "$managed_root" 1 "$stage_parent/linked-parent/$case_name" \
    "$tinycc_repo" thirdparty/tcc

  expect_recipe_failure "$test_root/$case_name.v-root-overlap.log" \
    'TCCBIN_STAGE_ROOT must be outside the V and candidate repositories' \
    run_recipe "$recipe_path" "$managed_root" 1 "$managed_root/stage-$case_name" \
    "$tinycc_repo" thirdparty/tcc

  expect_recipe_failure "$test_root/$case_name.candidate-overlap.log" \
    'TCCBIN_STAGE_ROOT must be outside the V and candidate repositories' \
    run_recipe "$recipe_path" "$managed_root" 1 "$managed_candidate/stage-$case_name" \
    "$tinycc_repo" thirdparty/tcc

  expect_recipe_failure "$test_root/$case_name.wrong-folder.log" \
    'deferred builds require TCC_FOLDER=thirdparty/tcc' \
    run_recipe "$recipe_path" "$managed_root" 1 "$stage_parent/$case_name.wrong-folder" \
    "$tinycc_repo" thirdparty/other

  expect_recipe_failure "$test_root/$case_name.positional-argument.log" \
    'this script accepts no positional arguments; use TCCBIN_DEFER_COMMIT=1' \
    run_recipe "$recipe_path" "$managed_root" 1 "$stage_parent/$case_name.positional" \
    "$tinycc_repo" thirdparty/tcc --defer-commit

  expect_recipe_failure "$test_root/$case_name.managed-legacy.log" \
    'managed tccbin bundles require TCCBIN_DEFER_COMMIT=1' \
    run_recipe "$recipe_path" "$managed_root" 0 __unset__ "$tinycc_repo" \
    thirdparty/tcc

  alternate_candidate="$managed_root/thirdparty/alternate-$case_name"
  mkdir "$alternate_candidate"
  printf 'alternate sentinel\n' > "$alternate_candidate/sentinel"
  alternate_before="$(sha256sum "$alternate_candidate/sentinel" | awk '{print $1}')"
  expect_recipe_failure "$test_root/$case_name.managed-legacy-alternate.log" \
    'managed tccbin bundles require TCCBIN_DEFER_COMMIT=1' \
    run_recipe "$recipe_path" "$managed_root" 0 __unset__ "$tinycc_repo" \
    "thirdparty/alternate-$case_name"
  test "$(sha256sum "$alternate_candidate/sentinel" | awk '{print $1}')" = \
    "$alternate_before" \
    || fail "$recipe_name changed the alternate legacy target before refusing it"
done

candidate_fingerprint "$managed_candidate" > "$test_root/managed-after-negatives.txt"
cmp -s "$managed_before" "$test_root/managed-after-negatives.txt" \
  || fail "validation failures changed managed candidate state"
test -f "$managed_root/tinycc/preflight-sentinel"
test -f "$managed_root/thirdparty/tcc.original/preflight-sentinel"

symlink_candidate_v_root="$test_root/symlink-candidate-v"
mkdir -p "$symlink_candidate_v_root/vlib/v" "$symlink_candidate_v_root/thirdparty"
printf 'fixture\n' > "$symlink_candidate_v_root/vlib/v/compiler_errors_test.v"
ln -s "$managed_candidate" "$symlink_candidate_v_root/thirdparty/tcc"
for recipe_name in "${recipe_names[@]}"; do
  recipe_path="$script_dir/$recipe_name"
  case_name=${recipe_name%.sh}
  symlink_candidate_stage="$stage_parent/$case_name.symlink-candidate"
  expect_recipe_failure "$test_root/$case_name.symlink-candidate.log" \
    'candidate tccbin root must be a physical directory' \
    run_recipe "$recipe_path" "$symlink_candidate_v_root" 1 "$symlink_candidate_stage" \
    "$tinycc_repo" thirdparty/tcc
  test ! -e "$symlink_candidate_stage" \
    || fail "$recipe_name created a stage before rejecting a symlink candidate"
done

candidate_fingerprint "$managed_candidate" > "$test_root/managed-after-symlink-candidate.txt"
cmp -s "$managed_before" "$test_root/managed-after-symlink-candidate.txt" \
  || fail "symlink candidate failures changed the external candidate target"

assert_atomic_failure_cleanup() {
  local recipe_name=$1
  local stage_root=$2
  test ! -e "$stage_root" && test ! -L "$stage_root" \
    || fail "$recipe_name exposed a final stage after a deferred failure"
  local residual_sibling
  local residual_siblings=("$stage_parent"/.tccbin-build.* "$stage_parent"/.tccbin-output.*)
  for residual_sibling in "${residual_siblings[@]}"; do
    test ! -e "$residual_sibling" && test ! -L "$residual_sibling" \
      || fail "$recipe_name left a private sibling after a deferred failure"
  done
}

for recipe_name in "${recipe_names[@]}"; do
  recipe_path="$script_dir/$recipe_name"
  case_name=${recipe_name%.sh}

  configure_failure_stage="$stage_parent/$case_name.configure-failure"
  export FIXTURE_CONFIGURE_FAIL=1
  expect_recipe_failure "$test_root/$case_name.configure-failure.log" \
    'deferred TinyCC build failed' \
    run_recipe "$recipe_path" "$managed_root" 1 "$configure_failure_stage" \
    "$tinycc_repo" thirdparty/tcc
  unset FIXTURE_CONFIGURE_FAIL
  assert_atomic_failure_cleanup "$recipe_name" "$configure_failure_stage"

  build_failure_stage="$stage_parent/$case_name.build-failure"
  export FIXTURE_BUILD_FAIL=1
  expect_recipe_failure "$test_root/$case_name.build-failure.log" \
    'deferred TinyCC build failed' \
    run_recipe "$recipe_path" "$managed_root" 1 "$build_failure_stage" "$tinycc_repo" \
    thirdparty/tcc
  unset FIXTURE_BUILD_FAIL
  assert_atomic_failure_cleanup "$recipe_name" "$build_failure_stage"

  rsync_failure_stage="$stage_parent/$case_name.rsync-failure"
  export FIXTURE_RSYNC_FAIL=1
  expect_recipe_failure "$test_root/$case_name.rsync-failure.log" \
    'deferred output copy failed' \
    run_recipe "$recipe_path" "$managed_root" 1 "$rsync_failure_stage" "$tinycc_repo" \
    thirdparty/tcc
  unset FIXTURE_RSYNC_FAIL
  assert_atomic_failure_cleanup "$recipe_name" "$rsync_failure_stage"

  promotion_failure_stage="$stage_parent/$case_name.promotion-failure"
  export FIXTURE_MV_FAIL=1
  expect_recipe_failure "$test_root/$case_name.promotion-failure.log" \
    'deferred stage promotion failed' \
    run_recipe "$recipe_path" "$managed_root" 1 "$promotion_failure_stage" \
    "$tinycc_repo" thirdparty/tcc
  unset FIXTURE_MV_FAIL
  assert_atomic_failure_cleanup "$recipe_name" "$promotion_failure_stage"
done

candidate_fingerprint "$managed_candidate" > "$test_root/managed-after-partial.txt"
cmp -s "$managed_before" "$test_root/managed-after-partial.txt" \
  || fail "atomic deferred failures changed managed candidate state"
test "$(sha256sum "$external_collision_target" | awk '{print $1}')" = \
  "$external_collision_before" \
  || fail "atomic deferred failures mutated the external collision target"

for recipe_name in "${recipe_names[@]}"; do
  recipe_path="$script_dir/$recipe_name"
  case_name=${recipe_name%.sh}
  legacy_root="$test_root/legacy-$case_name"
  create_v_root "$legacy_root" false
  legacy_candidate="$legacy_root/thirdparty/tcc"
  legacy_head="$(git -C "$legacy_candidate" rev-parse HEAD)"
  legacy_workflow_sha="$(sha256sum "$legacy_candidate/.github/workflows/sentinel.yml" | awk '{print $1}')"
  legacy_gitignore_sha="$(sha256sum "$legacy_candidate/.gitignore" | awk '{print $1}')"
  legacy_gitattributes_sha="$(sha256sum "$legacy_candidate/.gitattributes" | awk '{print $1}')"

  run_recipe "$recipe_path" "$legacy_root" 0 __unset__ "$tinycc_repo" thirdparty/tcc \
    > "$test_root/$case_name.legacy.log" 2>&1

  test "$(git -C "$legacy_candidate" rev-list --count "$legacy_head"..HEAD)" -eq 1 \
    || fail "$recipe_name legacy mode did not create exactly one commit"
  test "$(git -C "$legacy_candidate" log -1 --format=%s)" = 'build with `fixture-build`' \
    || fail "$recipe_name legacy commit message changed"
  test -z "$(git -C "$legacy_candidate" status --porcelain=v1 -uno)" \
    || fail "$recipe_name left tracked legacy changes"
  test "$(sha256sum "$legacy_candidate/.github/workflows/sentinel.yml" | awk '{print $1}')" = "$legacy_workflow_sha"
  test "$(sha256sum "$legacy_candidate/.gitignore" | awk '{print $1}')" = "$legacy_gitignore_sha"
  test "$(sha256sum "$legacy_candidate/.gitattributes" | awk '{print $1}')" = "$legacy_gitattributes_sha"
  test -x "$legacy_candidate/tcc.exe" || fail "$recipe_name legacy output is missing"
done

echo 'tccbin recipe deferred/legacy preservation contract: PASS'
