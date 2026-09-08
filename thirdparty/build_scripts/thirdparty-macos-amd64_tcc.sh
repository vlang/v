## NOTE: this script does *not* use a shebang **deliberately**, in order to
## access the same shell, to capture its own launching command with `fc`,
## and to record it later in the new commit message in thirdpart/tcc.

## WARNING: THE ORIGINAL OF THIS SCRIPT IS IN:
## https://github.com/vlang/v/blob/master/thirdparty/build_scripts/thirdparty-macos-amd64_tcc.sh ,
## I.E. IN THE MAIN V REPOSITORY. IF YOU NEED TO MAKE CHANGES, CHANGE THAT.
##
## THE `build.sh` FILE IN `vlang/tccbin` REPO IS A COPY, RECORDED AT THE TIME
## OF REBUILDING, FOR EASIER/RELIABLE REPRODUCTION OF HISTORIC VERSIONS.
## IT IS NOT INTENDED TO BE MODIFIED.

set -e

if test "$#" -ne 0; then
  echo "this script accepts no positional arguments; use TCCBIN_DEFER_COMMIT=1" >&2
  exit 2
fi
TCCBIN_DEFER_COMMIT="${TCCBIN_DEFER_COMMIT:-0}"
case "$TCCBIN_DEFER_COMMIT" in
  0|1) ;;
  *)
    echo "TCCBIN_DEFER_COMMIT must be exactly 0 or 1" >&2
    exit 2
    ;;
esac

if test -z "$BUILD_CMD"; then
  BUILD_CMD="$(fc -nl -0 2>/dev/null || true)"
fi
## remove whitespaces before/after the actual command:
BUILD_CMD="$(echo "${BUILD_CMD}" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
if test -z "$BUILD_CMD"; then
  BUILD_CMD="$0"
fi

## make sure that commands use English in their output, instead of the local system's local:
export LANG=C

if ! test -f vlib/v/compiler_errors_test.v; then
  echo "this script should be run in V's main repo folder!"
  exit 1
fi

export CURRENT_SCRIPT_PATH=$(realpath "$0")

export TCC_COMMIT="${TCC_COMMIT:-mob}"
export TCC_FOLDER="${TCC_FOLDER:-thirdparty/tcc.$TCC_COMMIT}"
export TCC_REPO="${TCC_REPO:-https://repo.or.cz/tinycc.git}"
export CC="${CC:-clang}"
## Neither half of the tcc.exe/libgc pair pinned a deployment target
## before this - without one, a rebuilt tcc.exe silently inherits the
## CI runner's own macOS floor. Pin the same default here as
## thirdparty-macos-amd64_bdwgc.sh uses for the GC half of the pair,
## and fold it into CFLAGS explicitly - tcc's own build (unlike
## bdwgc's autotools one) doesn't necessarily honor the bare env var.
export MACOSX_DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:-10.13}"
export CFLAGS="-O3 -mmacosx-version-min=$MACOSX_DEPLOYMENT_TARGET"

V_REPO_ROOT="$(pwd -P)"
candidate_root="$V_REPO_ROOT/thirdparty/tcc"
TINYCC_SOURCE=tinycc
TCC_OUTPUT_ROOT="$TCC_FOLDER"
TINYCC_BUILD_ROOT=
TCCBIN_WORK_ROOT=
TCCBIN_DEFER_COMPLETE=0

if test "$TCCBIN_DEFER_COMMIT" = 1; then
  if test "$TCC_FOLDER" != thirdparty/tcc; then
    echo "deferred builds require TCC_FOLDER=thirdparty/tcc" >&2
    exit 2
  fi
  if test -L "$candidate_root" || ! test -d "$candidate_root" \
    || test "$(cd "$candidate_root" && pwd -P)" != "$candidate_root"; then
    echo "candidate tccbin root must be a physical directory" >&2
    exit 2
  fi
  if test -z "${TCCBIN_STAGE_ROOT:-}"; then
    echo "deferred builds require TCCBIN_STAGE_ROOT" >&2
    exit 2
  fi
  case "$TCCBIN_STAGE_ROOT" in
    /*) ;;
    *)
      echo "TCCBIN_STAGE_ROOT must be an absolute physical path" >&2
      exit 2
      ;;
  esac
  case "$TCCBIN_STAGE_ROOT" in
    *$'\n'*|*$'\r'*|*$'\t'*)
      echo "TCCBIN_STAGE_ROOT contains a forbidden control character" >&2
      exit 2
      ;;
  esac
  stage_parent="$(dirname "$TCCBIN_STAGE_ROOT")"
  stage_leaf="$(basename "$TCCBIN_STAGE_ROOT")"
  case "$stage_leaf" in
    ''|.|..|/)
      echo "TCCBIN_STAGE_ROOT has an unsafe final component" >&2
      exit 2
      ;;
  esac
  if ! test -d "$stage_parent"; then
    echo "TCCBIN_STAGE_ROOT parent must already exist" >&2
    exit 2
  fi
  stage_parent_physical="$(cd "$stage_parent" && pwd -P)"
  if test "$stage_parent_physical" = /; then
    expected_stage_root="/$stage_leaf"
  else
    expected_stage_root="$stage_parent_physical/$stage_leaf"
  fi
  if test "$TCCBIN_STAGE_ROOT" != "$expected_stage_root"; then
    echo "TCCBIN_STAGE_ROOT must name a physical, normalized child" >&2
    exit 2
  fi
  case "$TCCBIN_STAGE_ROOT/" in
    "$V_REPO_ROOT/"*|"$candidate_root/"*)
      echo "TCCBIN_STAGE_ROOT must be outside the V and candidate repositories" >&2
      exit 2
      ;;
  esac
  if test -e "$TCCBIN_STAGE_ROOT" || test -L "$TCCBIN_STAGE_ROOT"; then
    echo "TCCBIN_STAGE_ROOT must not already exist" >&2
    exit 2
  fi
else
  if test -n "${TCCBIN_STAGE_ROOT:-}"; then
    echo "TCCBIN_STAGE_ROOT is only valid with TCCBIN_DEFER_COMMIT=1" >&2
    exit 2
  fi
  if test -e "$candidate_root/automation/bundle-manifest.json" \
    || test -L "$candidate_root/automation/bundle-manifest.json"; then
    echo "managed tccbin bundles require TCCBIN_DEFER_COMMIT=1" >&2
    exit 2
  fi
fi

echo "                      BUILD_CMD: \`$BUILD_CMD\`"
echo "                             CC: $CC"
echo "                     TCC_COMMIT: $TCC_COMMIT"
echo "                     TCC_FOLDER: \`$TCC_FOLDER\`"
echo "       MACOSX_DEPLOYMENT_TARGET: $MACOSX_DEPLOYMENT_TARGET"
echo ===============================================================

if test "$TCCBIN_DEFER_COMMIT" = 1; then
  TCC_OUTPUT_ROOT=
  cleanup_deferred_build() {
    cleanup_status=$?
    if test "$TCCBIN_DEFER_COMPLETE" != 1; then
      cd "$stage_parent" 2>/dev/null || true
      test -z "$TCC_OUTPUT_ROOT" || rm -rf -- "$TCC_OUTPUT_ROOT" || true
      test -z "$TCCBIN_WORK_ROOT" || rm -rf -- "$TCCBIN_WORK_ROOT" || true
    fi
    exit "$cleanup_status"
  }
  trap 'cleanup_deferred_build' EXIT
  trap 'exit 129' HUP
  trap 'exit 130' INT
  trap 'exit 143' TERM
  if ! TCCBIN_WORK_ROOT="$(mktemp -d "$stage_parent/.tccbin-build.XXXXXX")"; then
    echo "deferred workspace creation failed" >&2
    exit 2
  fi
  if ! TCC_OUTPUT_ROOT="$(mktemp -d "$stage_parent/.tccbin-output.XXXXXX")"; then
    echo "deferred output creation failed" >&2
    exit 2
  fi
  if test -L "$TCCBIN_WORK_ROOT" || ! test -d "$TCCBIN_WORK_ROOT" \
    || test "$(cd "$TCCBIN_WORK_ROOT" && pwd -P)" != "$TCCBIN_WORK_ROOT" \
    || test -L "$TCC_OUTPUT_ROOT" || ! test -d "$TCC_OUTPUT_ROOT" \
    || test "$(cd "$TCC_OUTPUT_ROOT" && pwd -P)" != "$TCC_OUTPUT_ROOT"; then
    echo "deferred workspace or output is not physical" >&2
    exit 2
  fi
  TINYCC_BUILD_ROOT="$TCCBIN_WORK_ROOT"
  TINYCC_SOURCE="$TINYCC_BUILD_ROOT/tinycc"
else
  rm -rf tinycc/
  rm -rf thirdparty/tcc.original/
  rsync -a --exclude='/.git' --exclude='/.git/' \
    thirdparty/tcc/ thirdparty/tcc.original/
fi
## rm -rf $TCC_FOLDER

pushd .

git clone "$TCC_REPO" "$TINYCC_SOURCE"

cd "$TINYCC_SOURCE"

git checkout $TCC_COMMIT
export TCC_COMMIT_FULL_HASH=$(git rev-parse HEAD)

### NB: the symlinks below are needed, to ensure proper support for bootstrapping tcc, otherwise backtraces will be disabled .
for i in include/*.h; do echo $i; ln -s $i $(basename $i); done

#	    --libdir=$TCC_FOLDER/lib \

deferred_tinycc_build_failed() {
  echo "deferred TinyCC build failed" >&2
  exit 2
}

configure_tinycc() {
  ./configure \
        --prefix=$TCC_FOLDER \
        --bindir=$TCC_FOLDER \
	    --tccdir=$TCC_FOLDER/lib \
        --includedir=$TCC_FOLDER/include \
        --crtprefix=$TCC_FOLDER/lib:/usr/lib \
        --sysincludepaths=$TCC_FOLDER/include:$TCC_FOLDER/lib/include:/usr/local/include:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include:/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include \
        --libpaths=$TCC_FOLDER/lib:/usr/local/lib:/usr/lib:/lib \
	    --config-new_macho=yes \
	    --config-codesign \
        --cc="$CC" \
        --extra-cflags="$CFLAGS" \
	    --extra-ldflags="-mmacosx-version-min=$MACOSX_DEPLOYMENT_TARGET" \
	    --config-bcheck=yes \
	    --config-backtrace=yes \
	    --enable-static \
	    --dwarf=5 \
        --debug
}

if test "$TCCBIN_DEFER_COMMIT" = 1; then
  if ! configure_tinycc; then
    deferred_tinycc_build_failed
  fi
else
  configure_tinycc
fi

if test "$TCCBIN_DEFER_COMMIT" = 1; then
  if ! gmake MACOSX_DEPLOYMENT_TARGET="$MACOSX_DEPLOYMENT_TARGET" \
    || ! gmake MACOSX_DEPLOYMENT_TARGET="$MACOSX_DEPLOYMENT_TARGET" install; then
    deferred_tinycc_build_failed
  fi
else
  gmake MACOSX_DEPLOYMENT_TARGET="$MACOSX_DEPLOYMENT_TARGET"
  gmake MACOSX_DEPLOYMENT_TARGET="$MACOSX_DEPLOYMENT_TARGET" install
fi

popd

if test "$TCCBIN_DEFER_COMMIT" = 1; then
  if ! rsync -a --delete "$TINYCC_SOURCE/$TCC_FOLDER/" "$TCC_OUTPUT_ROOT/"; then
    echo "deferred output copy failed" >&2
    exit 2
  fi
else
  rsync -a --delete \
    --exclude='/.git' --exclude='/.git/' --exclude='/.github/' \
    --exclude='/.gitignore' --exclude='/.gitattributes' \
    "$TINYCC_SOURCE/$TCC_FOLDER/" "$TCC_FOLDER/"
  rsync -a thirdparty/tcc.original/lib/libgc* "$TCC_FOLDER/lib/"
  for build_file in thirdparty/tcc.original/lib/build*; do
    if test -e "$build_file"; then
      rsync -a "$build_file" "$TCC_FOLDER/lib/"
    fi
  done
  rsync -a thirdparty/tcc.original/README.md "$TCC_FOLDER/README.md"
  rsync -a "$CURRENT_SCRIPT_PATH" "$TCC_FOLDER/build.sh"
fi
mv "$TCC_OUTPUT_ROOT/tcc" "$TCC_OUTPUT_ROOT/tcc.exe"

date                                                > "$TCC_OUTPUT_ROOT/build_on_date.txt"
echo "$TCC_COMMIT_FULL_HASH"                        > "$TCC_OUTPUT_ROOT/build_source_hash.txt"
"$TCC_OUTPUT_ROOT/tcc.exe" --version                > "$TCC_OUTPUT_ROOT/build_version.txt"
uname -a                                            > "$TCC_OUTPUT_ROOT/build_machine_uname.txt"
echo "$MACOSX_DEPLOYMENT_TARGET"                    > "$TCC_OUTPUT_ROOT/build_macosx_deployment_target.txt"
$CC --version                                       > "$TCC_OUTPUT_ROOT/build_toolchain_identity.txt"

## needed for Big Sur
ln -s /System/DriverKit/usr/lib/libSystem.dylib "$TCC_OUTPUT_ROOT/lib/libc.dylib"

## show the builtin search paths for sanity checking:
"$TCC_OUTPUT_ROOT/tcc.exe" -v -v

if test "$TCCBIN_DEFER_COMMIT" = 1; then
  if test -e "$TCCBIN_STAGE_ROOT" || test -L "$TCCBIN_STAGE_ROOT" \
    || test -L "$TCC_OUTPUT_ROOT" || ! test -d "$TCC_OUTPUT_ROOT" \
    || test "$(cd "$TCC_OUTPUT_ROOT" && pwd -P)" != "$TCC_OUTPUT_ROOT"; then
    echo "deferred output identity changed during the build" >&2
    exit 2
  fi
  if ! rm -rf -- "$TCCBIN_WORK_ROOT"; then
    echo "deferred workspace cleanup failed" >&2
    exit 2
  fi
  TCCBIN_WORK_ROOT=
  if ! mv "$TCC_OUTPUT_ROOT" "$TCCBIN_STAGE_ROOT"; then
    echo "deferred stage promotion failed" >&2
    exit 2
  fi
  TCC_OUTPUT_ROOT=
  TCCBIN_DEFER_COMPLETE=1
  trap - EXIT HUP INT TERM
  exit 0
else
  pushd .
  cd "$TCC_FOLDER"
  git add .
  git commit -m "build with \`$BUILD_CMD\`"
  popd
fi

echo "tcc commit: $TCC_COMMIT , full hash: $TCC_COMMIT_FULL_HASH ."
echo "The tcc executable is ready in $TCC_FOLDER/tcc.exe"
