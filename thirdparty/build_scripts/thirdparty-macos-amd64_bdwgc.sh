#!/usr/bin/env bash

set -e

if ! test -f vlib/v/compiler_errors_test.v; then
  echo "this script should be run in V's main repo folder!"
  exit 1
fi

## ensure all output is in English, independent from the current user's local:
export LANG=C

export CURRENT_SCRIPT_PATH=$(realpath "$0")

## CC defaults to clang here (not arm64_bdwgc.sh's gcc default) - this
## matches the exact toolchain vlang/tccbin#74 already proved works for
## a real, CI-validated macOS-amd64 libgc rebuild; not a guess.
export CC="${CC:-clang}"
export TCC_FOLDER="${TCC_FOLDER:-thirdparty/tcc}"
## Callers should pass an already-resolved commit SHA here, not a
## floating ref - the caller (update_tccbin.yml) resolves bdwgc's
## commit once, before deciding whether a rebuild is even needed, and
## must pass that exact resolved SHA through so the build can't
## silently drift from what the skip-check just fingerprinted (a
## `master` re-resolved independently at build time could have moved
## in between). The `master` default here exists only for local/manual
## invocation outside that workflow.
export LIBGC_COMMIT="${LIBGC_COMMIT:-master}"
## Same reasoning as LIBGC_COMMIT above: pass the already-resolved
## libatomic_ops SHA through explicitly and check it out, rather than
## just cloning its default branch and recording whatever HEAD happens
## to land - otherwise the resolved hash the caller fingerprinted and
## the commit actually built could silently disagree.
export LIBATOMIC_OPS_COMMIT="${LIBATOMIC_OPS_COMMIT:-master}"
## Neither half of the tcc.exe/libgc pair pins a deployment target
## today, so a rebuilt binary silently inherits the CI runner's own
## macOS floor. Pin one explicitly here; thirdparty-macos-amd64_tcc.sh
## pins the same default for the tcc.exe half of the pair.
export MACOSX_DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:-10.13}"
## Bump this whenever this script's (or its validate-script sibling's)
## own logic materially changes, independent of any upstream SHA - lets
## the caller's rebuild-fingerprint check force a rebuild on a recipe
## change alone, e.g. a fixed configure flag, even when bdwgc/
## libatomic_ops/TinyCC haven't moved.
export RECIPE_VERSION="${RECIPE_VERSION:-1}"
mkdir -p $TCC_FOLDER/lib/

echo "                      CC: $CC"
echo "              TCC_FOLDER: $TCC_FOLDER"
echo "            LIBGC_COMMIT: $LIBGC_COMMIT"
echo "   LIBATOMIC_OPS_COMMIT: $LIBATOMIC_OPS_COMMIT"
echo "MACOSX_DEPLOYMENT_TARGET: $MACOSX_DEPLOYMENT_TARGET"
echo "          RECIPE_VERSION: $RECIPE_VERSION"
echo ===============================================================

## A prior rebuild's libgc-family files (any SONAME, any extension)
## must not linger alongside a freshly-built set - if libtool ever
## bumps the SONAME (e.g. libgc.1.dylib -> libgc.2.dylib), an orphaned
## old-versioned file left in place would be dead weight at best and a
## confusing double-provider at worst. Clear the whole family before
## staging the new build's output below.
rm -f $TCC_FOLDER/lib/libgc*.dylib $TCC_FOLDER/lib/libgc*.a $TCC_FOLDER/lib/libgc.la $TCC_FOLDER/lib/libgc.lai

rm -rf bdwgc/

pushd .
git clone https://github.com/ivmai/bdwgc
cd bdwgc/

git checkout $LIBGC_COMMIT
export LIBGC_COMMIT_FULL_HASH=$(git rev-parse HEAD)

## Check out the explicitly-resolved commit, same as LIBGC_COMMIT
## above - previously this cloned the default branch and recorded
## whatever HEAD it happened to land on, which could silently disagree
## with whatever hash the caller's rebuild-fingerprint check resolved
## and recorded moments earlier.
git clone https://github.com/bdwgc/libatomic_ops
git -C libatomic_ops checkout $LIBATOMIC_OPS_COMMIT
export LIBATOMIC_OPS_COMMIT_FULL_HASH=$(git -C libatomic_ops rev-parse HEAD)

./autogen.sh

export CONFIGURE_CMD="MACOSX_DEPLOYMENT_TARGET=\"$MACOSX_DEPLOYMENT_TARGET\" CC=\"$CC\" CFLAGS=\"-Os -mtune=generic -fPIC\" LDFLAGS=\"-Os -fPIC\" ./configure --disable-dependency-tracking --disable-docs --enable-static=yes --enable-shared=yes --enable-single-obj-compilation --enable-gc-debug --enable-thread-local-alloc --enable-large-config --enable-cplusplus --with-libatomic-ops=check --enable-sigrt-signals"

MACOSX_DEPLOYMENT_TARGET=$MACOSX_DEPLOYMENT_TARGET CC=$CC CFLAGS="-Os -mtune=generic -fPIC" LDFLAGS="-Os -fPIC" ./configure \
	--disable-dependency-tracking \
	--disable-docs \
	--enable-static=yes \
	--enable-shared=yes \
	--enable-single-obj-compilation \
	--enable-gc-debug \
	--enable-thread-local-alloc \
	--enable-large-config \
	--enable-cplusplus \
	--with-libatomic-ops=check \
	--enable-sigrt-signals

make

cd .libs/
for dname in *.dylib; do
   echo "Post processing ${dname} ..."
   install_name_tool -id "@rpath/${dname}" "$dname"
   otool -D "$dname"
done

popd

################################################################################################
date                                   > $TCC_FOLDER/lib/libgc_build_on_date.txt
echo $LIBGC_COMMIT_FULL_HASH           > $TCC_FOLDER/lib/libgc_build_source_hash.txt
echo $LIBATOMIC_OPS_COMMIT_FULL_HASH   > $TCC_FOLDER/lib/libgc_build_libatomic_ops_source_hash.txt
uname -a                               > $TCC_FOLDER/lib/libgc_build_machine_uname.txt
echo $RECIPE_VERSION                   > $TCC_FOLDER/lib/libgc_build_recipe_version.txt
{
  echo "$0"
  echo "$CONFIGURE_CMD"
}                                      > $TCC_FOLDER/lib/libgc_build_cmd.txt

rsync -a bdwgc/.libs/                   $TCC_FOLDER/lib/
ls -lad $TCC_FOLDER/lib/*

## Verify libgc.dylib actually landed as a real symlink to a versioned
## file, not a plain/dereferenced copy - confirmed via the GitHub API
## that the existing macOS-arm64 bundle's committed libgc.dylib is a
## plain file, not a symlink, i.e. this exact failure mode has already
## happened once, silently. Don't just `test -L`; resolve and print the
## actual target, and independently re-check the install name
## `install_name_tool -id` set above rather than trusting it ran.
if [ ! -L "$TCC_FOLDER/lib/libgc.dylib" ]; then
  echo "::error::$TCC_FOLDER/lib/libgc.dylib is not a symlink after staging - expected a symlink to a versioned file (e.g. libgc.1.dylib), got a plain file or nothing. This would silently ship a dereferenced copy instead of the real libtool-managed layout."
  exit 1
fi
libgc_dylib_target=$(readlink "$TCC_FOLDER/lib/libgc.dylib")
if [ ! -f "$TCC_FOLDER/lib/$libgc_dylib_target" ]; then
  echo "::error::$TCC_FOLDER/lib/libgc.dylib points at '$libgc_dylib_target', but that file does not exist alongside it - the symlink target was not staged."
  exit 1
fi
echo "libgc.dylib -> $libgc_dylib_target (confirmed present)"
libgc_install_name=$(otool -D "$TCC_FOLDER/lib/$libgc_dylib_target" | tail -n 1)
case "$libgc_install_name" in
  @rpath/*)
    echo "install name of $libgc_dylib_target: $libgc_install_name (rpath-relative, as expected)" ;;
  *)
    echo "::error::install name of $libgc_dylib_target is '$libgc_install_name', not an @rpath/-relative path - install_name_tool -id did not take effect as expected."
    exit 1 ;;
esac

echo "Done compiling libgc, at commit $LIBGC_COMMIT, full hash: $LIBGC_COMMIT_FULL_HASH. Static: $TCC_FOLDER/lib/libgc.a, dynamic: $TCC_FOLDER/lib/libgc.dylib -> $libgc_dylib_target"
