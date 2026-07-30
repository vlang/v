## NOTE: this script does *not* use a shebang **deliberately**, in order to
## access the same shell, to capture its own launching command with `fc`,
## and to record it later in the new commit message in thirdpart/tcc.

## WARNING: THE ORIGINAL OF THIS SCRIPT IS IN:
## https://github.com/vlang/v/blob/master/thirdparty/build_scripts/thirdparty-linux-amd64_tcc.sh ,
## I.E. IN THE MAIN V REPOSITORY. IF YOU NEED TO MAKE CHANGES, CHANGE THAT.
##
## THE `build.sh` FILE IN `vlang/tccbin` REPO IS A COPY, RECORDED AT THE TIME
## OF REBUILDING, FOR EASIER/RELIABLE REPRODUCTION OF HISTORIC VERSIONS.
## IT IS NOT INTENDED TO BE MODIFIED.

if test -z "$BUILD_CMD"; then
  BUILD_CMD="$(fc -nl -0 2>/dev/null || true)"
fi
## remove whitespaces before/after the actual command:
BUILD_CMD="$(echo "${BUILD_CMD}" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
if test -z "$BUILD_CMD"; then
  BUILD_CMD="$0"
fi

set -e

if ! test -f vlib/v/compiler_errors_test.v; then
  echo "this script should be run in V's main repo folder!"
  exit 1
fi

export CURRENT_SCRIPT_PATH=$(realpath "$0")

export TCC_COMMIT="${TCC_COMMIT:-mob}"
export TCC_FOLDER="${TCC_FOLDER:-thirdparty/tcc.$TCC_COMMIT}"
export TCC_REPO="${TCC_REPO:-https://repo.or.cz/tinycc.git}"
export CC="${CC:-gcc}"

git_argv_runner="${GIT_ARGV_RUNNER:-cmd/tools/git_argv.sh}"
if ! test -r "$git_argv_runner"; then
  echo "the Git argv helper was not found: $git_argv_runner" >&2
  exit 2
fi
# shellcheck source=../../cmd/tools/git_argv.sh
source "$git_argv_runner"
parse_git_argv
require_git_executable

echo " BUILD_CMD: \`$BUILD_CMD\`"
echo "        CC: $CC"
echo "TCC_COMMIT: $TCC_COMMIT"
echo "TCC_FOLDER: \`$TCC_FOLDER\`"
echo ===============================================================

rm -rf tinycc/
rm -rf thirdparty/tcc.original/
rsync -a thirdparty/tcc/ thirdparty/tcc.original/
## rm -rf $TCC_FOLDER

pushd .

run_git clone "$TCC_REPO" tinycc

cd tinycc

run_git checkout "$TCC_COMMIT"
TCC_COMMIT_FULL_HASH="$(run_git rev-parse HEAD)"
export TCC_COMMIT_FULL_HASH

## Note: crt1.o is located in:
## /usr/lib/x86_64-linux-gnu on Debian/Ubuntu
## /usr/lib64 on Redhat/CentOS
## /usr/lib on ArchLinux

./configure \
            --prefix=$TCC_FOLDER \
            --bindir=$TCC_FOLDER \
            --crtprefix=$TCC_FOLDER/lib:/usr/lib/x86_64-linux-gnu:/usr/lib64:/usr/lib:/lib/x86_64-linux-gnu:/lib \
            --libpaths=$TCC_FOLDER/lib/tcc:$TCC_FOLDER/lib:/usr/lib/x86_64-linux-gnu:/usr/lib64:/usr/lib:/lib/x86_64-linux-gnu:/lib:/usr/local/lib/x86_64-linux-gnu:/usr/local/lib \
            --sysincludepaths="{B}/include:/usr/local/include/x86_64-linux-gnu:/usr/local/include:/usr/include/x86_64-linux-gnu:/usr/include" \
            --cc=$CC \
            --extra-cflags=-O3 \
            --config-bcheck=yes \
            --config-backtrace=yes \
            --debug

make
make install

popd

rsync -a --delete --exclude='/.github/' tinycc/$TCC_FOLDER/                 $TCC_FOLDER/
rsync -a          thirdparty/tcc.original/.git/       $TCC_FOLDER/.git/
rsync -a          thirdparty/tcc.original/lib/libgc*  $TCC_FOLDER/lib/
rsync -a          thirdparty/tcc.original/lib/build*  $TCC_FOLDER/lib/
rsync -a          thirdparty/tcc.original/README.md   $TCC_FOLDER/README.md
rsync -a          $CURRENT_SCRIPT_PATH                $TCC_FOLDER/build.sh
mv                $TCC_FOLDER/tcc                     $TCC_FOLDER/tcc.exe

date                                                > $TCC_FOLDER/build_on_date.txt
echo $TCC_COMMIT_FULL_HASH                          > $TCC_FOLDER/build_source_hash.txt
$TCC_FOLDER/tcc.exe --version                       > $TCC_FOLDER/build_version.txt
uname -a                                            > $TCC_FOLDER/build_machine_uname.txt

## show the builtin search paths for sanity checking:
$TCC_FOLDER/tcc.exe -v -v

pushd .
cd $TCC_FOLDER
run_git add .
run_git commit -m "build with \`$BUILD_CMD\`"
popd

echo "tcc commit: $TCC_COMMIT , full hash: $TCC_COMMIT_FULL_HASH . The tcc executable is ready in $TCC_FOLDER/tcc.exe "
