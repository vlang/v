## NOTE: this script does *not* use a shebang **deliberately**, in order to
## access the same shell, to capture its own launching command with `fc`,
## and to record it later in the new commit message in thirdpart/tcc.

## WARNING: THE ORIGINAL OF THIS SCRIPT IS IN:
## https://github.com/vlang/v/blob/master/thirdparty/build_scripts/thirdparty-openbsd-amd64_tcc.sh ,
## I.E. IN THE MAIN V REPOSITORY. IF YOU NEED TO MAKE CHANGES, CHANGE THAT.
##
## THE `build.sh` FILE IN `vlang/tccbin` REPO IS A COPY, RECORDED AT THE TIME
## OF REBUILDING, FOR EASIER/RELIABLE REPRODUCTION OF HISTORIC VERSIONS.
## IT IS NOT INTENDED TO BE MODIFIED.

BUILD_CMD=`fc -nl -0`
## remove whitespaces before/after the actual command:
BUILD_CMD="$(echo "${BUILD_CMD}" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"

set -e

if ! test -f vlib/v/compiler_errors_test.v; then
  echo "this script should be run in V's main repo folder!"
  exit 1
fi

export CFLAGS='-O3'
export CURRENT_SCRIPT_PATH=$(realpath "$0")

export TCC_COMMIT="${TCC_COMMIT:-mob}"
export TCC_FOLDER="${TCC_FOLDER:-thirdparty/tcc.$TCC_COMMIT}"
export CC="${CC:-clang}"

echo " BUILD_CMD: \`$BUILD_CMD\`"
echo "        CC: $CC"
echo "TCC_COMMIT: $TCC_COMMIT"
echo "TCC_FOLDER: \`$TCC_FOLDER\`"
echo ===============================================================

rm -rf tinycc/
rm -rf thirdparty/tcc.original/
rsync -a thirdparty/tcc/ thirdparty/tcc.original/

pushd .

git clone git://repo.or.cz/tinycc.git

cd tinycc

git checkout $TCC_COMMIT
export TCC_COMMIT_FULL_HASH=$(git rev-parse HEAD)

./configure \
            --prefix=$TCC_FOLDER \
            --bindir=$TCC_FOLDER \
            --crtprefix=$TCC_FOLDER/lib:/usr/lib \
            --sysincludepaths=$TCC_FOLDER/lib/tcc/include:/usr/local/include:/usr/include \
            --libpaths=$TCC_FOLDER/lib/tcc:$TCC_FOLDER/lib:/usr/lib:/usr/local/lib \
            --cc="$CC" \
            --extra-cflags="$CFLAGS" \
            --config-backtrace=yes \
            --config-bcheck=yes \
            --debug

gmake
gmake install

popd

rsync -a --delete tinycc/$TCC_FOLDER/                 $TCC_FOLDER/
rsync -a          thirdparty/tcc.original/.git/       $TCC_FOLDER/.git/
# rsync -a          thirdparty/tcc.original/lib/libgc*  $TCC_FOLDER/lib/
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
git add .
git commit -m "build with \`$BUILD_CMD\`"
popd

echo "tcc commit: $TCC_COMMIT , full hash: $TCC_COMMIT_FULL_HASH ."
echo "The tcc executable is ready in $TCC_FOLDER/tcc.exe"
