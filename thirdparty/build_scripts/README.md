This folder, contains build scripts used by the CI, to generate prebuilt versions
(or latest version by default) of:
A) TCC from https://repo.or.cz/tinycc.git/
B) libgc from https://github.com/ivmai/bdwgc/

In time, if everything works fine, we can deprecate the older approach, keeping 2 *separate*
binary repos, tccbin/ and libgcbin/, with just pointers back to the build scripts here.

Note: the previous approach (before 2024/08), used in https://github.com/vlang/tccbin/,
where that separate repo contained both prebuilt binaries for tcc AND libgc.a,
AND the separate scripts to generate them, in *separate branches*.
In contrast, this folder contains *all scripts in the same location, in the same branch*, just
with different names, thus making editing them all at the same time and in atomic commits,
that can be reviewed and tested separately in PRs on the CI *much easier*.

WARNING: this is still experimental, and should not be used, except to ease testing on the CI.
