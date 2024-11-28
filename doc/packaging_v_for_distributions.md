Packaging V
=============

Thank you for supporting V and its users, and for creating, *and maintaining* a package for it.

V is still changing, and installing it from source, is still the *recommended way*, to
distribute it to most V users.

However, we understand that sooner or later people will want to adapt the
upstream source to their favorite distro/platform, and that usually involves preparing a package
that will be installed and updated (using a standardized package manager) just like all other
programs in the distribution.

This document is for the brave people that want to prepare and
*maintain* such a package.

Good luck.

Details
=============

Preparing a package of the current version of V for distribution, has some peculiarities
that must be understood, and taken into account, for the best experience of the users of
your package, for your sanity, and for easier troubleshooting of problems when they happen.

1) V builds many of its commands on demand, to save both time and space. The source of the
entry points of those commands is stored in the folder cmd/tools/ . If you are preparing a
prebuilt V package, that needs to ensure that the final package after installation will be
read only, that will be a problem, since V will try to write to its cmd/tools/ folder.
To prevent that, you need to build *all* the commands/executables in that folder *ahead of time*.
This can be done by the command `v build-tools`.

2) Tell V to not try to recompile its tools anymore. To do that, V supports checking for the
presence of a file named `cmd/tools/.disable_autorecompilation` (that file is missing by default
in the cloned version of V).
Note: the content of this file does not matter, *only its presence does*.

3) V has a tool/command, that enables rapid updating from the latest master branch in its main
repository. That tool is `v up`. Most distros however, have the policy of not allowing packages
to make modifications to themselves. Instead, they require the user to use their package manager,
provided by the distro/platform.

To facilitate that, it is usually best for all involved if you, as a package maintainer, remove
or replace cmd/tools/vup.v, with a short v program, that advises the final V user to use the
package manager tool, or to use a V installed from source instead.

4) Another V feature/command that can interfere with read only packaging is `v self`. We
recommend that you disable this command as well, by replacing cmd/tools/vself.v with a small
V program that informs the user that `v self` is not recommended when used with a packaged
version of V, and then advises the user to install V from source if he wants to use `v self`.

5) The V source repo contains sizable folders like `.git/` and `thirdparty/tcc/.git/` which will
not be needed by users of packages (they are not useful if `v up` is disabled,
as recommended previously). Some other files, such as `Makefile`, `make.bat`, and `GNUmakefile`
will not be used either, so they can also be safely removed.

Depending on how stripped you want your package, you can remove the examples/ folder as well.
It is usually good to have examples, but they could be in a separate package.

If a user attempts to compile the examples in that folder, it
will not work in a read-only package (because the executables are put right next to the 
source `.v` files by default, unless `-o path_to_executable_name` is used).

6) Location of vlib/ . V expects, that its vlib/ folder, will be located right next to its
executable, and that is currently hard to customize. It will be changed, if there is enough
interest from package maintainers, expressed on the V issue tracker:
https://github.com/vlang/v/issues

*However, you can* leave the executable in say /opt/vlang/v, with vlib/ in /opt/vlang/vlib/ ,
then put a symlink from `/opt/vlang/v` to whatever folder that is on the user's PATH on your
distro, i.e.: `sudo ln -s /opt/vlang/v /usr/bin/v` or `sudo ln -s /opt/vlang/v /bin/v` .

7) Location of the V executable can be controlled by setting the env variable VEXE.
It defaults to the absolute path of the V executable.

8) Location of the modules folder for `v install` can be controlled by setting the env variable
named `VMODULES`. By default, modules are installed to `~/.vmodules`,
(`C:\Users\<user id>\.vmodules` on Windows).

9) Location of the folder used by V for its temporary files can be controlled by setting the env
variable `VTMP`.  By default, the system TEMP folder is used.

10) Setting additional V flags for each compilation can be done by setting the env variable
`VFLAGS`.

Note: points 7, 8, 9, 10 above, allow you to create a small launcher shell script, named `v`,
that sets those variables dynamically, according to the current user. You can then put
*this shell script* inside your package bin folder, i.e. inside /usr/bin/v, or /bin/v etc,
*instead of* the main V executable, or a symlink to it.


Example script to prepare the V source folder for packaging:
-----------------------------------------------------------

Combining all of the above, here is a small script, that can be used as a base for your packaging
efforts:

```sh
echo "println('use your package manager to update V," > cmd/tools/vup.v
echo "or if you want more recent V versions, just clone V from source," >> cmd/tools/vup.v
echo "see https://github.com/vlang/v#installing-v-from-source')" >> cmd/tools/vup.v

echo "println('v self is disabled for packaged versions of V." > cmd/tools/vself.v
echo "Use your package manager, to update your V package instead." >> cmd/tools/vself.v
echo "Alternatively, if you do want a more recent V version, just clone V from source," >> cmd/tools/vself.v
echo "then follow the instructions here: https://github.com/vlang/v#installing-v-from-source')" >> cmd/tools/vself.v

v -prod -o v cmd/v                            ## build V itself with -prod
./v -prod build-tools                         ## build all tools with -prod too
touch ./cmd/tools/.disable_autorecompilation  ## tell V to not try to recompile any tool anymore

### Cleanup folders that would not be needed inside a package,
### that is distributed separately from the V source repository:
rm -rf .git/
rm -rf thirdparty/tcc/.git/
```

Example `v` script for your bin folder:
--------------------------------------------------

```sh
#!/usr/bin/env bash

export VEXE="/opt/vlang/v"
export VFLAGS="-cc /usr/bin/custom_cc"
export VCACHE="/var/cache/custom_vcache_folder" ## ~/.vmodules/cache by default
export VTMP="/var/cache/custom_tmp"
export VMODULES="$HOME/.vmodules"

/opt/vlang/v $@
```

Note: do not forget to also do this in your post install script: `chmod +x /usr/bin/v`
