v.pkgconfig
===========

This module implements the `pkg-config` tool as a library in pure V.

Features:

* Simple API, but still not stable, but shouldnt change much
* Runs 2x faster than original pkg-config
* Commandline tool that aims to be compatible with `pkg-config`
* Resolve full path for `.pc` file given a name
* Recursively parse all the dependencies
* Find and replace all inner variables
* Integration with V, so you can just do `#pkgconfig r_core`

Todo/Future/Wish:

* 100% compatibility with `pkg-config` options
* Strictier pc parsing logic, with better error reporting

Example
-------

The commandline tool is available in `vlib/v/pkgconfig/bin/pkgconfig.v`

```
$ ./bin/pkgconfig -h
pkgconfig 0.3.0
-----------------------------------------------
Usage: pkgconfig [options] [ARGS]

Options:
  -V, --modversion          show version of module
  -d, --description         show pkg module description
  -h, --help                show this help message
  -D, --debug               show debug information
  -l, --list-all            list all pkgmodules
  -e, --exists              return 0 if pkg exists
  -V, --print-variables     display variable names
  -r, --print-requires      display requires of the module
  -a, --atleast-version <string>
                            return 0 if pkg version is at least the given one
      --exact-version <string>
                            return 0 if pkg version is at least the given one
  -v, --version             show version of this tool
  -c, --cflags              output all pre-processor and compiler flags
  -I, --cflags-only-I       show only -I flags from CFLAGS
      --cflags-only-other   show cflags without -I
  -s, --static              show --libs for static linking
  -l, --libs                output all linker flags
      --libs-only-l         show only -l from ldflags
  -L, --libs-only-L         show only -L from ldflags
      --libs-only-other     show flags not containing -l or -L
$
```

Using the API in your own programs:
```v
import v.pkgconfig

opt := pkgconfig.Options{}
mut pc := pkgconfig.load('expat', opt) or { panic(err) }
println(pc.libs)
```
... will produce something like this:
```
['-L/usr/lib/x86_64-linux-gnu', '-lexpat']
```
