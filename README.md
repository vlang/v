# The V Programming Language

[![Build Status](https://github.com/vlang/v/workflows/CI/badge.svg)](https://github.com/vlang/v/workflows/CI)
[![Build Status](https://travis-ci.org/vlang/v.svg?branch=master)](https://travis-ci.org/vlang/v)
<a href='https://patreon.com/vlang'><img src='https://img.shields.io/endpoint.svg?url=https%3A%2F%2Fshieldsio-patreon.herokuapp.com%2Fvlang%2Fpledges&style=for-the-badge' height='20'></a>

https://vlang.io

Documentation: https://vlang.io/docs

Changelog: [Master Changelog](https://github.com/vlang/v/blob/master/CHANGELOG.md) | [Releases (with changelogs)](https://github.com/vlang/v/releases)

Twitter: https://twitter.com/v_language

Discord (primary community): https://discord.gg/n7c74HM

Installing V: https://github.com/vlang/v#installing-v-from-source


## Key Features of V

- Simplicity: the language can be learned in less than an hour
- Fast compilation: ≈100k — 1.2 million loc/s
- Easy to develop: V compiles itself in less than a second
- Performance: within 3% of C
- Safety: no null, no globals, no undefined behavior, immutability by default
- C to V translation
- Hot code reloading
- Powerful UI and graphics libraries
- Easy cross compilation
- REPL
- Built-in ORM
- C and JavaScript backends

V 1.0 release is planned for December 2019. Right now V is in an alpha stage.

## Installing V from source

### Linux, macOS, Windows, *BSD, WSL, Android, Raspbian


```bash
git clone https://github.com/vlang/v
cd v
make
```

That's it! Now you have a V executable at `[path to V repo]/v`. `[path to V repo]` can be anywhere.

V is being constantly updated. To update V, simply run

```
v up
```


### C compiler

You'll need Clang or GCC or Visual Studio. If you are doing development, you most likely already have one of those installed.

Otherwise follow these instructions:

[https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Linux-macOS](https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Linux-macOS)

[github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows](https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows)


### Symlinking

You can create a `/usr/local/bin/v` symlink so that V is globally available:

```
sudo ./v symlink
```


### Docker

```bash
git clone https://github.com/vlang/v
cd v
docker build -t vlang .
docker run --rm -it vlang:latest
v
```



### Testing and running the examples

Make sure V can compile itself:

```
v -o v compiler
```

```
$ v
V 0.1.x
Use Ctrl-D to exit

>>> println('hello world')
hello world
>>>
```


```
cd examples
v hello_world.v && ./hello_world    # or simply
v run hello_world.v                 # this builds the program and runs it right away

v word_counter.v && ./word_counter cinderella.txt
v run news_fetcher.v
v run tetris/tetris.v
```

<img src='https://raw.githubusercontent.com/vlang/v/master/examples/tetris/screenshot.png' width=300>

In order to build Tetris and anything else using the graphics module, you will need to install glfw and freetype libraries.

If you plan to use the http package, you also need to install OpenSSL on non-Windows systems.

```
macOS:
brew install glfw freetype openssl

Debian/Ubuntu:
sudo apt install libglfw3 libglfw3-dev libfreetype6-dev libssl-dev

Arch/Manjaro:
sudo pacman -S glfw-x11 freetype2

Fedora:
sudo dnf install glfw glfw-devel freetype-devel

Windows:
git clone --depth=1 https://github.com/ubawurinna/freetype-windows-binaries [path to v repo]/thirdparty/freetype/

```

glfw dependency will be removed soon.

## JavaScript backend

```
fn main() {
        for i := 0; i < 3; i++ {
                println('Hello from V.js')
        }
}
```

```bash
v -o hi.js hi.v && node hi.js
Hello from V.js
Hello from V.js
Hello from V.js
```

## Troubleshooting:

You can see how V invokes the C backend compiler with `v -show_c_cmd file.v` .

You can produce a .c file, *without* compiling it further with `v -o file.c file.v` . 
That is useful, if you want to integrate v as a transpiler into the build system (probably using a Makefile) of an existing large C code base, or if you just want to read the produced C code.

You can prevent v from deleting the intermediate .c file (which is useful if you want to use a debugger like gdb or msvc) by: `v -debug file.v` .

You can pass `-g`, which has the effect of -debug, and in addition will make the debugger information to have V line numbers, instead of C ones (NB: this will make the intermediate .c file harder to read).


You can also set the VFLAGS environment variable to pass one or more flags to v, so that you do not have to type them manually everytime.
Windows (cmd): `set VFLAGS=-debug -show_c_cmd`
Windows (PowerShell): `$env:VFLAGS="-debug -show_c_cmd"`
Unix (bash): export VFLAGS="-debug -show_c_cmd"

Windows:
If you get this error while running the V REPL, and you are using msvc:
`'gcc' is not recognized as an internal or external command, operable program or batch file.`

... please try:
```shell
set VFLAGS=-os msvc
v.exe runrepl

```


## Contributing

Code structure:

https://github.com/vlang/v/blob/master/CONTRIBUTING.md

If you introduce a breaking change and rebuild V, you will no longer be able to use V to build itself. So it's a good idea to make a backup copy of a working compiler executable.


