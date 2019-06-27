# The V Programming Language 0.1.x

[![Build Status](https://dev.azure.com/alexander0785/vlang/_apis/build/status/vlang-CI?branchName=master)](https://dev.azure.com/alexander0785/vlang/_build/latest?definitionId=1&branchName=master) [![Build Status](https://travis-ci.org/vlang/v.svg?branch=master)](https://travis-ci.org/vlang/v)

https://vlang.io

Documentation: https://vlang.io/docs

Twitter: https://twitter.com/v_language

Discord (primary community): https://discord.gg/n7c74HM

Installing V: https://github.com/vlang/v#installing-v-from-source


## Key Features of V

- Simplicity: the language can be learned in half an hour, less if you already know Go
- Fast compilation: ~100k loc/s right now, ~1.2 million loc/s once x64 generation is mature enough
- Easy to develop: V compiles itself in less than a second
- Performance: within 5% of C
- Safety: no null, no globals, no undefined behavior, immutability by default
- C to V translation
- Hot code reloading
- Powerful UI and graphics libraries
- Easy cross compilation
- REPL

V 1.0 release is planned for December 2019.

## Notes

GitHub marks V's code as written in Go. It's actually written in V, GitHub doesn't support the language yet.

The compilation is temporarily slower for this release:

- Debug builds are used (use `./v -prod -o v .` to get faster compilation).
- vlib is recompiled with every program you build.
- The new formatter runs on every single token and slows the compiler down by ~20%. This will be taken care of.
- There are a lot of known issues that are quick to fix (like function lookups being O(n)).

There's some old hacky code written when V was 2 months old. All of it will be quickly cleaned up. There are ~500 lines of C code, which will be removed by the end of June.

## Code structure

https://github.com/vlang/v/blob/master/CodeStructure.md

## Installing V from source

### Linux and macOS

You'll need Clang or GCC. On macOS run `xcode-select --install` if you don't have XCode or XCode tools installed.

```bash
# You can clone V anywhere
git clone https://github.com/vlang/v
cd v/compiler
make
```
Or build without make:
```bash
# Download the V compiler's source translated to C
wget https://raw.githubusercontent.com/vlang/vc/master/v.c  
cc -std=gnu11 -w -o v v.c  # Build it with Clang or GCC
./v -o v .                 # Use the resulting V binary to build V from V source
./v -o v .                 # Build the compiler again to make sure it works
```

That's it! Now you have a V executable at `v/compiler/v`.

You can create a symlink so that it's globally available:

```
sudo ln -s [path to V repo]/compiler/v /usr/local/bin/v
```

V is being constantly updated. To update V, simply run

```
git pull origin master
cd compiler/
make clean
make
```


### Windows

V works great on Windows Subsystem for Linux. The instructions are the same as above.

If you want to build v.exe on Windows without WSL, you will need Visual Studio. Microsoft doesn't make it easy for developers.  Mingw-w64 could suffice, but if you plan to develop UI and graphical apps, VS is your only option.

V temporarily can't be compiled with Visual Studio. This will be fixed asap.

### Testing

```
$ cd examples
$ v run hello_world.v
hello world

$ v
V 0.1.2
Use Ctrl-D to exit

>>> println('hello world')
hello world
>>>
```

Now if you want, you can start tinkering with the compiler. If you introduce a breaking change and rebuild V, you will no longer be able to use V to build itself. So it's a good idea to make a backup copy of a working compiler executable.


### Running the examples

```
v hello_world.v && ./hello_world    # or simply
v run hello_world.v                 # this builds the program and runs it right away

v word_counter.v && ./word_counter cinderella.txt
v run news_fetcher.v
v run tetris.v
```

<img src='https://raw.githubusercontent.com/vlang/v/master/examples/tetris/screenshot.png' width=300>


In order to build Tetris and anything else using the graphics module, you will need to install glfw and freetype.

If you plan to use the http package, you also need to install libcurl.

```
macOS:
brew install glfw freetype curl

Ubuntu:
sudo apt install libglfw3 libglfw3-dev libfreetype6-dev libcurl3-dev

Arch:
sudo pacman -S glfw-x11 curl freetype2
```

glfw and libcurl dependencies will be removed soon.
