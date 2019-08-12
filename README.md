# The V Programming Language 0.1.x

[![Build Status](https://travis-ci.org/vlang/v.svg?branch=master)](https://travis-ci.org/vlang/v)

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

V 1.0 release is planned for December 2019. Right now V is in an alpha stage. 

## Notes

GitHub marks V's code as written in Go. It's actually written in V, GitHub doesn't support the language yet.

The compilation is temporarily slower for this release:

- Debug builds are used (use `./v -prod -o v compiler` to get faster compilation).
- vlib is recompiled with every program you build.
- The new formatter runs on every single token and slows the compiler down by ~20%. This will be taken care of.





## Installing V from source

### Linux, macOS, Windows, *BSD, WSL, Android, Raspbian


```bash
git clone https://github.com/vlang/v
cd v
make
```

That's it! Now you have a V executable at `[path to V repo]/v`. `[path to V repo]` can be anywhere.


### C compiler

You'll need Clang or GCC.

On macOS run `xcode-select --install` if you don't have XCode or XCode tools installed.

On Windows follow these instructions: [github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows](https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows)

### Building without make
```bash
git clone https://github.com/vlang/v
cd v
# Download the V compiler's source translated to C
curl -O https://raw.githubusercontent.com/vlang/vc/master/v.c
cc -std=gnu11 -w -o v v.c -lm  # Build it with Clang or GCC
./v -o v compiler              # Use the resulting V binary to build V from V source
```
(These instructions won't work on Windows, use `make.bat` instead).

### Symlinking and updates

You can create a symlink so that it's globally available:

```
sudo ln -s [path to V repo]/v /usr/local/bin/v
```

V is being constantly updated. To update V, simply run

```
v up
```

## Docker

```bash
git clone https://github.com/vlang/v
cd v
docker build -t vlang .
docker run --rm -it vlang:latest
v
```



### Testing

```
$ cd examples
$ v run hello_world.v
hello world

$ v
V 0.1.x
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
v run tetris/tetris.v
```

<img src='https://raw.githubusercontent.com/vlang/v/master/examples/tetris/screenshot.png' width=300>


In order to build Tetris and anything else using the graphics module, you will need to install glfw and freetype.

```
v install glfw
```

If you plan to use the http package, you also need to install openssl on non-Windows systems.

```
macOS:
brew install glfw freetype openssl

Debian/Ubuntu:
sudo apt install libglfw3 libglfw3-dev libfreetype6-dev libssl-dev

Arch/Manjaro:
sudo pacman -S glfw-x11 freetype2

Fedora:
sudo dnf install glfw glfw-devel freetype-devel
```

glfw dependency will be removed soon.

## Code structure

https://github.com/vlang/v/blob/master/CONTRIBUTING.md

