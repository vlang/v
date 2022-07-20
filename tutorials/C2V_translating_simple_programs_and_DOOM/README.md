In this tutorial I'll demonstrate C2V, a fully automatic C to V source code translator,
that has just been released.

We'll run some simple examples and then translate the entire original DOOM,
the legendary shooter, the first ever 3D game released in 1993 and open sourced in 1997.

A video version of this tutorial is available on YouTube:

https://www.youtube.com/watch?v=6oXrz3oRoEg

Let's start.

### A simple C program

First, We have a simple C program that prints prime numbers. It's a small program, 
but it has some of the most widely used language features:
function definitions, control flow via for loops and if conditions, a libc function call,
and various expressions.


```c
#include <stdio.h>
#include <stdbool.h>

bool is_prime(int x) {
    for (int i = 2; i <= x / 2; i++) {
        if (x % i == 0) return false;
    }
    return true;
}

int main() {
    for (int i = 2; i < 100; i++) {
        if (is_prime(i)) {
            printf("%i\n", i);
        }
    }
    return 0;
}
```

We can translate the C file to V by simply running

```
v translate primes.c
```

It will create `primes.v` with the following contents:


```v
[translated]
module main

fn is_prime(x int) bool {
	for i := 2; i <= x / 2; i++ {
		if x % i == 0 {
			return false
		}
	}
	return true
}

fn main() {
	for i := 2; i < 100; i++ {
		if is_prime(i) {
			C.printf(c'%i\n', i)
		}
	}
	return
}
```

This is a valid V program, and running it will give us the same result.

This code is pretty similar to the original, you can notice one major difference.
C allows one statement code blocks without brackets, and V doesn't, so in the
translated code all blocks are explicitely marked with brackets.

C2V successfully converts C's bool type to V's bool.

### Wrapper generation

C2V also has a wrapper generation mode.

For many projects it would make more sense not to translate the entire C code base,
but just to generate V wrappers on top of C APIs. 

For example, let's say you have some complicated crypto or networking library in C
with lots of functions and types, and you don't want to manually type their definitions in V.

For this C2V has a wrapper mode.

We have a simple file `usersapi.h`. By running `v translate -wrapper usersapi.c` 
we get `usersapi.v` with just the function definitions. Function bodies are skipped.

```c
#include <stdio.h>

void usersapi_create_user(char* name, char* password, int age) {
    // ...
    puts("User created!");
}

int usersapi_get_number_of_users() {
    return 1;
}

```

```
module usersapi

fn C.usersapi_create_user(name &i8, password &i8, age int)

pub fn f create_user(name &i8, password &i8, age int)  {
    C.usersapi_create_user(name, password, age)
}

fn C.usersapi_get_number_of_users() int

pub fn f get_number_of_users() int {
    return C.usersapi_get_number_of_users()
}
```

C doesn't have modules, so in most C libraries all functions start with the same prefix,
the name of the virtual module. C2V helpfully strips those prefixes and generates nice
clean wrapper functions that can be called with `usersapi.create_user()` instead of
`usersapi.usersapi_create_user()`

Perhaps in the future C2V will translate C strings (char pointers) to V strings as well.

### Translating DOOM

All right, this works, and it's nice, but these are very trivial examples.
Let's try something a lot more fun, like the DOOM game.


We think it's very important to have large functioning examples to demonstrate the power
of the tools but also to have something to test via CI to avoid regressions.
During V's release we had lots of various examples, now for C2V we have DOOM.
This is the primary test of C2V.

DOOM is a very complex project that uses some really tricky elements of the C language.
And making sure C2V can always handle DOOM not only serves as a good C2V test but also
as a test of the V compiler itself, since it can run entire DOOM in pure V.

First let's download the DOOM source and build it to make sure everything works.
The original 1997 open source release doesn't work nicely on modern OSs, so the community
has been working on different forks. I'll be using Chocolate Doom, it's one of the most
popular forks, and its goal is to be as close to the original as possible
(they even keep the original bugs).


```bash
git clone https://github.com/vlang/doom
```

The main difference in this fork is using SDL for cross platform rendering, so we'll need
to install SDL2 before we can build it.

A great thing about DOOM is that it has a very simple structure and is easy to build.
It's just a bunch of C files that are compiled into object files and are later linked
into a single executable.

To build the C version:

```bash
cd doom/chocolate-doom
cmake .
make chocolate-doom
```

We'll also need a freely distributed shareware doom1.wad with game resources and
the first free levels.

Now let's run the compiled game 

```bash
./src/chocolate-doom -width 640
```

Great, as expected, it all works, now let's translate it to V and try to build that.


In the previous example we translated a single file, this time we'll translate 
an entire directory.

```
v translate src/doom
```

This creates a new directory `src/doom/doom_v/`.

And we can use any name we want, there's a c2v configuration file.

`ll  src/doom/doom_v`

It has a bunch of files translated from C to V. 

We can open any file, for example g_game.c and verify that it is indeed DOOM code
translated from C to V. It's also been nicely formatted by vfmt.

Now let's build it.


```bash
cd src/doom
sh build_doom_v.sh
```

I'll explain what we have in the shell script a bit later.

But first let's run the translated code!

The script compiles into the `src/doom/doomv` binary. Everything works. We have the music,
we can watch the demo, go to the settings, start the game, walk around and shoot enemies.

There's a slight rendering bug at the bottom, but it can be cleaned up by pressing tab twice
and forcing a re-render.

There's a bug somewhere in C2V, it has just been released and needs some fixes.


Now let's look at those shell scripts and see how we built the V translation of DOOM.

C2V doesn't understand makefiles yet, so we have to do things manually. 

In the future making such scripts won't be necessary.


Since doom uses C libraries like textscreen, pcsound, and opl, and we are only translating
doom itself, we need to build doom.o and then link it with the rest of object files.

`build_doom_v.sh`:

```bash
export DOOM=~/code/doom/chocolate-doom

v -o doom_v/doom.o -w -translated doom_v


cc -o doomv \
  $DOOM/src/doom/doom_v/doom.o \

  $DOOM/src/CMakeFiles/chocolate-doom.dir/*.o \
  $DOOM/textscreen/CMakeFiles/textscreen.dir/*.o \
  $DOOM/pcsound/CMakeFiles/pcsound.dir/*.o \
  $DOOM/opl/CMakeFiles/opl.dir/*.o \
  $(sdl2-config --libs) -lSDL2_mixer -lSDL2_net -lpng $LDFLAGS
```

The first line compiles all V files in `doom_v/` directory into a single doom.o object file.
 `-translated` tells V that the code was translated from C, so the compiler is less strict.
 For example it allows using `i++` as an expression (in V it always must be a statement).
 In the future C2V will be able to translate such C expressions into valid V code,
 and `-translated` will be removed entirely.

After generating doom.o, we can compile the entire project.


The second line links it with the rest of the object files.

Here we have the shared library used by other games in the chocolate doom project:
Hexen, Heretic, and Strife.

Then textscreen, pcsound, and opl.


In the near future we'll also translate all these C libraries DOOM depends on, and this
shell script and interacting with the C compiler won't be needed. The entire project will
be runnable with a simple `v translate src/doom && v .` 



One other thing I wanted to do is to show you that it actually is V code we are running.

Let's go to `p_enemy.v` and modify the `P_Move` function. We can add a simple println with
some V specific code, like a reversed V array.


`println([1, 2, 3, 4, 5].reverse())`

Let's rebuild it and run again. As expected, we're getting our array printed every time an
enemy moves.

### Replacing C with V file by file

We translated the entire project. But sometimes it may be better to do more gradual transition
from C to V. For example, if you want to move your project from C to V, you might prefer doing
it slowly and carefully, file by file. After each file migration you can run tests and see if
your application runs correctly.

You can do that with c2v. All you need to do is just translate a single file, generate an object
file, and replace your current C object file with the new V object file.

We can do this with DOOM too. In fact that's how we made the entire thing work. We were doing it
for every single file, one by one, fixing compilation errors, bugs in C2V, and runtime bugs.

```
c2v -o p_enemy.o p_enemy.v
cp p_enemy.o ...
make clean
make chocolate-doom
```


It is actually amazing how well it all works considering there have been two translation 
processes. From C to V via C2V and then From V back to C again via the V compiler.

In fact we can actually look at the generated C code after all the translation and compare it
to the original. Very clean and barely changed:

![image](https://user-images.githubusercontent.com/687996/175178560-58e04248-bcb3-4e33-aebd-ad66bb94a887.png)

Both C2V and V generate clean human readable code.


The comments and C defines are missing right now, that's obviously something very nice to have,
so they will be translated as well.


### Conclusion


C2V has just been released on June 22th 2022, so it's going to mature with the help of 
the community. With bug fixes and new C features supported, it should support the 
entire C standard soon.
Luckily C is a relatively small language. Any kind of software written in C will be
translatable to V fully automatically.

I will also be publishing the same demo with Sqlite and Quake translation.

It's a huge milestone for V and gives V developers access to huge amounts of software
written in C.

We're very excited about this release. 
