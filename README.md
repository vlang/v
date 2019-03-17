# The V Programming Language

V is going to be open-sourced in June 2019. Early access on May 1.

https://vlang.io

Documentation: https://vlang.io/docs

Twitter: https://twitter.com/vlang_io


&nbsp;

## Fast compilation
V compiles 1.5 million lines of code per second per CPU core
```
cd doom3/
wc -l doom3.v     # 458 713
time v doom3.v    # 0.5s
```
[Compilation speed benchmark and comparison with other languages.](https://vlang.io/compilation_speed)

## Safety
- No global state
- No null
- No undefined values
- Option types
- Generics
- Immutability by default
- Partially pure functions

## C/C++ translation
V can translate your entire C/C++ project and offer you the safety, simplicity, and up to 200x compilation speed up. 
```
std::vector<std::string> s;
s.push_back("V is ");
s.push_back("awesome");
std::cout << s.size();
```
```
s := []string 
s << 'V is '
s << 'awesome'
println(s.len)
```
Read about translating Doom & Doom 3, LevelDB, SQLite (coming in March).	

## 400 KB compiler with zero dependencies
The entire V language and its standard library is less than 400 KB. You can build V in 0.3 seconds.


## Performance
- As fast as C
- Minimal amount of allocations 
- Built-in serialization without reflection 

## Hot code reloading
Get your changes instantly without recompiling!

Since you also don't have to waste time to get to the state you are working on after every compilation, this can save a lot of precious minutes of your development time.

[Demonstration of hot code reloading.](https://volt-app.com/img/lang.webm)

## Simple language for building maintainable programs
You can learn the entire language by going through the documentation in half an hour.

Despite being simple, it gives a lot of power to the developer. Anything you can do in other languages, you can do in V.

## REPL
```
 v
 >> data := http.get('https://vlang.io/utc_now')? 
 >> data 
 '1551205308' 
```

## Native cross platform UI library
Build native apps that look native. You no longer need to embed a browser to develop cross platform apps quickly.	

## Run everywhere
V can compile to (human readable) C, so you get the great platform support and optimization of gcc and Clang.
