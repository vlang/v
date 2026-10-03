---
name: v-tools
description: The parts of the V command surface that are easy to miss - inspecting code (`v ast`, `v where`, `v mod why`), shrinking a compiler error (`v reduce`), project hygiene (`v missdoc`, `v check-md`, `v bump`, `v git-fmt-hook`), embedding binaries (`v bin2v`), shell integration (`v complete`), timing and repeating commands (`v time`, `v repeat`, `v retry`), and compiler upkeep (`v up`, `v clean`, `v env`, `v tracev`). Use when a task is about the toolchain rather than the program, when reaching for `v` by habit and a better command exists, or when asked what V can do. Does not cover writing `.vsh` scripts (see v-scripts), the language rules (see v-lang), the build and test loop (see v-workflow), or reading a project through the MCP server (see v-mcp).
license: MIT
---

# V tools

Most of V's command surface is reached by habit. This is the part that is not:
commands that answer a question, shrink a failure, or keep a project honest.
`v help topics` lists everything; the ones below are the ones worth knowing.

## Look at code before changing it

```sh
v ast file.v            # the AST as JSON, the same as `v ast -p`
v where NAME            # where a symbol is declared, and where it is used
v mod why MODULE        # which import chain pulls a module into the build
```

`v mod why` is the one that answers a question people otherwise guess at: nothing
in `v.mod` records what a build actually reaches, so a dependency can outlive the
code that needed it.

Through the MCP server the same ground is covered with less shell work; see
v-mcp.

## Shrink a compiler error

```sh
v reduce file.v         # minimise to the smallest source that still errors
```

The reduction lands in `rpdc.v` in the current directory. Pair it with
`v bug`, which opens a prefilled issue, and the report carries a minimal
reproducer instead of the file you happened to be editing.

## Project hygiene

```sh
v missdoc src/          # functions with no documentation comment
v check-md README.md    # are the ```v blocks in this markdown correct?
v bump                  # bump the semantic version in v.mod
v git-fmt-hook install  # format on commit
```

`v check-md` is worth knowing about specifically: it compiles the V examples in a
markdown file, which catches a snippet that stopped compiling long after anyone
looked at the prose. `v missdoc` takes `-t` to include function tags.

## Embed binary files

```sh
v bin2v assets/logo.png assets/font.ttf
```

Turns arbitrary files into a V module, so a program carries its images and fonts
instead of reading them from disk at runtime.

## Shell integration

```sh
v complete bash         # autocompletion; bash, fish, zsh and powershell
```

## Time, repeat, retry

```sh
v time CMD              # how long CMD took, and what it exited with
v repeat CMD...         # repeat commands and collect statistics
v retry CMD              # rerun until it succeeds, or a timeout passes
```

`v retry` is the one to reach for when a flaky network call or a busy port is the
thing in the way; `v time` when the question is which of two approaches is
actually cheaper.

## Compiler upkeep

```sh
v up                    # update V itself
v clean                 # remove what a default build leaves behind
v env                   # the environment variables that steer the compiler
v tracev                # a tracing build of the compiler
```

`v env` is the first thing to read when the compiler or one of its tools behaves
differently on another machine, because most of that behaviour is an environment
variable. `v build-c` documents the C-backend flags — `-cc`, `-glibc`, `-musl` —
when the C toolchain itself is the thing in question.

## Other commands worth knowing

```sh
v ls                    # install and run the V language server
v repl                  # the V REPL
v watch file.v          # rebuild when the sources it needs change
v crun file.v           # build and run, keeping the executable
v tool NAME             # run a tool module by name
v share                 # send the source to the V Playground
v download -RD URL      # fetch a self-contained .v or .vsh program
v symlink               # put v on PATH
```

Package management is `v search`, `v show`, `v list`, `v outdated`, `v update`,
`v upgrade` and `v remove`, over the VPM registry.

## The command that installs this skill

```sh
v skills list           # what is bundled, and what is installed
v skills add v-tools    # install one
v skills update         # refresh what a new V revision changed
```

## See also

v-scripts for `.vsh`, v-workflow for the build and test loop, v-lang for the
language rules, v-mcp for the same questions answered through the compiler.