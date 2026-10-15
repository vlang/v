# Live code reloading

Compile a program with `v -live run main.v` and mark reloadable functions with `@[live]`.
The C backend starts the reloader before `main`, watches source files containing live functions,
and reloads their implementations when those files change. This also works in imported modules.
Live functions can call other live functions and retain their arguments and return values.

The executable keeps its existing state during reloads. Calls and library replacement share a
recursive mutex, so a nested live call does not deadlock and a library is not unloaded mid-call.
Source change detection uses whole-second modification times.
