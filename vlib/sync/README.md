## Description

`sync` provides cross platform handling of concurrency primitives.

Channels can carry function values, including aliases such as `type Task = fn ()`.
Function channels support explicit capacities and default initialization in structs
and globals. Arrays of channels can use an explicit `init` expression to initialize
each channel. A received function value can be called normally.
