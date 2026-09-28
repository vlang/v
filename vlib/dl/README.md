## Description

`dl` can be used to Dynamically Load a library during runtime.
It is a thin wrapper over `LoadLibrary` on Windows, and `dlopen` on Unix.

Using it, you can implement a plugin system for your application.

A V shared library can return interface values whose concrete types exist only
inside that library. Their methods can be called by a program that loaded the
library with `dl.open` (or `dl.loader`), as long as it has not been closed yet.

> **Note**
> We highly recommend using `dl.loader` instead of `dl` directly.
> It provides a more user-friendly API in the V way.
