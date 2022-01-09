## Description:

`dl` can be used to Dynamically Load a library during runtime.
It is a thin wrapper over `LoadLibrary` on Windows, and `dlopen` on Unix.

Using it, you can implement a plugin system for your application.
