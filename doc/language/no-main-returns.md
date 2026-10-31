# Main functions in object builds

With `-d no_main`, the V `main` function is emitted as an ordinary void function.
Bare returns keep that void return type. A normal executable still returns zero
from its generated C entry point.
