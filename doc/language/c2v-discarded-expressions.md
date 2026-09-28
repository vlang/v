# Discarded expressions in translated C

Files marked `@[translated]` allow expression statements whose values are unused, including
arithmetic expressions emitted by C2V.

A function or method marked `@[must_use]` still warns when its return value is ignored.
Use the result or explicitly discard it with `_ = call()` to acknowledge the annotation.
