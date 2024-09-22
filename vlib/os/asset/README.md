# Asset

The `asset` module provides a cross platform way to read assets, without cluttering
the user code with comptime conditionals, like `$if ios {` or `$if android {` etc.

Currently it supports Android assets, and desktop applications, that do not use
archived/zipped files, but could be extended to support archived files too in the future.

It relies on the assumption that each platform has a way to either read files, relative
to the location of the executable, or a platform specific way, to package all asset files
into the application archive/package/executable, that is then distributed to the end users.
