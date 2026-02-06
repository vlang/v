This folder contains a V project,
intended to be used as a demonstration
for how to test functions defined inside 
a main module.

See my_test.v and my_other_test.v .
These files work as any other internal module tests,
i.e. they do `module main` at their top, so that v knows,
that they are internal tests, and for which module they apply.

When you do `v my_test.v`, v will try to find other *.v files in 
the same folder that also have `module main` at their top,
then it will process them and process the my_test.v file too.

The v `fn main(){}` function that you most likely also have will get
compiled as normal to `void main__main(){...}`, but it will NOT be 
called by anything, so it will not mess up your tests.

Instead, your test_ functions will get called inside the generated
`int main(){...}` test runner, just like it is the case with all _test.v
files (internal or external ones).

> **Note**
> Each _test.v file is compiled separately from all other _test.v
> files, so you can have conflicting test_ functions in them without a
> problem too.
