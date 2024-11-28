A simple example to show how to call a function written in v from ruby

Step 1: Compile the v code to a shared library using `v -d no_backtrace -shared test.v`

Step 2: Run the ruby file using `ruby test.rb`

Note: you do not need `-d no_backtrace` if you use gcc or clang .
