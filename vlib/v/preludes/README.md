# V preludes:

The vlib/v/preludes/ contains small v code snippets, that V uses when 
compiling certain v programs. V adds the files below automatically itself.
Each file is used in different situations (see below).

NB: preludes are *NOT* intended to be used by user programs/modules.
The folder vlib/v/preludes/ is *NOT* a v module.

## Details:

### vlib/v/preludes/live_main.v
Used when compiling live programs. This file is used by the main executable
live program, that starts the file change monitoring thread. Each live program
needs module `os` and module `time`, in order for the background file change 
monitoring thread to work properly.

### vlib/v/preludes/live_shared.v
Used when compiling live programs, for the shared library portion of the live 
programs, that is reloaded each time the code is changed.

### vlib/v/preludes/tests_assertions.v  
Used when compiling `_test.v` programs. 
It specifies how failed assertions will look.

### vlib/v/preludes/tests_with_stats.v
Used when compiling `_test.v` programs with -stats option. 
It specifies how the result will appear ('assert' vs 'asserts' and so on).
