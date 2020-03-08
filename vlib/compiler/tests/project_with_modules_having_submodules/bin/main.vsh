#!/usr/local/bin/v run
import mod1.submodule as m

println('This script is located inside: ' + resource_abs_path(''))

println('The result of calling m.f is: ' + m.f().str() )


/* 
NB: this main program v script is under bin/ , 
but it *still* can find mod1, because the parent project has v.mod, 
so v module lookup for this bin/main.vsh file will find mod1 through 
relation to the parent ../v.mod file
*/
