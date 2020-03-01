
This projects demonstrates how v.mod lookup can be used so that 
a project/module can be as selfcontained as possible.

The programs under bin/ can find the modules mod1, 
because the project has a 'v.mod' file, so v module lookup for 
the programs under bin/ can still find the parent sibling folder
mod1/ through relation to the parent 'v.mod' file.

Note also that mod1/ also has its own 'v.mod' file.
This allows mod1 submodules to find and import themselves 
in relation to it too.

Finally, there is a test/ folder, so you can put all your tests
in there, without cluttering your top level folder, or your module
folders if you so desire.
