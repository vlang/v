# V REPL Tests Script

### How to write a new test
  - Create a new file named `*.repl`
  - Write the input to be given to REPL
  - Add `===output===`
  - Write the output expected
  
### Notes
Keep in mind, that the way V repl works for now, every non empty line
would cause a new recompilation of the entire repl content that was
collected so far. 

*Longer REPL files would cause measurably*
*longer recompilation/testing times.*

Also, longer repl files would be slower to debug when they fail,
*It is better to have several smaller files vs one huge REPL file.*

### Example :
```
a := 1
println(a)
===output===
1
