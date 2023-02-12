# Documentation style rules

Thank you for helping us improve the documentation! Below we have described some rules that we
advise you to follow when you expand the documentation with new articles or add new sections to it.

Any comments should be written in the following format:

```
> **Note**
> Text of
> note
```

The same goes for Warning:

```
> **Warning**
> Text of
> your warning
```

Do not use `->`, `=>` instead of `–`:

```
`@FN` – name of the current V function
```

All filenames must be **bold**:

```
**file.v**
```

All symbols (modules, functions, structures, etc.) must be `monospaced`:

```
Module `http.net`
```

A single terminal command may be written inline, but must be `monospaced`:

```
Command `v install`
```

Several terminal commands must be wrapped in \`\`\`:

````
```
cd project
v install markdown
```
````

The language name must be capitalized:

```
V
```

Every sentence must end with a punctuation mark:

```
Please see the Troubleshooting section on our wiki page.
```

Use — or – instead of - when grammar requires it:

```
V needs some packages preinstalled — a working C compiler
```

Compiler names must be spelled correctly:

```
Clang, GCC, MSVC
```

Operating system names must be spelled correctly:

```
macOS, Linux, Windows
```

Keyboard shortcuts must be wrapped in `<kbd>`:

```
Press <kbd>Windows Key</kbd>
```

If you want to show the output of your block of code, then:

1. If the output is short, then write it in a block of code using comments for each line of the
   output:

    ```v
    println("Hello, World!") // Hello, World!
    ```

     ```v
    fn main() {
    	for i in 0..3 {
    		println(i)
    	}
    }
      
    // 1
    // 2
    // 3
     ```

2. If the output is large (more than 10 lines), then it can be put into a separate code block with
   the `Output:` line before it:

   Output:

    ```
    // 1
    // 2
    // 3
    // ...
    ```

If you want to show that there will be a compilation error in the code, then mark it in the code
using `^` to show where exactly the error is:

```v failcompile
fn main() {
	if true {
		println(foo)
		//      ^^^ error: unknown identifier `foo`
	}
}
```
