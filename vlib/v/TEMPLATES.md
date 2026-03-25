V allows for easily using text templates, expanded at compile time to
V functions, that efficiently produce text output. This is especially
useful for templated HTML views, but the mechanism is general enough
to be used for other kinds of text output also.

# Template directives

Each template directive begins with an `@` sign.
Block directives are line-based: start with `@if`, `@for`, or `@else` on their own line,
then close the block with `@end`, `@endif`, or `@endfor`.
Other directives only have `''` (string) parameters.

For example:

```html
@if bool_val
    <span>This is shown if bool_val is true</span>
@end
```

... renders as:

```html

    <span>This is shown if bool_val is true</span>

```

... which is less readable.

## if

The if directive consists of the `@if` tag, the condition
(using the same syntax as in V), and a block of template content.
Close the block with `@end` or `@endif`.

```
@if <condition>
    ...
@end
```

### Example

```html
@if bool_val
    <span>This is shown if bool_val is true</span>
@end
```

You can also use `@else`:

```html
@if bool_val
    <span>This is shown if bool_val is true</span>
@else
    <span>This is shown if bool_val is false</span>
@endif
```

The first example would result in:

```html
    <span>This is shown if bool_val is true</span>
```

## for

The for directive consists of three parts, the `@for` tag,
the condition (same syntax like in V) and the template block,
where you can write text, rendered for each iteration of the loop:

```
@for <condition>
    ...
@end
```

### Example for @for

```html
@for i, val in my_vals
    <span>${i} - ${val}</span>
@end
```

The first example would result in:

```html
    <span>0 - "First"</span>
    <span>1 - "Second"</span>
    <span>2 - "Third"</span>
    ...
```

You can also write (and all other for condition syntaxes that are allowed in V):

```html
@for i = 0; i < 5; i++
    <span>${i}</span>
@end
```

## include

The include directive is for including other html files (which will be processed as well)
and consists of two parts, the `@include` tag and a following `'<path>'` string.
The path parameter is relative to the template file being called.

### Example for the folder structure of a project using templates:

```
Project root
/templates
    - index.html
    /headers
        - base.html
```

`index.html`

```html

<div>@include 'header/base'</div>
```

> Note that there shouldn't be a file suffix,
> it is automatically appended and only allows `html` files.


## js

The js directive consists of two parts, the `@js` tag and `'<path>'` string,
where you can insert your src

```
@js '<url>'
```

### Example for the @js directive:

```html
@js 'myscripts.js'
```

# Variables

All variables, which are declared before the $tmpl can be used through the `@{my_var}` syntax.
It's also possible to use properties of structs here like `@{my_struct.prop}`.

# Escaping

The `@` symbol starts a template directive. If you need to use `@` as a regular 
character within a template, escape it by using a double `@` like this: `@@`.
