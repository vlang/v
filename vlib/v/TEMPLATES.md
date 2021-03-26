This package is to generate data-driven HTML output.

# Directives
Each directive begins with an `@` sign.
Some directives begin contains a `{}` block, others only have `''` (string) parameters.
More on the directives itself.

Newlines on the beginning and end are ignored in `{}` blocks,
otherwise this (see [if](#if) for this syntax):
```html
@if bool_val {
    <span>This is shown if bool_val is true</span>
}
```
would result in:
```html

    <span>This is shown if bool_val is true</span>

```
which could result in unreadable output.

## if
The if directive consists of three parts, the `@if` tag, the condition (same syntax like in V)
and the `{}` block where you can write html which will be rendered if the condition is true:
```
@if <condition> {}
```

### Example
```html
@if bool_val {
    <span>This is shown if bool_val is true</span>
}
```
One-liner:
```html
@if bool_val { <span>This is shown if bool_val is true</span> }
```

The first example would result in:
```html
    <span>This is shown if bool_val is true</span>
```
while the one-liner results in:
```html
<span>This is shown if bool_val is true</span>
```

## for
The for directive consists of three parts, the `@for` tag, the condition (same syntax like in V)
and the `{}` block where you can write html which will be rendered for each loop:
```
@for <condition> {}
```

### Example
```html
@for i, val in my_vals {
    <span>$i - $val</span>
}
```
One-liner:
```html
@for i, val in my_vals { <span>$i - $val</span> }
```

The first example would result in:
```html
    <span>0 - "First"</span>
    <span>1 - "Second"</span>
    <span>2 - "Third"</span>
    ...
```
while the one-liner results in:
```html
<span>0 - "First"</span>
<span>1 - "Second"</span>
<span>2 - "Third"</span>
...
```

You can also write (and all other for condition syntaxes that are allowed in V):
```html
@for i = 0; i < 5; i++ {
    <span>$i</span>
}
```

## include
The include directive is for including other html files (which will be processed as well)
and consists of two parts, the `@include` tag and a following `'<path>'` string.
The path parameter is relative to the `/templates` directory in the corresponding project.

### Example
Files
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
    it is automatically appended and only allows `html` files.
    
    
## js
The js directive consists of two parts, the `@js` tag and `'<path>'` string,
where you can insert your src
```
@js '<url>'
```

### Example
```html
@js 'myscripts.js'
```

# Variables
All variables which are declared before can be used through the `@{my_var}` syntax.
It's also possible to use properties of structs here like `@{my_struct.prop}`.
