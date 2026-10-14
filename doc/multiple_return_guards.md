# Multiple values from guarded expressions

An `if` expression can supply multiple values. A guard can bind a map entry,
array element, Option, or Result before producing those values:

```v
names := {
	'key': 'value'
}
a, b := if name := names['key'] { name, name } else { '', '' }
assert a == 'value' && b == 'value'
```

Each selected branch can end with either comma-separated values or a call returning
multiple values. The same expression can initialize variables or assign existing ones.
The guard checks presence before reading its payload, and evaluates its source once.
An `else if` branch can introduce another guard; each binding belongs to its own branch.
