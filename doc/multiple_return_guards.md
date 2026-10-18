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
Each branch also keeps the incoming smartcasts independently. Reassigning a narrowed value
in one branch does not remove a valid narrowing from its sibling, and invalidations from
either branch apply after the expression.

A Result guard exposes its implicit `err` only inside a plain `else` block. An `else if`
uses the surrounding bindings. When assigning existing variables, branch values are
evaluated before the destinations, so an outer destination named `err` is updated after
leaving the implicit error scope.

Assignment destinations remain outside every guard binding scope. A guard-local name
can supply a branch value without intercepting a write to an existing destination
with the same name.
