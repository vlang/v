# Imported constants in fixed-array lengths

A fixed-array length referring to an imported constant respects its import alias.
For `import sizes as fx`, `[fx.max_items]int` uses `sizes.max_items`, even when another module
is named `fx` and imported under a different alias.
When the same alias has different meanings in different source files, constant resolution
does not guess a module.
