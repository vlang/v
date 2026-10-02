# V skills command

`v skills list` reads the bundled catalog from the invoking compiler's source
tree, including when the command itself runs from the tool cache.

`v skills remove NAME --dry-run` reports a preview and preserves the installed
directory and every file. The same preview option works with `--global`.
Removal accepts one lowercase skill name made of letters, digits and hyphens.
Path traversal and symlink skill directories are refused; removal targets must
remain immediate children of the selected skills directory.
