# V skills command

`v skills list` reads the bundled catalog from the invoking compiler's source
tree, including when the command itself runs from the tool cache.

`v skills remove NAME --dry-run` reports a preview and preserves the installed
directory and every file. The same preview option works with `--global`.
Removal accepts one lowercase skill name made of letters, digits and hyphens.
Path traversal and symlink skill directories are refused; removal targets must
remain immediate children of the selected skills directory.

Installation also refuses an existing symlink destination, including with
`--force` and `--dry-run`, and preserves the linked directory and its contents.

`v skills update` refreshes the skills whose installed copy is still what was
installed while their bundle has moved on. Installation records the digest of
what it wrote in `origin.json` beside the skill directories, which is how
`update` tells that case apart from a skill whose files were edited in place. An
edited skill, and a skill installed before that record existed, are reported and
left alone; `--force` overwrites them and `--dry-run` previews without writing.
The record sits beside the skill directories rather than inside one, so it is
neither listed as a skill nor compared as skill content.

`origin.json` must be a regular file: installation refuses a symlink, or any
other file type, there before replacing skill content, and writes the new record
by rename rather than in place. If `origin.json` or an installed skill file
cannot be read, the skill is treated as unknown, so it is held back rather than
refreshed on an incomplete comparison.
