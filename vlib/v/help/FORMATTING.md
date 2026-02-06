# Formatting a Help File

## Requirements

- 2 space initial indentation (titles exempt)
- Description indentation of two spaces from the longest word
- All descriptions should be indented to the same column within a block, with a
  hard limit at column 80.
    - Multi-line descriptions should indent to match the description part of the previous line

## Example

Adding a help file was made to be easy. All you have to do is create the text file in the
appropriate category, and V should recognize it.

```text
Short description of what it does

Usage:
  v COMMAND (args and such here)

Longer description of what your command aims to do (optional)

Examples: (optional)
  v COMMAND -arg1 -arg2
  
Options:
  -h, --help      Help menu
  -s, --short     Description of short
  -l, --long      Description of long
```
