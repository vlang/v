# Editing through the server

> The tools are listed in `TOOLS.md`. This reference covers the three writing
> tools, what each guard is for, and when to reach for which.

Three tools write: `v_edit_replace`, `v_rename_symbol` and `v_format`. The last
two default to reporting a plan rather than writing; `v_edit_replace` writes at
once, guarded by `expected_old`. The reason is the same — a file can change between
the moment you read it and the moment you write it, and a silent overwrite is how
that becomes data loss.

## v_references before v_rename_symbol

A rename is the riskiest of the three, because it can touch files you never
opened. Find out what it will cost first:

```json
{"name": "v_references", "arguments": {"path": "src/config.v", "name": "load"}}
```

Then:

```json
{"name": "v_rename_symbol", "arguments": {"name": "load", "new_name": "load_config"}}
```

The response is the plan: every file, every position, `files_changed` and
`edits`. **Read it
before applying.** A count that includes a file you did not expect is the signal to
stop and investigate.

Apply with `"dry_run": false`, or narrow it with `"paths"` first:

```json
{"name": "v_rename_symbol", "arguments": {
    "name": "load", "new_name": "load_config", "paths": ["src/config.v"], "dry_run": false
}}
```

Why the AST matters: the hits are real mentions of the identifier. A comment or a
string that happens to hold `load` is left alone, which a textual replace would
not manage.

Two rules the tool enforces: the new name must be a valid V identifier, and it
must not be the same as the old one.

## v_edit_replace: read, then write back

For a change that is not a rename, replace an exact range. The `expected_old`
argument is the guard, and it is not optional for a file that exists:

```json
{"name": "v_edit_replace", "arguments": {
    "path": "src/main.v",
    "start_line": 42,
    "end_line": 43,
    "expected_old": "\told line\n\tanother line\n",
    "new_text": "\tnew line\n"
}}
```

Read the range first, paste it back exactly, and the write happens only if the
file still says that. If it does not, the response reports `expected` against
`actual` and writes nothing.

That turns "someone edited this file while you were thinking" from a lost change
into a reported one. A textual edit tool has no such answer.

The three shapes:

| Intent | How |
| --- | --- |
| Replace | `expected_old` is the current text, `new_text` is the replacement |
| Insert | `expected_old` is `""`, `start_line` is where the text goes |
| Delete | `new_text` is omitted or `""` |

An insert does not touch the line it is inserted before. A delete removes the
range. `end_line` defaults to `start_line`.

`start_line` one past the last line appends. Anything further is an error, not a
truncation.

## v_format: dry run by default

```json
{"name": "v_format", "arguments": {"path": "src/main.v"}}
```

Reports `changed`, and the full `before` and `after` when it differs. Nothing is
written until you pass `"write": true`.

Format **every file you touched**. A file the formatter would change is a file that
will be rewritten by the next person, and in a diff it is noise that hides the
change you made.

## The loop

1. Make the change with the narrowest tool that fits.
2. `v_check` — does it compile.
3. `v_test_run` — does it behave.
4. `v_format` on what you touched.

Skipping step 4 is the common one, and it is the one a reviewer will notice.

## If something is refused

- **`expected_old` mismatch** — re-read the range. The file moved under you. Do
  not retry with the value the error told you the file now has; that discards
  whatever changed it.
- **Edit count higher than expected** — look at which files. A rename that reaches
  a generated file or a vendored module is a signal that the name is more
  widely used than you thought.
- **`started: false`** — the compiler could not be launched. Nothing was checked
  and nothing ran. This is about the environment, not the code; see
  `TOOLS.md`.

## Do not mix the server and a shell editor mid-change

The `expected_old` guard compares the range on disk with what you last read of
that range, and nothing else. A shell edit to that range makes the next server
write fail, correctly; a shell edit elsewhere in the file, or any change before an
insert, goes through unnoticed, and so does a change between the check and the
write.

Pick one for a given edit: the server for AST-aware and guarded changes, an editor
for exploratory work. Finish the exploratory pass, then re-read before writing.