## Description

`i18n` loads translations from a directory and looks them up by language and key.

## Translation files

Both formats are read from the same directory, and may be mixed.

### `.tr`

The primary format. Each entry is a key line, the text below it, and a `-----` separator:

```
msg_hello
Hello
-----
goods
goods|item|items
```

The file name is the language: `translations/en.tr` defines the `en` translations.

### `.json`

Useful when translations are produced by an external localization tool, or when they
are large enough to be worth splitting per feature.

`translations/en.json` defines the `en` translations, exactly like `en.tr` does:

```json
{
  "msg_hello": "Hello",
  "menu": {
    "file": "File",
    "edit": { "undo": "Undo" }
  }
}
```

Nested objects are flattened with `.`, so the above defines `msg_hello`, `menu.file`
and `menu.edit.undo`.

A file inside a language subdirectory is namespaced by its own name, so translations
can be split per feature. `translations/zh/dashboard.json` defines `zh` keys under
`dashboard.`:

```json
{ "title": "Dashboard", "widgets": { "clock": "Clock" } }
```

gives `dashboard.title` and `dashboard.widgets.clock`.

Non-string scalar values use their JSON representation, so `42`, `true`, and `null`
are loaded as `"42"`, `"true"`, and `"null"`. Arrays are ignored.

When the same key is defined by both formats, the `.tr` value wins.

## Usage

```v ignore
import i18n

println(i18n.tr('en', 'msg_hello'))
println(i18n.tr_plural('en', 'goods', 2))
```

`tr` and `tr_plural` read from the `translations` directory. Use `load_tr_map_from_dir`
with `tr_from_map` / `tr_plural_from_map` to read from another directory.

## Embedding translations in the executable

Reading translations from a directory at run time means shipping that directory next
to the program. To carry them inside the executable instead, embed the files with
`$embed_file` and load them with `load_tr_map_from_embedded`:

```v ignore
import i18n

const translations = i18n.load_tr_map_from_embedded('translations', [
	$embed_file('translations/en.tr'),
	$embed_file('translations/pt-br.tr'),
	$embed_file('translations/zh/dashboard.json'),
])

println(i18n.tr_from_map(translations, 'en', 'msg_hello'))
```

The first argument is the translations directory, spelled the way the embedded paths
spell it. Each file's path inside it gives its language and key prefix, just as it
does when the directory is read from disk.

> [!NOTE]
> Only a `-prod` build (or `-os cross` C output) puts the files' contents into the
> executable. A development build keeps just their paths, to keep rebuilds cheap, and
> reads the files from those paths when the translations are loaded. It works where it
> was built, but panics on a machine without the files, so build anything you ship with
> `-prod`.

Translations that come from somewhere else, such as a database or a download, can be
loaded with `load_tr_map_from_files`, which takes a map from each file's path inside
the translations directory (`en.tr`, `zh/dashboard.json`) to its text. The paths may
use `/` or `\` between their parts.
