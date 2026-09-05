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

When the same key is defined by both formats, the `.tr` value wins.

## Usage

```v ignore
import i18n

println(i18n.tr('en', 'msg_hello'))
println(i18n.tr_plural('en', 'goods', 2))
```

`tr` and `tr_plural` read from the `translations` directory. Use `load_tr_map_from_dir`
with `tr_from_map` / `tr_plural_from_map` to read from another directory.
