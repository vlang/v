# dtm2 - Dynamic Template Manager 2

`dtm2` is the modern runtime renderer for Dynamic Template Manager.
It keeps the original DTM idea: templates are normal files on disk, so they can
be edited without recompiling the application.

The main change from `x.templating.dtm` is architectural. `dtm2` caches parsed
template trees, not rendered HTML responses. This keeps rendering fast while
removing the old async rendered-cache server from the hot path.

## Quick Start

Create a `templates/` folder in your application and put your templates inside
it. DTM2 supports `.html`, `.htm`, `.xml`, `.txt`, and `.text` by default.

```v
import x.templating.dtm2

fn main() {
	mut manager := dtm2.initialize(
		template_dir: 'templates'
	)
	placeholders := {
		'title': 'DTM2'
		'body':  '<strong>escaped by default</strong>'
	}
	rendered := manager.expand('page.html', placeholders: &placeholders)
	println(rendered)
}
```

Example template:

```html
<!doctype html>
<html>
	<head>
		<title>@title</title>
	</head>
	<body>
		<main>@body</main>
	</body>
</html>
```

The rendered `@body` value is escaped by default.

## Veb Example

```v
import veb
import x.templating.dtm2

pub struct App {
pub mut:
	templates &dtm2.Manager = unsafe { nil }
}

pub struct Context {
	veb.Context
}

fn main() {
	mut app := &App{
		templates: dtm2.initialize(
			template_dir: 'templates'
		)
	}
	veb.run[App, Context](mut app, 18081)
}

@['/']
pub fn (mut app App) index(mut ctx Context) veb.Result {
	placeholders := {
		'title': 'Home'
		'body':  'Hello from DTM2'
	}
	html := app.templates.expand('index.html', placeholders: &placeholders)
	return ctx.html(html)
}
```

## Available Options

### Manager options

`dtm2.initialize()` accepts:

- `template_dir` (**string**): root directory used for relative template paths.
  If empty, `<executable directory>/templates` is used.
- `compress_html` (**bool**): enables a lightweight deterministic HTML
  whitespace compressor. It is enabled by default.
- `reload_modified_templates` (**bool**): when enabled, DTM2 checks the source
  template and included files before reusing a parsed template tree. It is
  enabled by default.
- `extension_config_file` (**string**): optional JSON file containing extension
  mappings. If empty, DTM2 automatically loads `dtm2_extensions.json` from the
  configured `template_dir` when that file exists.

Example:

```v ignore
mut manager := dtm2.initialize(
	template_dir:              'templates'
	compress_html:             true
	reload_modified_templates: true
)
```

For maximum hot-path throughput in applications where templates are immutable
after startup, you can disable reload checks:

```v ignore
mut manager := dtm2.initialize(
	template_dir:              'templates'
	reload_modified_templates: false
)
```

The recommended runtime model is one long-lived manager per application or
rendering context. Reusing the manager is what keeps parsed templates and path
resolution cached.

### Template extensions

DTM2 has two rendering modes:

- `TemplateType.html`: HTML/XML-like output, with default escaping and optional
  HTML compression.
- `TemplateType.text`: raw text output, also escaped by default.

Default mappings:

- HTML mode: `.html`, `.htm`, `.xml`
- Text mode: `.txt`, `.text`

Project-specific extensions should be configured with a JSON file.

Example `templates/dtm2_extensions.json`:

```json
{
	"html": [".view", ".tmpl"],
	"text": [".mail", ".md"]
}
```

If the file is named `dtm2_extensions.json` and is placed directly in the
configured `template_dir`, DTM2 loads it automatically:

```v ignore
mut manager := dtm2.initialize(
	template_dir: 'templates'
)
```

You can also point to an explicit config file:

```v ignore
mut manager := dtm2.initialize(
	template_dir:           'templates'
	extension_config_file: 'config/my_dtm2_extensions.json'
)
```

DTM2 ships a default config example that can be copied into your own
`templates/` directory:

```sh
vlib/x/templating/dtm2/dtm2_extensions.json
```

If the JSON file is absent, invalid, or contains invalid entries, DTM2 keeps the
built-in defaults and prints a warning for entries that cannot be registered.
JSON extension config files are limited to 64 KB and every extension is
validated before being registered.

### Render options

`manager.expand()` accepts:

- `placeholders` (**&map[string]string**): values inserted in the template.
- `missing_placeholder_prefix` (**string**): prefix written when a placeholder
  is missing. The default is `@`, preserving the original placeholder text.

Example:

```v ignore
placeholders := {
	'title': 'Example'
}
html := manager.expand('page.html',
	placeholders:               &placeholders
	missing_placeholder_prefix: '@'
)
```

## The Placeholder System

Template placeholders use the `@name` form.

```html
<h1>@title</h1>
<p>@body</p>
```

In DTM2, placeholder values are strings:

```v ignore
placeholders := {
	'title': 'Hello'
	'count': '42'
}
```

Values are escaped by default in both HTML and text templates.

<u>**Security note for custom non-HTML formats such as SQL:** DTM2 is a template
renderer, not a domain-specific sanitizer. By default, it escapes HTML-special
characters in placeholder values. It does not make SQL queries safe, does not
replace prepared statements, and does not validate business-specific formats.
If you add `.sql` or another sensitive extension through configuration, the
security of that generated content remains the responsibility of the
application.</u>

## Explicit HTML Inclusion

The historical `_#includehtml` suffix is still supported for compatibility.
It allows a placeholder to include a restricted set of HTML tags in `.html`
templates.

```v ignore
placeholders := {
	'body_#includehtml': '<p>allowed</p><script>escaped</script>'
}
html := manager.expand('page.html', placeholders: &placeholders)
```

The template still uses the normal placeholder name:

```html
<main>@body</main>
```

DTM2 escapes the complete value first, then restores only allowed tags. This
preserves the old opt-in behavior without allowing arbitrary raw HTML through.
In `.txt` templates, HTML is always escaped.

Allowed tags:

```html
<div>, </div>, <h1>, </h1>, <h2>, </h2>, <h3>, </h3>, <h4>, </h4>,
<h5>, </h5>, <h6>, </h6>, <p>, </p>, <br>, <hr>, <span>, </span>,
<ul>, </ul>, <ol>, </ol>, <li>, </li>, <dl>, </dl>, <dt>, </dt>,
<dd>, </dd>, <menu>, </menu>, <table>, </table>, <caption>, </caption>,
<th>, </th>, <tr>, </tr>, <td>, </td>, <thead>, </thead>,
<thread>, </thread>, <tbody>, </tbody>, <tfoot>, </tfoot>, <col>, </col>,
<colgroup>, </colgroup>, <header>, </header>, <footer>, </footer>,
<main>, </main>, <section>, </section>, <article>, </article>,
<aside>, </aside>, <details>, </details>, <dialog>, </dialog>,
<data>, </data>, <summary>, </summary>
```

## Includes

Templates can include other templates with a simple line-level directive:

```html
<header>@include "partials/nav"</header>
<main>@body</main>
```

Include paths are resolved relative to the current template. If no file
extension is provided, `.html` is added. The final resolved include path must
stay inside the manager `template_dir`; attempts to include files through `../`
or absolute paths outside that root fail.

The same boundary applies to templates passed to `expand()`. Absolute template
paths are accepted only when they resolve inside `template_dir`.

Included files are tracked as dependencies of the parsed template. When
`reload_modified_templates` is enabled, changing an included file invalidates
the cached parsed tree.

## Backward Compatibility With DTM v1

Existing code that imports `x.templating.dtm` is kept source-compatible for the
migration period. The v1 facade is deprecated, but it now delegates rendering to
DTM2 internally.

That means old code can continue to compile:

```v ignore
import x.templating.dtm

mut manager := dtm.initialize()
mut placeholders := map[string]dtm.DtmMultiTypeMap{}
placeholders['title'] = 'Legacy DTM'
placeholders['count'] = 7
html := manager.expand('page.html', placeholders: &placeholders)
```

For new code, prefer importing `x.templating.dtm2` directly:

```v ignore
import x.templating.dtm2

mut manager := dtm2.initialize(template_dir: 'templates')
placeholders := {
	'title': 'Modern DTM'
	'count': '7'
}
html := manager.expand('page.html', placeholders: &placeholders)
```

Migration notes:

- Replace `import x.templating.dtm` with `import x.templating.dtm2`.
- Replace `DtmMultiTypeMap` placeholder maps with `map[string]string`.
- Convert numeric values to strings before rendering.
- Remove `stop_cache_handler()` calls; DTM2 does not start an async cache
  server.
- Keep using `_#includehtml` only when HTML inclusion is intentional.

## Design Notes

DTM2 intentionally keeps rendering and rendered-output caching separate.

The manager caches:

- canonical template paths;
- parsed template trees;
- dependency metadata for root templates and includes.

The manager does not cache rendered HTML responses. If a future rendered-cache
layer is needed, it should remain a small optional layer above DTM2 rather than
part of the parser/renderer core.

## Benchmarks

The local benchmark harness lives in:

```sh
vlib/x/templating/dtm2/benchmarks/
```

Run it from the repository root:

```sh
vlib/x/templating/dtm2/benchmarks/run_dtm2_benchmark.sh
```

Useful options:

- `DTM2_BENCH_MODE=prod|prod_o2|dev`
- `DTM2_BENCH_CASE=all|small_hot|small_cold|many_hot|many_cold|include_hot`
- `DTM2_BENCH_CASE=include_cold|xml_hot|xml_cold`
- `DTM2_BENCH_ITERATIONS=50000`
- `DTM2_BENCH_COLD_ITERATIONS=500`
- `DTM2_BENCH_PLACEHOLDERS=50`
- `DTM2_BENCH_COMPRESS_HTML=true`
- `DTM2_BENCH_RELOAD_MODIFIED_TEMPLATES=false`
- `DTM2_BENCH_VALIDATE_EACH_ITERATION=false`

Benchmark result directories are generated locally under
`vlib/x/templating/dtm2/benchmarks/results/` and should not be committed.
