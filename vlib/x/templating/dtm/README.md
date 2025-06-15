# dtm - Dynamic Template Manager

A simple template manager integrated into the V project, designed to combine the power of V
templates with Vweb, without the need to recompile the application with every change.

## Quick Start

Using the dynamic template manager ( named '**dtm**' in this readme) is relatively straightforward.
You just need to initialize an instance. Then, call the '**expand**' function in the code that
manages your template.

Before starting, You can specify a folder, but it is not mandatory to store the generated cache. If
nothing is specified or if there is a problem with the targeted folder (for example, a permission
issue), the DTM will attempt to create a cache folder in the temporary file area of your OS.
Finally, if all this does not prove successful, then the DTM will disable the cache system and
notify the user if the cache system was previously required.

However, at the root directory of your project, you need to create a '**templates**' folder. The
absence of these directories will prevent you from using the dtm. You must add your template files
into the folder you previously created. ***Be aware that if the templates are not placed in the
correct directory, the DTM will return an error message indicating that it cannot find the
template!***

The DTM currently only works with 2 types of files:

- html
- raw text

Below you will find 2 usage contexts:

### 1. Minimal static generator example :

```v
import x.templating.dtm

fn main() {
	mut dtmi := dtm.initialize()

	// No need to add this 'defer' if you have chosen to disable the cache system in the options.
	defer {
		dtmi.stop_cache_handler()
	}
	mut tmp_var := map[string]dtm.DtmMultiTypeMap{}
	tmp_var['title'] = 'V dtm best title'
	tmp_var['non_string_type'] = 7
	tmp_var['html_section_#includehtml'] = '<span>will <br> be <br> escaped <br> in <br> text mode</span>'

	render := dtmi.expand('test.txt', placeholders: &tmp_var)

	println(render)
}
```

#### and its template text format :

```
Title of text : @title
Value in the text : @non_string_type
HTML tags are always escaped in text file : @html_section
```

### 2. Minimal Vweb example:

```v
import veb
import x.templating.dtm
import os

pub struct App {
pub mut:
	dtmi &dtm.DynamicTemplateManager = unsafe { nil }
}

pub struct Context {
	veb.Context
}

fn main() {
	cache_folder_path := os.join_path(os.dir(os.executable()), 'vcache_dtm')
	mut app := &App{
		dtmi: dtm.initialize(def_cache_path: cache_folder_path)
	}
	// No need to add this 'defer' if you have chosen to disable the cache system in the options.
	defer {
		app.dtmi.stop_cache_handler()
	}

	/*
    Here is an example of init configuration :

	dtm.initialize(
      def_cache_path: cache_folder_path
	  compress_html: false
	  active_cache_server: false
	  max_size_data_in_mem: 100
    )
	*/

	veb.run[App, Context](mut app, 18081)
}

@['/']
pub fn (mut app App) index(mut ctx Context) veb.Result {
	mut tmpl_var := map[string]dtm.DtmMultiTypeMap{}
	tmpl_var['title'] = 'The true title'
	html_content := app.dtmi.expand('index.html', placeholders: &tmpl_var)
	return ctx.html(html_content)
}
```

#### and its template html format :

```html
<!doctype html>
<html>
	<head>
		<title>@title</title>
	</head>
	<body>
		<div id="container">
			<h1>@title</h1>
		</div>
	</body>
</html>
```

You have a ready-to-view demonstration available
[here](https://github.com/vlang/v/tree/master/vlib/vweb/tests/dynamic_template_manager_test_server).

## Available Options

There are two types of option possibilities:

- Specific to the initialization
- Defined for each call of expand function

### Specific to the initialization

Three parameters are available:

- `def_cache_path` : ( **String** value ) User can define the path of cache folder.
- `max_size_data_in_mem` : ( **Int** value ) Maximum size of data allowed in memory for each cached
  template. The value must be specified in kilobytes. ( Default is: 500KB / Limit max is : 500KB )
- `compress_html` : ( **Bool** value ) Light '**minifier**' of the HTML output, to remove all
unnecessary spacing. ( Default is true, parameter taken into account only for HTML files )
- `active_cache_server` : ( **Bool** value ) Activate or not the template cache system. ( Default
  is true, ***_Highly recommended to keep it enabled for optimal performance_*** )

Regarding the `compress_html` option, it is recommended for performance reasons to disable it
when working directly with raw template generation (i.e., with the cache system disabled).

Use it like this :

```v ignore
initialize(
    def_cache_path: 'your/directorie/cache/path'
    max_size_data_in_mem: 500
    compress_html: true
    active_cache_server: true
)
```

### Defined for each call of expand function

- `placeholders` ( **&map[string]DtmMultiTypeMap** value ) Used to map placeholders within the
  template to their corresponding content, facilitating dynamic content insertion, by specifying
  values in the placeholders map. Templates can dynamically display content.

- `cache_delay_expiration` ( **i64** value ) Specifies the cache expiration time for the concerned
  page in seconds. ( Default value is **86400** seconds or one day ). You can add any value you
  want in seconds as long as it remains within the indicated range ( see below ).

Possibility to use already defined cache delay constants like:

- `cache_delay_expiration_at_min` : five minutes
- `cache_delay_expiration_at_max` : one year
- `cache_delay_expiration_by_default` : one day

For specific cases, you can cancel the generation and use of cache file, even if the cache system
is active :

- `cache_delay_expiration` : -1

Or set a cache that will never expire:

- `cache_delay_expiration` : 0

Example :

```v ignore
expand('path/of/template.html',
		placeholders: &the_map_var
		cache_delay_expiration: -1
)
```

## The PlaceHolders System

### On The User's Side Code :

The placeholder system allows for the insertion of dynamic content into your template. As of the
current state of the module, it accepts the following types like:

```
- string
- i8, i16, int, i64
- u8, u16, u32, u64
- f32, f64
```

Example:

```v ignore
mut plhs := map[string]dtm.DtmMultiTypeMap{}
plhs['placeholder_name_1'] = "title content"
plhs['placeholder_name_2'] = 123456
plhs['placeholder_name_3_#includehtml'] = "<p>allow to include</p><span>certain HTML tags</span>"

expand('path/of/template.html',
		placeholders: &plhs
)
```

Pay attention to this particular tag: "**_#includehtml**", it enables you to include HTML in the
dynamic content. Without this tag, all characters will be escaped for obvious security reasons. By
using this tag, only certain HTML tags are allowed. Here is the list:

```html
'<div>', '</div>', '<h1>', '</h1>', '<h2>', '</h2>', '<h3>', '</h3>', '<h4>',
'</h4>', '<h5>', '</h5>', '<h6>', '</h6>', '<p>', '</p>', '<br>', '<hr>', '<span>', '</span>',
'<ul>', '</ul>', '<ol>', '</ol>', '<li>', '</li>', '<dl>', '</dl>', '<dt>', '</dt>', '<dd>',
'</dd>', '<menu>', '</menu>', '<table>', '</table>', '<caption>', '</caption>', '<th>', '</th>',
'<tr>', '</tr>', '<td>', '</td>', '<thread>', '</thread>', '<tbody>', '</tbody>', '<tfoot>',
'</tfoot>', '<col>', '</col>', '<colgroup>', '</colgroup>', '<header>', '</header>', '<footer>',
'</footer>', '<main>', '</main>', '<section>', '</section>', '<article>', '</article>', '<aside>',
'</aside>', '<details>', '</details>', '<dialog>', '</dialog>', '<data>', '</data>', '<summary>',
'</summary>'
```

#### Note that with a raw text template, all HTML tag inclusions are escaped.

### On The Template Side :

An example of a template, corresponding to the previous subsection:

```html
<!doctype html>
<html>
	<head>
		<title>@placeholder_name_1</title>
	</head>
	<body>
		<div id="container">
			<h1>@placeholder_name_1</h1>
			<p>@placeholder_name_2</p>
			@placeholder_name_3
		</div>
	</body>
</html>
```

You will note that the `'_#includehtml'` directive is not found in the template with
`'@placeholder_name_3'`, and this is entirely normal. Directives are specially handled by the DTM,
and including them in the name of your placeholders within the template will result in the
placeholder not being found because it does not match the key name defined in the map containing
the dynamic content.

Like the traditional template system in V, inclusions or placeholders start with the '**@**'
character. The traditional inclusion system is still perfectly usable, such as:

```
- @include 'my/html/path.html'
- @css 'my/css/path.css'
- @js 'my/js/path.js'
```

## In The Future

As you've understood, the DTM is still under development and optimization. There are
functionalities to be added, such as data compression, managing loops or conditions within the
template itself. Able to be used in contexts other than HTML and raw text.

This will come in time.

Feel free to report any issues or contribute to the project!
