# dtm - Dynamic Template Manager

A simple template manager integrated into the V project, designed to combine the power of V templates with Vweb, without the need to recompile the application with every change.

**_This module is in the experimental phase, it lacks some features and may contain bugs_**

## Quick Start

Using the dynamic template manager ( named '**dtm**' in this readme) is relatively straightforward. You just need to create an instance and initialize it. 
Then, call the '**serve_dynamic_template**' function in the code that manages your web pages. 

Before starting, at the root directory of your vweb project, you need to create a '**templates**' folder as well as a '**vcache**' folder. The absence of these directories at the root of your project will prevent you from using the dtm.

Next, you must add your HTML file into the folder you previously created. ***Be aware that if the HTML templates are not placed in the correct directory, the DTM will return an error message indicating that it cannot find the template!!***

A minimal quick start example:

```
import vweb
import x.templating.dtm

struct App {
	vweb.Context
pub mut:
    // The vweb_global attribute must be applied in order to use the dtm 
    // (Dynamic Template Manager) throughout your application.
    // However, you do not need the attribute if you are using x.vweb
	dtm &dtm.DynamicTemplateManager = dtm.create_dtm() @[vweb_global] 
}

fn main() {
	mut app := &App{}

	dtm.initialize_dtm(mut app.dtm)!

	vweb.run(app, 18081)
}

@['/']
pub fn (mut app App) index() vweb.Result {
	html_content := app.dtm.serve_dynamic_template('index.html')
	return app.html(html_content)
}

```

You have a ready-to-view demonstration available [here](https://github.com/vlang/v/tree/master/vlib/vweb/tests/dynamic_template_manager_test_server).

## Available Options

There are two types of option possibilities:

- Specific to the initialization
- Defined for each web page

### Specific to the initialization

Three parameters are available:

- `max_size_data_in_mem` : ( **Int** value ) Maximum size of data allowed in memory for each cached template. The value must be specified in kilobytes. ( Default is: 500KB / Limit max is : 500KB )
- `compress_html` : ( **Bool** value ) Light compress of the HTML ouput, to remove all unnecessary spacing. ( Default is true )
- `active_cache_server` : ( **Bool** value ) Activate or not the template cache system. ( Default is true, ***_Highly recommended to keep it enabled for optimal performance_*** )

Using like this :

```
initialize_dtm(mut app.dtm,
    max_size_data_in_mem: 500
    compress_html: true
    active_cache_server: true
)!
```


### Defined for each web page

- `placeholders` ( **&map[string]DtmMultiTypeMap** value ) Used to map placeholders within the template to their corresponding content, facilitating dynamic content insertion, by specifying values in the placeholders map. Templates can dynamically display content.

- `cache_delay_expiration` ( **i64** value ) Specifies the cache expiration time for the concerned page in seconds. ( Minimum value is **300** seconds and maximum is **31536000** seconds, equivalent to five minutes and one year. Default value is **86400** seconds or one day ). You can add any value you want in seconds as long as it remains within the previously indicated range. 

Possibility to use already defined cache delay constants like: `cache_delay_expiration_at_min`, `cache_delay_expiration_at_max`, `cache_delay_expiration_by_default`

Using like this :

```
serve_dynamic_template('path/of/template.html',
		placeholders: &the_map_var
		cache_delay_expiration: cache_delay_expiration_by_default
)
```

## The PlaceHolders System

### On The User's Side Code :

The placeholder system allows for the insertion of dynamic content into your template. As of the current state of the module, it accepts the following types like:

```
- string
- i8, i16, int, i64
- u8, u16, u32, u64
- f32, f64
```

it is used as follows:

```
mut plhs := map[string]dtm.DtmMultiTypeMap{}
plhs['placeholder_name_1'] = "title content"
plhs['placeholder_name_2'] = 123456
plhs['placeholder_name_3_#includehtml'] = "<p>allow to include</p><span>certain HTML tags</span>"

serve_dynamic_template('path/of/template.html',
		placeholders: &plhs
)
```
Pay attention to this particular tag: "**_#includehtml**", it enables you to include HTML in the dynamic content. Without this tag, all characters will be escaped for obvious security reasons. By using this tag, only certain HTML tags are allowed. Here is the list:

```
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

### On The Template Side :

An example of a template, corresponding to the previous subsection:

```
<!DOCTYPE html>
<html>
  <head>
    <title>@placeholder_name_1</title>
  </head>
  <body>
    <div id="container">
      <H1>@placeholder_name_1</H1>
      <p>@placeholder_name_2</p>
      @placeholder_name_3
    </div>
  </body>
</html>
```
You will note that the `'_#includehtml'` directive is not found in the template with `'@placeholder_name_3'`, and this is entirely normal. Directives are specially handled by the DTM, and including them in the name of your placeholders within the template will result in the placeholder not being found because it does not match the key name defined in the map containing the dynamic content.


Like the traditional template system in V, inclusions or placeholders start with the '**@**' character. The traditional inclusion system is still perfectly usable, such as:

```
- @include 'my/html/path.html'
- @css 'my/css/path.css'
- @js 'my/js/path.js'
```

## In The Future

As you've understood, the DTM is still under development and optimization. There are functionalities to be added, such as data compression, managing loops or conditions within the template itself. Able to be used in contexts other than HTML, such as text files, and for modules other than vweb.

This will come in time.

Feel free to report any issues or contribute to the project!