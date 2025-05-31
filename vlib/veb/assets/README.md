# Assets

The asset manager for veb. You can use this asset manager to minify CSS and JavaScript files,
combine them into a single file and to make sure the asset you're using exists.

## Usage

Add `AssetManager` to your App struct to use the asset manager.

**Example:**

```v
module main

import veb
import veb.assets

pub struct Context {
	veb.Context
}

pub struct App {
pub mut:
	am assets.AssetManager
}

fn main() {
	mut app := &App{}
	veb.run[App, Context](mut app, 8080)
}
```

### Including assets

If you want to include an asset in your templates you can use the `include` method.
First pass the type of asset (css or js), then specify the "include name" of an asset.

**Example:**

```html
@{app.am.include(.css, 'main.css')}
```

Will generate

```html
<link rel="stylesheet" href="/main.css" />
```

### Adding assets

To add an asset use the `add` method. You must specify the path of the asset and what its
include name will be: the name that you will use in templates.

**Example:**

```v ignore
// add a css file at the path "css/main.css" and set its include name to "main.css"
app.am.add(.css, 'css/main.css', 'main.css')
```

### Minify and Combine assets

If you want to minify each asset you must set the `minify` field and specify the cache
folder. Each assest you add is minifed and outputted in `cache_dir`.

**Example:**

```v ignore
pub struct App {
pub mut:
	am assets.AssetManager = assets.AssetManager{
		cache_dir: 'dist'
		minify: true
	}
}
```

To combine the all currently added assets into a single file you must call the `combine` method
and specify which asset type you want to combine.

**Example:**

```v ignore
// `combine` returns the path of the minified file
minified_file := app.am.combine(.css)!
```

### Handle folders

You can use the asset manger in combination with veb's `StaticHandler` to serve
assets in a folder as static assets.

**Example:**

```v ignore
pub struct App {
	veb.StaticHandler
pub mut:
	am assets.AssetManager
}
```

Let's say we have the following folder structure:

```
assets/
├── css/
│   └── main.css
└── js/
	└── main.js
```

We can tell the asset manager to add all assets in the `static` folder

**Example:**

```v ignore
fn main() {
	mut app := &App{}
	// add all assets in the "assets" folder
	app.am.handle_assets('assets')!
	// serve all files in the "assets" folder as static files
	app.handle_static('assets', false)!
	// start the app
	veb.run[App, Context](mut app, 8080)
}
```

The include name of each minified asset will be set to its relative path,
so if you want to include `main.css` in your template you would write
`@{app.am.include('css/main.css')}`

#### Minify

If you add an asset folder and want to minify those assets you can call the
`cleanup_cache` method to remove old files from the cache folder
that are no longer needed.

**Example:**

```v ignore
pub struct App {
	veb.StaticHandler
pub mut:
	am assets.AssetManager = assets.AssetManager{
		cache_dir: 'dist'
		minify: true
	}
}

fn main() {
	mut app := &App{}
	// add all assets in the "assets" folder
	app.am.handle_assets('assets')!
	// remove all old cached files from the cache folder
	app.am.cleanup_cache()!
	// serve all files in the "assets" folder as static files
	app.handle_static('assets', false)!
	// start the app
	veb.run[App, Context](mut app, 8080)
}
```

#### Prefix the include name

You can add a custom prefix to the include name of assets when adding a folder.

**Example:**

```v ignore
// add all assets in the "assets" folder
app.am.handle_assets_at('assets', 'static')!
```

Now if you want to include `main.css` you would write
``@{app.am.include('static/css/main.css')}`
