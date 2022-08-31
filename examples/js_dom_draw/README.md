Drawing with mouse events using DOM API. Adopted from MDN examples.

# Compiling
```
v -b js_browser examples/js_dom_draw/draw.js.v
```

Then you can open `index.html` with your favourite browser.
# Serve examples

### JS server
Afer run `npm init -y` code and genared `./package.json`
You can put `start` and `build` at script in jason leaf.
`path './package.json'`
```json
  "scripts": {
    "start": "npm run build && node server.js",
    "build":"v -b js_browser draw.js.v"
  },

```
here is the pure javascript server code
`path './server.js'`

```javascript
const http = require("http");
const fs = require("fs");
var path = require('path');

const host = "localhost";
const port = 3000;

const reqListener = function (req, res) {
    console.log('[route] - ', req.url);

    var filePath = '.' + req.url;
    if (filePath == './') {
        filePath = './index.html';
    }

    var extname = String(path.extname(filePath)).toLowerCase();
    var mimeTypes = {
        '.html': 'text/html',
        '.js': 'text/javascript',
        '.css': 'text/css',
        '.json': 'application/json',
        '.png': 'image/png',
        '.jpg': 'image/jpg',
        '.gif': 'image/gif',
        '.svg': 'image/svg+xml',
        '.wav': 'audio/wav',
        '.mp4': 'video/mp4',
        '.woff': 'application/font-woff',
        '.ttf': 'application/font-ttf',
        '.eot': 'application/vnd.ms-fontobject',
        '.otf': 'application/font-otf',
        '.wasm': 'application/wasm'
    };

    var contentType = mimeTypes[extname] || 'application/octet-stream';

    fs.readFile(filePath, function(error, content) {
        if (error) {
            if(error.code == 'ENOENT') {
                fs.readFile('./404.html', function(error, content) {
                    res.writeHead(404, { 'Content-Type': 'text/html' });
                    res.end(content, 'utf-8');
                });
            }
            else {
                res.writeHead(500);
                res.end('Sorry, check with the site admin for error: '+error.code+' ..\n');
            }
        }
        else {
            res.writeHead(200, { 'Content-Type': contentType });
            res.end(content, 'utf-8');
        }
    });
};

const server = http.createServer(reqListener);
server.listen(port, host, () => {
  console.log(`Server is running on http://${host}:${port}`);
});

```

This project has a no dependence serve in `./server.js` path.
That can be run with `node` command after build.

or just run: `npm run start`

To install node, you can access node [download page](https://nodejs.org/en/download/) 
or [package manager](https://nodejs.org/en/download/package-manager)

```
$ cd examples/js_dom_draw/
$ npm run build
```


### V server
```v ignore
module main

import vweb
import os

const (
    http_port = 3001
)

struct App {
    vweb.Context
}

fn main() {
    vweb.run(new_app(), http_port)
}

pub fn (mut app App) before_request() {
    // This build server json files
    os.execute_or_panic('v -b js_browser draw.js.v ')
}

fn new_app() &App {
    mut app := &App{}
    app.serve_static('/favicon.ico', 'favicon.ico')
    app.serve_static('/draw.js', 'draw.js')
    app.mount_static_folder_at(os.resource_abs_path('.'), '/')
    return app
}

['/'; get]
pub fn (mut app App) controller_get_all_task() vweb.Result {
    file :=os.read_file('./index.html') or { panic(err) }
    return app.html(file)
}
```