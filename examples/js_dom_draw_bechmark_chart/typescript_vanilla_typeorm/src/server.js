const http = require("http");
const fs = require("fs");
var path = require("path");
const { hello, sqlite_memory, appDataSource } = require("..");

const host = "localhost";
const port = 3000;

// const hello = require("../index")

const reqListener = async (req, res) => {
  console.log(`[route] - (${req.method}) ${req.url}`);

  var filePath = "." + req.url;
  // if (filePath == './') {
  //     filePath = './index.html';
  // }

  var extname = String(path.extname(filePath)).toLowerCase();
  var mimeTypes = {
    ".html": "text/html",
    ".js": "text/javascript",
    ".css": "text/css",
    ".json": "application/json",
    ".png": "image/png",
    ".jpg": "image/jpg",
    ".gif": "image/gif",
    ".svg": "image/svg+xml",
    ".wav": "audio/wav",
    ".mp4": "video/mp4",
    ".woff": "application/font-woff",
    ".ttf": "application/font-ttf",
    ".eot": "application/vnd.ms-fontobject",
    ".otf": "application/font-otf",
    ".wasm": "application/wasm",
  };

  var contentType = mimeTypes[extname] || "application/octet-stream";

  fs.readFile(filePath, function (error, content) {
    if (error) {
      if (error.code == "ENOENT") {
        fs.readFile("./404.html", function (error, content) {
          // res.writeHead(404, { "Content-Type": "text/html" });
          // res.end(content, "utf-8");
        });
      } else {
        res.writeHead(500);
        res.end(
          "Sorry, check with the site admin for error: " + error.code + " ..\n"
        );
      }
    } else {
      res.writeHead(200, { "Content-Type": contentType });
      res.end(content, "utf-8");
    }
  });

  // Routes
  if (req.url == "/" && req.method == "GET") {
    res.writeHead(200);
    res.end("adad");
  }

  if (req.url == "/hello-world" && req.method == "GET") {
    res.writeHead(200);
    res.end("hello world");
  }

  if (req.url.includes("/sqlite-memory/") && req.method == "GET") {
    var count = req.url.replace("/sqlite-memory/", "");

    await sqlite_memory(count).then((response) => {
      res.writeHead(200, { "Content-Type": "application/json" });
      res.end(JSON.stringify(response));
    });
  }
};

try {
  const server = http.createServer(reqListener);

  appDataSource.initialize();
  console.log("Database working");

  server.listen(port, host);
  console.log(`Server is running on http://${host}:${port}`);
} catch (error) {
  console.log(error);
}
