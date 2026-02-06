# web_crawler
This simple web crawler fetches news from the homepage of HN (https://news.ycombinator.com/).

# Compile and Run

Use this to generate an executable, and then launch the web crawler:
```bash
v web_crawler.v
./web_crawler
```

And this to compile and launch the web crawler directly:
```bash
v run web_crawler.v
```

This project shows how to use http.fetch() to get http.Response, 
and then html.parse() to parse the returned html.

It's easy, isn't it?
