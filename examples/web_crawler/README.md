# web_crawler
web_crawler is a very simple web crawler.  
This web crawler fetches news from tuicool.com,
(a chinese site similar to hacker-news.firebaseio.com).  

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
