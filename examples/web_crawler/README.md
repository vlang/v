# web_crawler
web_crawler is a very simple web crawler made to learn V.  
This web crawler aimed to fetch news from tuicool.com(one web like hacker-news.firebaseio.com or something like that, but in chinese).  

# Compile and Run

Use this to generate a binary and then launch the web crawler.
```bash
v web_crawler.v
./web_crawler
```

And this to compile and launch the web crawler directly.
```bash
v run web_crawler.v
```

with this project we learn how to use http.fetch() to get http.Response and use html.parse() to parse the html.  
It's easy, isn't it?  