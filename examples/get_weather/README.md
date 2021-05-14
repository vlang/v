# get_weather
get_weather is a very simple web crawler.
Its goal is to get a weather forecast from caiyunapp.com.  

# Compile and Run

Use this to generate an executable and then launch the web crawler.
```bash
v get_weather.v
./get_weather
```

As a convenience, you can also compile and launch the web crawler directly.
```bash
v run get_weather.v
```

In this project we use http.fetch() to get a http.Response, with a
custom user-agent and then we use json.decode() to decode the json 
response to struct.
We also use a `[skip]` attribute to skip certain fields in the response,
that we don't need and use a `[json: result]` attribute to specify that
our struct field is named differently from the incoming json response.
