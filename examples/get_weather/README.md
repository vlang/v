# get_weather
get_weather is a very simple web crawler made to learn V.  
This project aimed to get weather forecast from caiyunapp.com.  

# Compile and Run

Use this to generate a binary and then launch the web crawler.
```bash
v get_weather.v
./get_weather
```

And this to compile and launch the web crawler directly.
```bash
v run get_weather.v
```

in this project we learn how to use http.fetch() to get http.Response with custom user-agent and use json.decode() to decode response json to struct.  
also we learn how to use \[skip\] attribute to skip certain fields that we don't need and use \[json: result\] attribute to specified different field name in JSON and struct.  