# get_weather

get_weather is a web crawler. Its goal is to get a weather forecast from
`https://api.caiyunapp.com` in chinese and translated to another selected language.

We use `http.fetch()` to get a forecast `http.Response`, with a custom user-agent
and then we decode the json into a struct with only relevant fields `lang`,
`result` and `forecast_keypoint`.

The chinese texts are translated with another `http.fetch()` to
`https://translate.googleapis.com` and then decoding the `http.Response` as `json2.Any` arrays.

## running

By default texts are translated to English. Another language can be indicated
with first argument as an [ISO 639](https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes) code:

```bash
$ v run examples/get_weather/get_weather.v
   zh_CN: 未来两小时天气
      en: Weather in the next two hours
   zh_CN: 最近的降雨带在西北66公里外呢
      en: The nearest rainfall zone is 66 kilometers northwest
$
$ v run examples/get_weather/get_weather.v es
   zh_CN: 未来两小时天气
      es: Clima en las próximas dos horas
   zh_CN: 最近的降雨带在西北66公里外呢
      es: La zona de lluvia más cercana está a 66 kilómetros al noroeste
```

