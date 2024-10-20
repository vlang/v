```sh
Starting benchmark...
max_iterations: 1000000

***Structure and maps***
SPENT  534.429 ms in decoder2.decode[Stru](json_data)!
SPENT  342.979 ms in old_json.decode(Stru, json_data)!

SPENT  150.442 ms in decoder2.decode[StructType[string]](json_data1)!
SPENT   92.664 ms in old_json.decode(StructType[string], json_data1)!

SPENT  172.836 ms in decoder2.decode[StructTypeOption[string]](json_data1)!
SPENT  126.346 ms in old_json.decode(StructTypeOption[string], json_data1)!

SPENT  138.684 ms in decoder2.decode[StructType[int]](json_data2)!
SPENT   85.761 ms in old_json.decode(StructType[int], json_data2)!

SPENT  127.400 ms in decoder2.decode[map[string]string](json_data1)!
SPENT  191.189 ms in old_json.decode(map[string]string, json_data1)!


***arrays***
SPENT  353.024 ms in decoder2.decode[[]int]('[1, 2, 3, 4, 5, 6, 7, 8, 9, 0]')!
SPENT  492.547 ms in old_json.decode([]int, '[1, 2, 3, 4, 5, 6, 7, 8, 9, 0]')!


***simple types***
SPENT   32.242 ms in decoder2.decode[int]('2')!
SPENT   28.266 ms in decoder2.decode[bool]('true')!
SPENT   38.225 ms in decoder2.decode[time.Time]('2022-03-11T13:54:25')!
SPENT   42.012 ms in decoder2.decode[string]('"abcdefghijklimnopqrstuv"')!
```
