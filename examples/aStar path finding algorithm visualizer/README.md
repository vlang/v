# A Star path finding algorithm visualizer
This is a simple visualizer template for path finding algorithm wriiten in v lang

# Quick start 

make sure you have c lang installed or follow the instruction here 
- v language docs: [v language docs](https://github.com/vlang/v/blob/master/doc/docs.md) 
```console
# to run without compiling
$ v run aStar.v

# to compile and run 
$ v aStar.v # compile 

# run 
$ ./aStar
```

# Controls

- `q` : To quit the application
- `c`: clear the grid and start from new one 
- `space`: initialize path finding algorithm 


# Demo 
![Demo image of the algorithm](screenshots/demo.png)


# 🔴🔴🔴🔴 Area of improvments 🔴🔴🔴🔴🔴

- 🚧 Under Construction: We are using heap but that is not correctly implemneted instead of O(log(n)) it takes O(n) Thats why having more grid size will break the application.

- 🔍 Needs Attention: One of the function has warning that will be an error soon `reconstruct_path()`.

- 🌱 Growth Opportunity: make it responsive.
