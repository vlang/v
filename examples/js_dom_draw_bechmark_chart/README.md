# To run app
Dockerfile
[docker build]=> Docker image
[docker run]=> Docker container

`sudo docker build -t <name> .`

`sudo docker run --name <container name> --interactive --tty --publish 3001:3001 <name>`

`v run .`

A message like `[Vweb] Running app on http://localhost:3001/` should appear

`exit`

# To implement new bechmarks

create a function to bench in main.v like `fn any_function() []int {}`
that must factory return the array of time spended.
So set the attribute in canvas (with id "canvas_insert_id")  
In draw.js.v put the attribute name inside of attribute_names array


# ROADMAP
02/09/2022
- [ ] select bench (easy)
- [ ] vsql (easy)