<img src='https://raw.githubusercontent.com/vlang/v/master/examples/tetris/screenshot.png' width=300>

Tetris has a temporary dependency on GLFW. 

FreeType is needed to display the score and some text. A smaller stb library is going to be used instead.

## macOS
`brew install glfw freetype` 
 
## Ubuntu 
`sudo apt install libglfw3 libglfw3-dev libfreetype6-dev libssl-dev`

## Arch (and Manjaro)
`sudo pacman -S glfw-x11 freetype2` 

## Windows 
Copy `thirdparty/glfw/glfw3.dll` & `thirdparty/freetype/win64/freetype.dll` to this directory. 
