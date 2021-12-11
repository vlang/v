# vviewer
Image viewer for V

This is an example of image viewer written in V.

## Usage

the program can be invoked by the command line:

file list: `viewer img1.jpg inmg2.bmp img3.tga`

folder list: `viewer folder1 folder2`

all the folder are scanned to find images

mixed list: `viewer img1.jpg img2.bmp folder1 folder2 img2.tga`

## Interactive usage

run the viewer than drag and drop files and folders on it.

## Accepted image format

JPEG, PNG, BMP, PSD, TGA, GIF (not animated), HDR, PIC, PNM

## Key bindings		

ESC/q - Quit
cursor right    - Next image
cursor  left      - Previous image
cursor  up       - Next folder
cursor  down  - Previous folder
F - Toggle full screen
R - Rotate image of 90 degree
I - Toggle the info text

mouse wheel - next/previous images
keep pressed left  Mouse button - Pan on the image
keep pressed right Mouse button - Zoom on the image

Author:

Dario Deledda 2021 (c)