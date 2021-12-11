# vviewer
Image viewer for V

This is an example of a simple image viewer written in V.

## Usage

the program can be invoked by the command line:

file list: `viewer img1.jpg inmg2.bmp img3.tga`
folder list: `viewer folder1 folder2`
zip list: `viewer folder1.zip folder2.zip`

All the folder/zips are scanned in order to find images
The user can also mix the sources like files , folders and zips.

mixed list: `viewer img1.jpg img2.bmp folder1 folder2 img2.tga folder1.zip` 

## Interactive usage

run the viewer than drag and drop files,folders and zips on it.

## Accepted image format

JPEG, PNG, BMP, PSD, TGA, GIF (not animated), HDR, PIC, PNM

#### Functions
The user can navigate through the files passed to the viewer.
On each file can do the following operations:

- **Pan**, move over the image
- **Zoom**, magnify or reduce the image
- **Rotate**, rotate by 90 degree steps

## Key bindings		

**H** - show this help

**ESC/q** - Quit
**cursor right**  - Next image
**cursor  left**  - Previous image
**cursor  up**    - Next folder
**cursor  down**  - Previous folder
**F** - Toggle full screen
**R** - Rotate image of 90 degree
**I** - Toggle the info text

**mouse wheel** - next/previous images
keep pressed **left  Mouse button** - Pan on the image
keep pressed **right Mouse button** - Zoom on the image

#### Author:

Dario Deledda 2021 (c)