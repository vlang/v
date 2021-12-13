# vviewer
Image viewer for V

This is an example of a simple image viewer written in V.

## Usage

The program can be invoked by the command line:

file list: `viewer img1.jpg inmg2.bmp img3.tga`
folder list: `viewer folder1 folder2`
zip list: `viewer folder1.zip folder2.zip`

All folders/zips are scanned for images.
The user can mix files, folders, and zips.

mixed list: `viewer img1.jpg img2.bmp folder1 folder2 img2.tga folder1.zip` 

## Interactive usage

Run the viewer then drag and drop files,folders and zips on it.

## Accepted image format

JPEG, PNG, BMP, PSD, TGA, GIF (not animated), HDR, PIC, PNM

#### Functions
The user can navigate through the files passed to the viewer.
The following operations can be performed on each image:

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
Hold **left  Mouse button** - Pan on the image
Hold  **right Mouse button** - Zoom on the image

#### Author:

Dario Deledda 2021 (c)