@echo off

curl -L https://www.libsdl.org/release/SDL2-devel-2.0.10-VC.zip -o SDL2.zip
curl -L https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-devel-2.0.15-VC.zip -o SDL2_ttf.zip
curl -L https://www.libsdl.org/projects/SDL_image/release/SDL2_image-devel-2.0.5-VC.zip -o SDL2_image.zip
curl -L https://www.libsdl.org/projects/SDL_mixer/release/SDL2_mixer-devel-2.0.4-VC.zip -o SDL2_mixer.zip

unzip SDL2.zip -d thirdparty/
unzip SDL2_ttf.zip -d thirdparty/
unzip SDL2_image.zip -d thirdparty/
unzip SDL2_mixer.zip -d thirdparty/

move /y thirdparty/SDL2-2.0.10 thirdparty/SDL2
move /y thirdparty/SDL2_ttf-2.0.15 thirdparty/SDL2_ttf
move /y thirdparty/SDL2_image-2.0.5 thirdparty/SDL2_image
move /y thirdparty/SDL2_mixer-2.0.4 thirdparty/SDL2_mixer
