A small Pong game written in V using the `gg` and `sokol.audio` modules.

## Features
-   **Proportional Resizing**: The game scales ball, paddles, and speeds based on the window size.
-   **Audio Effects**: Synthesized sounds for wall bounces, paddle hits, and scoring.
-   **Modern-Retro UI**: Deep blue background with vibrant accent colors.
-   **Real-time Metrics**: Displays ball speed in m/s (assuming a 10m wide playfield).

## AI use note: 
This example was created by Google gemini-cli `Auto (Gemini 3)`, using the following prompts:

1. create a small Pong game using gg in examples/gg/pong.v .
2. the game should start not paused. Show the current score for each player on the corresponding side of the screen. Show the current vertical position for each player below the score too. Show a green "PAUSED, press space to unpause" label in the bottom middle of the screen, when the game is paused. Show a yellow label "Press space - pause; W/S - left player; Up/Down - right player." in the bottom middle of the screen, when the game is not paused.
3. make the score and position labels green
4. make the ball be a filled white circle
5. make the paddles be slightly rounded
6. make the pause key work when a key_down event is received; it is not like the other keys
7. make the background very dark blue
8. be a good game designer, and tweak the colors and positions of all the screen elements, if they do not look good.
9. move the bottom status text even more down, so that the last stripe of the center line does not intersect it. Move the paused label in the center of the screen and make it bigger
10. move the paused text about 80 pixels to the top; make the bottom status text slightly bigger
11. move the paused text about 30 pixels down
12. use the `sokol.audio` module and add simple sound effects to the game: when the ball bounces on a wall, it should produce a ping sound; when the ball bounces on a paddle it should produce a pong sound (slightly lower pitched); when the ball escapes behind a paddle, and the game resets, produce a low pitched buzzing sound
13. the sounds work fine, but there is a very slight click at the end, that is especially pronounced at the end of the buzz sound
14. the buzz sound still has a slight click at the end
15. make the left paddle bight red, and the right paddle bright blue
16. assume that the whole playfield is 10 meters wide (5 on the left and 5 on the right side). Calculate the ball speed. Show the ball speed in the top center of the screen (for example `Ball: 5.2 m/s`, note it should be always rounded at the first digit after the dot (i.e. show 5 as `Ball: 5.0 m/s`).
17. make sure to resize the ball, paddles and playfield proportionally, when the screen is maximized
18. append all my current user prompts to examples/gg/pong/README.md
