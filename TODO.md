BETA RELEASE 1

[ ] Fix inverted head pitch
[ ] Add local physics sim option
[ ] Adjust falloff rate for sound sources (add configuration to pd-haskell)
[ ] Haptic feedback for touching cubes
[x] Add Icon http://stackoverflow.com/questions/708238/how-do-i-add-an-icon-to-a-mingw-gcc-compiled-executable

[ ] Remove Console
      Use:
      -optl-mwindows ??

      http://stackoverflow.com/questions/5995433/removing-console-window-for-glut-freeglut-glfw
      http://trac.haskell.org/gtk2hs/ticket/1276
      http://stackoverflow.com/questions/7993912/how-do-i-stop-the-console-window-from-appearing-when-making-system-calls

BUGS

[ ] Fix unreliable multiplayer

[ ] Reduce physics server latency (only cache one frame?)

[ ] Add touch-blobbing inside cubes





[ ] Render to each eye simultaneously
      http://vrfocus.com/archives/23546/epic-games-bullet-train-runs-at-a-solid-90-fps-much-higher-than-1080p/

[ ] Apply handOffset right at the point of putting the hands into the player struct rather than offsetting all over the codebase

FEATURES

[ ] Grabbing
[ ] FFT as texture
[ ] Add viscosity