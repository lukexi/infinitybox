BETA RELEASE 1

[ ] Haptic feedback for touching cubes
[ ] Have server handle scaling up cubes to fix glitchy cubes

[ ] Add local physics sim option
[ ] Update physics rate to 90hz!
  [ ] C.f. thread about object weight
      http://steamcommunity.com/app/358720/discussions/0/535152511347254326/
[ ] Update to OpenAL API for locating hrtf files rather than copying them

[x] Adjust falloff rate for sound sources (add configuration to pd-haskell)
[x] Update to latest OpenAL-soft
[x] Update to latest OpenVR
[x] Add Icon http://stackoverflow.com/questions/708238/how-do-i-add-an-icon-to-a-mingw-gcc-compiled-executable
[x] Inaccurate head pitch sending over multiplayer, and probably OpenAL

[x] Remove Console
      Use -optl-mwindows in ghc-options
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
