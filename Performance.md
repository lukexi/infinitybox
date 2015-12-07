
COST CENTRE                    MODULE                               %time %alloc

MAIN                           MAIN                                  14.6    0.5
encode'                        Network.UDP.Pal.Binary                 5.8   26.8
decode'                        Network.UDP.Pal.Binary                 4.6    5.0
setListHook.\                  Sound.Pd.Core                          4.1    5.0
infinityClient                 Client                                 3.3    0.4
processControls                Controls                               3.1    1.9
drawShape'                     Render                                 3.1    7.6
drawCubes.\                    Render                                 2.5    4.9
uniformV3                      Graphics.GL.Pal.Uniforms               2.0    1.0
drawCubes.\.model              Render                                 1.8    3.0
interpolateObjects             Types                                  1.8    1.5
recvBinaryFrom                 Network.UDP.Pal.Socket                 1.8    2.5
collectUnreliablePacket        Network.UDP.Pal.Reliable.ReliableUDP   1.6    0.9
updateAudio                    Audio                                  1.6    2.0
sendConn                       Network.UDP.Pal.Socket                 1.5    0.1
createTransceiver              Network.UDP.Pal.Reliable.Transceiver   1.4    0.1
uniformF                       Graphics.GL.Pal.Uniforms               1.4    0.3
acquireChannel                 Sound.Pd.Core                          1.2    0.1
renderOpenVR                   Graphics.VR.Pal.Window                 1.2    0.7
withVAO                        Graphics.GL.Pal.WithActions            1.2    1.4
drawRoom                       Render                                 1.1    1.6
drawCubes                      Render                                 1.1    2.0
drawLocalHands.\.finalMatrix   Render                                 1.1    1.5
drawPlayers                    Render                                 0.9    1.1
renderOpenVR.\.finalView       Graphics.VR.Pal.Window                 0.9    1.3
drawLocalHandles.\.finalMatrix Render                                 0.8    1.5
setLightUniforms               Render                                 0.5    1.1
updateAudio.\                  Audio                                  0.5    1.1



Performance plan:
[x] UBO to reduce redundant copying of camera info etc. to eyes

Render each eye simultaneously
Use instancing to render cubes

[x] Use a withPd helper to initialize audio and shut it down correctly, and to remove the need for a PdRef
[ ] Make sure encode and decode are occuring on secondary threads

[x] setListHook\ is from the amplitudes and pitches. Switch these to write to arrays
    and read those from the app
    On line 129 in Render.hs:
    Read voice pitches and amplitude from an array rather than one at a time!