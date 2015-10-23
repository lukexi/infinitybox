GIT_HASH=`git rev-parse --short HEAD`
RELEASE_ZIP=infinitybox-$GIT_HASH.zip

rm -rf infinitybox-r/
mkdir infinitybox-r
stack build
stack install infinitybox --local-bin-path infinitybox-r/
cp -R dlls/* infinitybox-r/
cp -R src/ infinitybox-r/
cp -R shaders/ infinitybox-r/
cp -R patches/ infinitybox-r/
cp -R audio-prototypes/ infinitybox-r/
cp -R openal/ infinitybox-r/
cp README.txt infinitybox-r/
cp InfinityBoxIcon.ico infinitybox-r/

7z a -tzip $RELEASE_ZIP infinitybox-r/
cp $RELEASE_ZIP /c/Users/$USER/Desktop
cp -R infinitybox-r /c/Users/$USER/Desktop/