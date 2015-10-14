GIT_HASH=`git rev-parse --short HEAD`

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

7z a -tzip infinitybox-$GIT_HASH.zip infinitybox-r/
