rm -rf release-tmp/
cp -R release/ release-tmp/
stack build
stack install infinitybox --local-bin-path release-tmp/
cp -R src/ release-tmp/
cp -R patches/ release-tmp/
cp -R audio-prototypes/ release-tmp/
cp -R openal/ release-tmp/
GIT_HASH=`git rev-parse --short HEAD`
tar -zcvf infinitybox-$GIT_HASH.tar.gz release-tmp/