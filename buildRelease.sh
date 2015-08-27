stack build
stack install infinitybox --local-bin-path release
cp -R src/ release/
cp -R audio-prototypes/ release/
tar -zcvf infinitybox.tar.gz release/