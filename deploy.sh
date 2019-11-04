npm run build
zip -r build.zip build
scp build.zip kevinl.io:/home/chokboy/blog
ssh kevinl.io "cd blog && rm -r build && unzip build.zip"
