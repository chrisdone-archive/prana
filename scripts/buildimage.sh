echo -n "Building image ..."
docker image build -t ghc-compile -f Dockerfile.ghc-8.0 .
echo done.
