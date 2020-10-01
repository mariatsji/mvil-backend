with import <nixpkgs> {};

# builds a base image to extend with a stack-built binary
dockerTools.buildImage {
  name = "mvilbackend";
  tag = "latest";
  created = "now";
  # runtime system deps  and binary tools in the docker image goes here
  contents = [ postgresql ncurses libffi gmp busybox binutils bash curl zlib rdkafka cacert ];
}
