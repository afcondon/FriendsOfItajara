# The Friend is a static page over the daemon's socket. `make bundle` builds
# it and brings the shared stylesheet alongside; serve `static/` with anything.
.PHONY: build bundle serve
build:
	spago build
bundle: build
	spago bundle -p friends-of-itajara
	cp ../itajara/surface/looper.css static/looper.css
serve: bundle
	node server.mjs
