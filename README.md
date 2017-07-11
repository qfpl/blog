This is a the FPL team blog.

Build this locally with: 
```
nix-build
```
where the output will be in `./release/blog`

Get to a local preview server with:
```
nix-shell
cabal configure
cabal build
./dist/build/site/site watch
```
where the content will be served from http://localhost:8000/

There is a post in there describing how to contribute, at http://localhost:8000/posts/writing-for-the-fp-blog.html
