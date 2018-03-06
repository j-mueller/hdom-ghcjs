# render-worker 


## How to build

* Build with nix 
* A docker file for building a base image is provided (for example for Windows development)

### Development (build with cabal)

* `nix-shell release.nix -A client.env`
* `cabal new-build` etc.

### Build example app

* `nix-build release.nix assembled-assets` builds the JS scripts, runs them through the closure compiler and symlinks the files to `./result`

## License

BSD3

## Contributions

Bug reports, pull requests, feature requests are welcome
