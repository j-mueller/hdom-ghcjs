FROM lnl7/nix:1.11.16
LABEL maintainer="jann.mueller@lbrm.de"

RUN ["nix-env", "-i", "cabal-install", "-i", "cabal2nix", "-i", "ghc", "-i", "closure-compiler"]

COPY default.nix /data/docker/default.nix
COPY release.nix /data/docker/release.nix
COPY nixpkgs.json /data/docker/nixpkgs.json

WORKDIR /data/docker
RUN ["nix-shell", "release.nix", "-A", "client.env", "--run", "echo Hello"]
RUN ["nix-shell", "release.nix", "-A", "assembled-assets.env", "--run", "echo Hello"]

CMD ["bash"]
