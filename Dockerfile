FROM nixos/nix:2.20.5

RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

COPY flake.nix /opt/bellkat/flake.nix
COPY flake.lock /opt/bellkat/flake.lock
COPY package.yaml /opt/bellkat/package.yaml

WORKDIR /opt/bellkat

RUN nix develop

COPY . /opt/bellkat

RUN nix build

RUN nix profile install .\#bellkatGHCWithFC --impure

ENTRYPOINT ["runhaskell"]
