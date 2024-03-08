FROM nixos/nix:2.20.5

RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

COPY flake.nix /opt/bellkat/flake.nix
COPY flake.lock /opt/bellkat/flake.lock
COPY package.yaml /opt/bellkat/package.yaml

WORKDIR /opt/bellkat

COPY . /opt/bellkat

RUN nix profile install .\#bellkatGHC --impure

CMD ["runhaskell"]
