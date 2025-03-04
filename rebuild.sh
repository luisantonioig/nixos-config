#!/bin/sh
sudo nixos-rebuild switch --flake ~/borrar/nixos-config#system;

# nix run home-manager/master --extra-experimental-features nix-command --extra-experimental-features flakes -- switch --flake ~/borrar/nixos-config#user --extra-experimental-features nix-command --extra-experimental-features flakes;
