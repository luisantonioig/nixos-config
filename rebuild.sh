#!/bin/sh
sudo nixos-rebuild switch --flake ~/nixos-config#system;

nix run home-manager/master --extra-experimental-features nix-command --extra-experimental-features flakes -- switch --flake ~/nixos-config#user;
