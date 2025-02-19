{ pkgs-stable, ... }:

{
  # Fonts are nice to have
  fonts.packages = with pkgs-stable; [
    # Fonts
    ibm-plex
    nerdfonts
    powerline
  ];

}
