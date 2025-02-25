{ pkgs, ... }:
let

  # My shell aliases
  myAliases = {
    ls = "eza --icons --group-directories-first --git -l --color=always --no-time --no-user --no-permissions --no-filesize";
    cat = "bat";
    htop = "btm";
    fd = "fd -Lu";
    w3m = "w3m -no-cookie -v";
    neofetch = "disfetch";
    fetch = "disfetch";
    gitfetch = "onefetch";
    cd = "z";
    "," = "comma";
  };
in
{
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;
    shellAliases = myAliases;
    initExtra = ''
      PROMPT="%F{cyan}╭─%F{magenta}%n%f@%F{blue}%m%f %F{yellow}%~%f
      %F{cyan}╰─%F{green}❯%f "

      RPROMPT="%F{red}▂%f%F{yellow}▄%f%F{green}▆%f%F{cyan}█%f%F{blue}▆%f%F{magenta}▄%f%F{white}▂%f"

      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '

      bindkey '^P' history-beginning-search-backward
      bindkey '^N' history-beginning-search-forward
      eval "$(fzf --zsh)"
      eval "$(zoxide init zsh)"
    '';  
  };

  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellAliases = myAliases;
  };

  home.packages = with pkgs; [
    vivid
    disfetch lolcat cowsay onefetch
    gnugrep gnused
    bat eza bottom fd bc
    direnv nix-direnv
  ];

  programs.direnv.enable = true;
  programs.direnv.enableZshIntegration = true;
  programs.direnv.nix-direnv.enable = true;
}
