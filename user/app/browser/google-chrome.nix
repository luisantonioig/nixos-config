{ pkgs, ... }:

{
  # Module installing google-chrome as default browser
  home.packages = [ pkgs.google-chrome ]; # Instala Google Chrome

  xdg.desktopEntries.google-chrome = {
    name = "Google Chrome";
    genericName = "Web Browser";
    exec = "google-chrome-stable %U";
    terminal = false;
    icon = "google-chrome";
    type = "Application";
    categories = [ "Network" "WebBrowser" ];
    mimeType = [
      "text/html"
      "x-scheme-handler/http"
      "x-scheme-handler/https"
    ];
  };

  xdg.mimeApps.defaultApplications = {
    "text/html" = [ "google-chrome.desktop" ];
    "x-scheme-handler/http" = [ "google-chrome.desktop" ];
    "x-scheme-handler/https" = [ "google-chrome.desktop" ];
  };

  home.sessionVariables = {
    DEFAULT_BROWSER = "${pkgs.google-chrome}/bin/google-chrome-stable";
  };

}
