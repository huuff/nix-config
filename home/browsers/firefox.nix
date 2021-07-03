{pkgs, ...}:
{
  programs.firefox = {
    enable = true;
    profiles = {
      default = {
        isDefault = true;
      };
    };
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      ublock-origin
      leechblock-ng
      i-dont-care-about-cookies
      decentraleyes
      privacy-badger
      https-everywhere
      pkgs.nur.repos.rycee.firefox-addons."1password-x-password-manager"
    ];
  };
}
