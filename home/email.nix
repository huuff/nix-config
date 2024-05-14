{ pkgs, secrets, ... }:
{
  programs.himalaya = {
    enable = true;
  };

  accounts.email.accounts = secrets.emailAccounts { inherit pkgs; };
}
