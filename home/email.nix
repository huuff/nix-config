{ pkgs, secrets, ... }:
{
  programs.himalaya = {
    enable = true;
  };

  accounts.email.accounts = {
    work =  (secrets.emailAccounts { inherit pkgs; }).work;
  };
}
