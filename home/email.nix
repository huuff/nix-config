{ pkgs, secrets, ... }:
{
  programs.himalaya = {
    enable = true;
  };

  accounts.email.accounts = {
    travail =  secrets.emailAccounts.work;
  };
}
