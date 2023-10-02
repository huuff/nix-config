{ pkgs, ... }: {
  dconf = {
    enable = true;
    settings = {
      "org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    };

  };

  home.packages = with pkgs; [ virt-manager ];
}
