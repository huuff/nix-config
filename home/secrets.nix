{ ... }: {
  sops = {
    defaultSopsFile = ../secrets.yaml;
    age = {
      keyFile = "/home/haf/.config/sops/age/keys.txt";
      # autogenerate the above key if it doesn't exist
      generateKey = true;
    };

    secrets = {
      openrouterApiKey.path = "%r/openrouter_api_key";
    };
  };
}
