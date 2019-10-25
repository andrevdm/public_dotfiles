{ config, pkgs, lib, ... }:

{
    services.postgresql = {
    enable = true;
    package = pkgs.postgresql_11;
    authentication = pkgs.lib.mkForce ''
      local all all              trust
      host  all andre 127.0.0.1/32 trust
      host  all all 127.0.0.1/32 trust
      host  all all ::1/128      password
    '';
  };
}
