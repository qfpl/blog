{ config, pkgs, lib ? pkgs.lib, ... }:

with lib;

let 
  cfg = config.services.blog-updater;
in
{
  # interface
  options = {

    services.blog-updater = rec {

      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run the blog updating service
        '';
      };

      virtualHost = mkOption {
        type = types.str;
        default = "blog.qfpl.io";
        description = ''
          The virtual host to serve the blog content from
        '';
      };

      hydraJob = mkOption {
        type = types.str;
        default = "http://hydra.qfpl.io/job/team-blog/blog/blog";
        description = ''
          The url to the hydra job for the blog content
        '';
      };

      blogProfile = mkOption {
        type = types.str;
        default = "blog";
        description = ''
          The profile to run hail from
        '';
      };

    };
  };

  # implementation
  config = mkIf cfg.enable {

    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      virtualHosts."${cfg.virtualHost}" = {
        forceSSL = true;
        enableACME = true;
        locations."/".root = "/nix/var/nix/profiles/${cfg.blogProfile}/blog";
      };
    };

    systemd.services.blog-updater = {
      description = "Team blog updater";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.nix ];
      environment = { HOME = "/var/lib/empty"; };
      stopIfChanged = false;
      serviceConfig = {
        ExecStart = "${pkgs.haskellPackages.hail}/bin/hail --profile ${cfg.blogProfile} --job-uri ${cfg.hydraJob}";
      };
    };

  }; 
} 
