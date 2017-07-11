{ config, pkgs, lib ? pkgs.lib, ... }:

with lib;

let 
  cfg = config.services.blog;
in
{
  # interface
  options = {

    services.blog = rec {

      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run the blog service;
        '';
      };

      package = mkOption {
        type = types.path;
        description = ''
          The blog package;
        '';
      };

      prefix = mkOption {
        type = types.str;
        default = "/blog";
        description = ''
          The url to the blog from the root of the webserver.
        '';
      };

      datadir = mkOption {
        type = types.str;
        default = "${cfg.package}/blog";
        description = ''
          The path to the assets for the blog.
        '';
      };
    };
  };

  # implementation
  config = mkIf cfg.enable {

    services.blog.package = mkDefault ((import ./default.nix));  
    services.nginx = {
      enable = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      virtualHosts."localhost" = {
        locations.${cfg.prefix}.alias = "${cfg.datadir}";
      }; 
    };

  }; 
} 
