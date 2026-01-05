{
  description = "bry's nix-darwin configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin, home-manager, nix-homebrew }:
    let
      username = "bry";
    in
    {
      darwinConfigurations."zyrbbook" = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = { inherit inputs username; };
        modules = [
          ./hosts/zyrbbook
          nix-homebrew.darwinModules.nix-homebrew
          {
            nix-homebrew = {
              enable = true;
              enableRosetta = true;
              user = username;
              autoMigrate = true;
            };
          }
          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              backupFileExtension = "backup";
              extraSpecialArgs = { inherit inputs username; };
              users.${username} = import ./hosts/zyrbbook/home.nix;
            };
          }
        ];
      };

      darwinPackages = self.darwinConfigurations."zyrbbook".pkgs;
    };
}
