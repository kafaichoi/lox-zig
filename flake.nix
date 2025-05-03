# flake.nix
{
  description = "A simple Zig 0.14.x development environment";

  inputs = {
    # Pin to nixpkgs-unstable for newer packages, or a specific commit for full reproducibility
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # --- Define Zig version ---
        # IMPORTANT: Verify the correct attribute for Zig 0.14.x on search.nixos.org/packages
        # It might be 'zig_0_14_0', 'zig_0_14', or simply 'zig' if 0.14.x is the latest default.
        # If 'zig_0_14_0' does not exist, try 'zig' and check its version,
        # or find a nixpkgs commit that has the version you need.
        zigForProject = pkgs.zig_0_14; # ADJUST THIS LINE AS NEEDED

        # --- Define ZLS (Zig Language Server) ---
        # pkgs.zls is usually the language server compatible with the main 'pkgs.zig'.
        # If using a specific/older Zig version, ensure this ZLS is compatible,
        # or look for a versioned ZLS package if available (e.g., pkgs.zls_0_14_0).
        zlsForProject = pkgs.zls;

      in
      {
        devShells.default = pkgs.mkShell {
          name = "zig-0.14-dev-shell";

          packages = [
            zigForProject
            zlsForProject
            # You can add other common development tools here, for example:
            # pkgs.git
            # pkgs.gdb # For debugging
            # pkgs.pkg-config # If your Zig project links against C libraries
          ];

          # Optional: You can add shell hooks or environment variables here
          # shellHook = ''
          #   echo "Welcome to the Zig ${zigForProject.version} dev environment!"
          #   echo "ZLS is also available."
          # '';
        };
      }
    );
}
