{
  allowUnfree = true;

  # The package sets defined here can be installed with `nix-env` like the
  # following:
  # $ nix-env -f '<nixpkgs>' -iA screenshotToClipboardEnv
  packageOverrides = pkgs: rec {

    # This python environment is used for running the copy-image-to-clipboard
    # program, which is called by the screenshot-to-clipboard program.
    copy-image-to-clipboard-env = pkgs.python27.withPackages (ps: with ps; [
        pygtk
      ]);

    # This environment is used for running the screenshot-to-clipboard program.
    screenshot-to-clipboard-env = pkgs.buildEnv {
      name = "screenshot-to-clipboard-env";
      paths = [
        copy-image-to-clipboard-env
        pkgs.imagemagick
        pkgs.gimp
      ];
    };

    # This is my pretty-simple Haskell library with the CLI enabled to build.
    pretty-simple-cli =
      with pkgs.haskell.lib;
      addBuildDepend
        (appendConfigureFlag pkgs.haskellPackages.pretty-simple "-fbuildexe")
        pkgs.haskellPackages.optparse-applicative;
  };
}
