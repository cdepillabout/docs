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

    # Change arbtt so it doesn't record window titles.
    arbtt =
      pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.arbtt (oldAttrs: {
        postPatch = (oldAttrs.postPatch or "") + ''
          pattern="getWindowTitle dpy =  myFetchName dpy"
          replace='getWindowTitle _ _ = pure ""'
          file="src/Capture/X11.hs"
          if grep -q "$pattern" "$file"; then
            substituteInPlace "$file" --replace "$pattern" "$replace"
          else
            echo "pattern \`$pattern\` does not exist in $file, so not able to replace it with \`$replace\`"
            exit 1
          fi
        '';
      });
  };
}
