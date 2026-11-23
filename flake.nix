{
  description = "xmobar in Bend: xmobar 0.50's templates, config, monitors and bar, with laws and differential tests against xmobar itself";

  # bend-frp's pinned Bend (2.0.39), with BendTT's kernel built in, so
  # `bend PROOF.bend --verdict` works; the same Bend xmonad-bend builds with
  inputs.bend-frp.url = "github:hhefesto/bend-frp";
  inputs.nixpkgs.follows = "bend-frp/nixpkgs";
  # xmobar's own source: the reference the differential tests ask
  inputs.xmobar = {
    url = "https://hackage.haskell.org/package/xmobar-0.50/xmobar-0.50.tar.gz";
    flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      bend-frp,
      xmobar,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      each = f: nixpkgs.lib.genAttrs systems (system: f system nixpkgs.legacyPackages.${system});
      lib = nixpkgs.lib;
      # What a build sees: the Bend modules, the C side and the tests
      src = lib.fileset.toSource {
        root = ./.;
        fileset = lib.fileset.unions (
          [ (lib.fileset.fileFilter (f: f.hasExt "bend") ./.) ]
          ++ lib.optional (builtins.pathExists ./x11.c) ./x11.c
          ++ lib.optional (builtins.pathExists ./sys.c) ./sys.c
          ++ [ ./tests ]
        );
      };
      bendFor = pkgs: bend-frp.packages.${pkgs.stdenv.hostPlatform.system}.bend;
      # xmobar 0.50 as nixpkgs builds it (the same flags), with every module
      # of its library exposed, so the reference programs can call the
      # internal ones (Xmobar.Config.Template's parseString, the monitors'
      # parsers) as xmobar itself does
      xmobarLibFor =
        pkgs:
        pkgs.haskell.lib.overrideCabal pkgs.haskellPackages.xmobar (d: {
          src = xmobar;
          postPatch = (d.postPatch or "") + ''
            ${pkgs.perl}/bin/perl -0pi -e '
              my ($a, $b) = split /\nexecutable xmobar\n/, $_, 2;
              $a =~ s/(Xmobar\.Plugins\.Monitors\.Cpu)\s*\n\s*other-modules:\s*Paths_xmobar,/$1,\n                   Paths_xmobar,/;
              $a =~ s/other-modules:/exposed-modules:/g;
              $_ = $a . "\nexecutable xmobar\n" . $b;
            ' xmobar.cabal
          '';
          doCheck = false;
          doHaddock = false;
        });
      # GHC with that xmobar and QuickCheck, for the generators
      ghcFor =
        pkgs:
        pkgs.haskellPackages.ghcWithPackages (p: [
          (xmobarLibFor pkgs)
          p.QuickCheck
          p.temporary
          p.X11
          p.parsec
          p.mtl
          p.stm
          p.async
        ]);
      # x11.c's libraries: X11 with Xrender, Xrandr, Xinerama and Xpm, Cairo,
      # Pango
      xLibs =
        pkgs: with pkgs; [
          libx11
          libxrender
          libxrandr
          libxinerama
          libxpm
          cairo
          pango
          glib
        ];
      xModules = "pangocairo cairo-xlib xrender xrandr xinerama xpm x11";
      # their flags, for the C compiler bend runs (its wrapper reads them
      # under the platform's suffix), from pkg-config
      xFlags =
        pkgs:
        let
          s = pkgs.stdenv.cc.suffixSalt;
        in
        ''
          export NIX_CFLAGS_COMPILE_${s}="''${NIX_CFLAGS_COMPILE_${s}:-} $(pkg-config --cflags ${xModules})"
          export NIX_LDFLAGS_${s}="''${NIX_LDFLAGS_${s}:-} $(pkg-config --libs ${xModules})"
        '';
      inSource =
        pkgs: name: inputs: script:
        pkgs.runCommand "xmobar-bend-${name}" {
          nativeBuildInputs = [
            (bendFor pkgs)
            pkgs.pkg-config
          ]
          ++ inputs;
          buildInputs = xLibs pkgs;
        } ''
          export HOME=$TMPDIR
          cp -r ${src} w
          chmod -R u+w w
          cd w
          ${script}
        '';
    in
    {
      packages = each (
        system: pkgs: {
          default = self.packages.${system}.xmobar-bend;
          # the bar: its X11 window, or text (-T at Plain)
          xmobar-bend = inSource pkgs "bar" [ ] ''
            ${xFlags pkgs}
            mkdir -p $out/bin
            bend Main.bend -o $out/bin/xmobar-bend
          '';
          ghc-xmobar = ghcFor pkgs;
          xmobar-lib = xmobarLibFor pkgs;
          bend = bendFor pkgs;
          # xmobar itself, as nixpkgs builds it: the end-to-end reference
          xmobar = pkgs.haskellPackages.xmobar;
          # what the differential tests run the references in: bwrap puts
          # a case's files in /proc and /sys, faketime stops the reference's
          # clock (so a rate is the counters' difference, as at scx' = 1)
          bubblewrap = pkgs.bubblewrap;
          libfaketime = pkgs.libfaketime;
        }
      );

      devShells = each (
        system: pkgs: {
          # what building Main.bend by hand needs (x11.c's flags), and what
          # tests/x-diff.sh runs: xmobar itself, Xvfb and the screen's tools,
          # the DejaVu fonts
          x = pkgs.mkShell {
            packages = [
              (bendFor pkgs)
              pkgs.pkg-config
              (ghcFor pkgs)
              pkgs.haskellPackages.xmobar
              pkgs.xvfb
              pkgs.xwd
              pkgs.xwininfo
              pkgs.xprop
              pkgs.xdotool
              pkgs.imagemagick
              pkgs.fontconfig
              pkgs.procps
              pkgs.zsh
            ];
            buildInputs = xLibs pkgs;
            shellHook = xFlags pkgs + ''
              export FONTS=${pkgs.dejavu_fonts}/share/fonts
            '';
          };
          default = pkgs.mkShell {
            packages = [
              (bendFor pkgs)
              (ghcFor pkgs)
              pkgs.haskellPackages.xmobar
              pkgs.zsh
            ];
            shellHook = ''
              export XMOBAR=${xmobar}
            '';
          };
        }
      );
    };
}
