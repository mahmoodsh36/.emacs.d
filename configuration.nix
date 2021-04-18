# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ lib, config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # allow non-free packages to be installed
  nixpkgs.config.allowUnfree = true; 


  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "mahmooz";
  #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
  networking.enableIPv6 = false;
  #networking.nameservers = [ "8.8.4.4" "8.8.8.8" ];
  environment.etc = {
    "resolv.conf".text = ''
      nameserver 8.8.8.8
      nameserver 8.8.4.4
    '';
  };

  # Set your time zone.
  time.timeZone = "Asia/Jerusalem";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp7s0.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };
  #
  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      fantasque-sans-mono
    ];
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;

    displayManager.startx.enable = true;

    windowManager.awesome = {
      enable = true;
      luaModules = with pkgs.luaPackages; [
        luarocks
        luadbi-mysql
      ];
    };
  };
 
  #services.sxhkd = {
  #  enable = true;
  #  
  #  extraConfig = ''
  #    super + Return
  #        alacritty -e tmux
  #  '';
  #};


  # Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "caps:swapescape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # tells you how to install a non-existent executable
  programs.command-not-found.enable = true;

  # Enable sound and bluetooth
  sound.enable = true;
  services.blueman.enable = true;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.config = {
    General = {
      Enable = "Source,Sink,Media,Socket";
    };
  };
  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
    extraConfig = "
      load-module module-switch-on-connect
    ";
  };
  nixpkgs.config.pulseaudio = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # enable zsh
  programs.zsh.enable = true;

  # users
  users.users.mahmooz = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "audio" ];
    shell = pkgs.zsh;
  };
  security.sudo.configFile = ''
    mahmooz ALL=(ALL:ALL) NOPASSWD: ALL
  '';

  # packages
  environment.systemPackages = with pkgs; [
    # text editors
    vim
    emacs
    neovim

    # networking tools
    curl
    wpa_supplicant
    networkmanager
    # required for wireless AP setup
    hostapd dnsmasq bridge-utils

    # media tools
    mpv
    spotify
    sxiv
    imagemagick
    feh # i use it to set wallpaper

    # general tools
    brave
    awesome
    discord

    # commandline tools
    alacritty # terminal emulator
    zsh
    bat
    lsd
    tmux
    cmake
    git
    pulsemixer # tui for pulseaudio control
    playerctl # media control
    nix-index # helps finding the package that contains a specific file
    ranger # file manager
    gnumake
    gcc
    file
    youtube-dl

    # x11 tools
    xorg.xinit
    sxhkd
    rofi
    libnotify
    xclip

    # other
    vimPlugins.Vundle-vim

    # python
    (python38.withPackages(ps: with ps; [ numpy requests beautifulsoup4 ]))

    # libs
    imlib2 x11 libexif giflib # required to compile sxiv
    libtool # requried to build vterm on emacs
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
