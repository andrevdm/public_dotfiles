# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./apps/tmux.nix
      ./graphical/fonts.nix
      ./services/docker.nix
      ./services/psql.nix
      ./services/ssh.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;

  nixpkgs.config.allowUnfree = true;

  networking.hostName = "andrevdm"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp9s0.useDHCP = true;
  networking.interfaces.wlp8s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # automatic gc
  nix.gc = {
    automatic = true;
    dates = "03:15";
    options = "--delete-older-than 30d";
  };


  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Africa/Johannesburg";

  environment.pathsToLink = [ "/libexec" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget


  environment.systemPackages = with pkgs; [
    acpi
    acpid
    bash
    bat
    binutils
    cabal2nix
    cacert
    cachix
    chromium
    curl 
    docker
    dropbox
    elixir
    feh
    firefox
    flameshot
    gcc
    gimp
    gitFull
    gnome3.gnome-disk-utility
    gnome3.gnome-keyring
    gnome3.gnome-screensaver
    gnome3.nautilus
    gnumake
    gnupg
    haskellPackages.cabal-install
    htop
    kdiff3
    keybase
    killall
    libreoffice
    lm_sensors
    lsd
    man_db
    mitmproxy
    neovim
    nodejs
    oh-my-zsh
    pass
    pcmanfm
    perl
    pgcli
    postgresql
    pulseaudio
    python
    python3
    ranger
    ripgrep
    slack
    spideroak
    stack
    sysstat
    termite
    tmux
    tree
    vim
    vlc
    watchman
    wget 
    which
    wireshark-qt
    xautolock
    xscreensaver
    zlib
    zlib.dev
    zsh 
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.dconf.enable = true;
  services.dbus.packages = [ pkgs.gnome3.dconf ];

  programs.zsh.enable = true;

  services.nixosManual.showManual = false;

  # Disable the infamous systemd screen/tmux killer
  services.logind = {
    lidSwitch = "suspend";
    lidSwitchDocked = "ignore";
    lidSwitchExternalPower = "ignore";
    extraConfig = ''
      HandlePowerKey=suspend
      KillUserProcesses=no
    '';
  };  

  # Increase the amount of inotify watchers
  # Note that inotify watches consume 1kB on 64-bit machines.
  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches"   = 1048576;   # default:  8192
    "fs.inotify.max_user_instances" =    1024;   # default:   128
    "fs.inotify.max_queued_events"  =   32768;   # default: 16384
  };  

  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };  

  # List services that you want to enable:

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  networking.networkmanager.enable = true;
 
  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.acpid.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  services.xserver.desktopManager = {
    default = "none";
    xterm.enable = false;
  };

  services.xserver.windowManager.i3 = {
    enable = true;
    extraPackages = with pkgs; [
      rofi
      i3status
      i3lock
      i3blocks
    ];
  };

  security.pam.services.gdm.enableGnomeKeyring = true;
  services.gnome3.gnome-keyring.enable = true;


  #services.xserver.videoDrivers = [ "nvidia" ];

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.andre = {
    isNormalUser = true;
    home = "/home/andre";
    description = "Andre Van Der Merwe";
    extraGroups = [  "wheel" "networkmanager" "vboxsf" "docker" ]; # Enable ‘sudo’ for the user.
    shell = "/run/current-system/sw/bin/zsh";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

}

