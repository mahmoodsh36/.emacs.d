(use-modules (gnu) (nongnu packages linux))
(use-modules (gnu) (nongnu packages mozilla))
(use-modules (gnu) (gnu packages base))
(use-modules (guix packages))
(use-modules (guix git-download))
(use-modules (guix build-system gnu))
(use-modules (guix licenses))
(use-modules (guix utils))
(use-modules (gnu system setuid))
(use-service-modules networking desktop xorg sound)
(use-package-modules vim gnome version-control curl wm
                     emacs xorg xdisorg image-viewers terminals
                     gtk rsync cran rust-apps shells bittorrent
                     gnuzilla pulseaudio compton video fonts tmux
                     freedesktop fontutils web-browsers package-management
                     emacs-xyz ssh cmake pkg-config image music photo android
                     glib python-xyz python unicode admin certs linux rust
                     crates-io disk imagemagick file haskell-xyz)

(define this-file
  (local-file (basename (assoc-ref (current-source-location) 'filename))
              "config.scm"))

(define-public my-sxiv
  (package
    (name "sxiv")
    (version "26")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mahmoodsheikh36/sxiv")
                    (commit "e10d3683bf9b26f514763408c86004a6593a2b66")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "161l59balzh3q8vlya1rs8g97s5s8mwc5lfspxcb2sv83d35410a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags
       (list (string-append "PREFIX=" %output)
             (string-append "CC=" ,(cc-for-target))
             ;; Xft.h #includes <ft2build.h> without ‘freetype2/’.  The Makefile
             ;; works around this by hard-coding /usr/include & $PREFIX.
             (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "freetype")
                            "/include/freetype2")
             "V=1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'install 'install-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "sxiv.desktop"
                           (string-append (assoc-ref outputs "out")
                                          "/share/applications"))
             #t))
         (add-after 'install 'install-icons
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "-C" "icon" "install" make-flags))))))
    (inputs
     `(("freetype" ,freetype)
       ("giflib" ,giflib)
       ("imlib2" ,imlib2)
       ("libexif" ,libexif)
       ("libx11" ,libx11)
       ("libxft" ,libxft)))
    (home-page "https://github.com/muennich/sxiv")
    (synopsis "Simple X Image Viewer")
    (description
     "sxiv is an alternative to feh and qiv.  Its primary goal is to
provide the most basic features required for fast image viewing.  It has
vi key bindings and works nicely with tiling window managers.  Its code
base should be kept small and clean to make it easy for you to dig into
it and customize it for your needs.")
    (license gpl2+)))

(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
EndSection
")

(operating-system
  (kernel linux)
  (locale "en_US.utf8")
  (host-name "mahmooz")
  (timezone "Asia/Jerusalem")

  (keyboard-layout (keyboard-layout "us" "altgr-intl"))

  ;; This is needed to create a bootable USB
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                ;;(timeout 1)
                (targets (list "/boot/efi"))))

  (firmware (append (list iwlwifi-firmware)
                    %base-firmware))

  (users (cons* (user-account
                  (name "mahmooz")
                  (group "users")
                  (supplementary-groups '("wheel" "audio" "adbusers"))
		  (shell (file-append zsh "/bin/zsh"))
                  (home-directory "/home/mahmooz"))
                %base-user-accounts))

  (packages (append (list
                     nss-certs ;; for https

                     ;; fonts
                     fontconfig
                     font-fantasque-sans
                     font-dejavu
                     font-google-noto

                     ;; media
                     mpv feh
                     my-sxiv

                     ;; X
                     libinput xf86-video-fbdev
                     xf86-video-nouveau xf86-video-ati xf86-video-vesa
                     sxhkd xinit
                     awesome
                     sxhkd setxkbmap
                     xorg-server
                     picom clipit
                     rofi 
                     xclip xset

                     ;; text editors
                     emacs
                     neovim

                     ;; commandline tools
                     curl git
                     zsh tmux
                     alacritty 
                     transmission
                     bat
                     clyrics
                     scrot
                     adb
                     ranger
                     vifm
                     imagemagick
                     file
                     ffmpeg

                     ;; other
                     libnotify
                     network-manager
                     nyxt
                     rsync
                     pulseaudio pulsemixer
                     qutebrowser
                     firefox
                     openssh
                     emacs-guix
                     cmake
                     gnu-make
                     dbus
                     playerctl
                     hostapd

                     ;; rust
                     rust
                     rust-cargo-0.53

                     ;; python
                     python
                     python-pip
                     )
                    %base-packages))

  (sudoers-file
   (plain-file
    "sudoers"
    "root ALL=(ALL) ALL
    %wheel ALL=(ALL) NOPASSWD: ALL"))

  (services (append (list (service network-manager-service-type)
                          (udev-rules-service 'android android-udev-rules
                                              #:groups '("adbusers"))
                          (service wpa-supplicant-service-type)
                          (service slim-service-type
                                   (slim-configuration
                                    (auto-login? #t)
                                    (default-user "mahmooz")
                                    (display ":0")
                                    (vt "vt2")
                                    (xorg-configuration (xorg-configuration (extra-config (list %xorg-libinput-config))))))
                          (service xorg-server-service-type)
                          (service hostapd-service-type
                                   (hostapd-configuration
                                     (interface "wlp0s20f3")
                                     (ssid "devilspawn")))
                          (service alsa-service-type (alsa-configuration
                                                       (pulseaudio? #t))))
                    %base-services))

  (file-systems (cons* (file-system
                        (device (file-system-label "guix"))
                        (mount-point "/")
                        (type "ext4"))
                       (file-system
                        (device "/dev/nvme0n1p1")
                        (type "vfat")
                        (mount-point "/boot/efi"))
                       %base-file-systems)))
