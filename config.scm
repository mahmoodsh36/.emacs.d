(use-modules (gnu) (nongnu packages linux))
(use-modules (gnu) (nongnu packages mozilla))
(use-service-modules networking desktop xorg sddm base sound)
(use-package-modules vim gnome version-control curl wm
		     emacs xorg xdisorg image-viewers terminals
		     gtk rsync cran rust-apps shells bittorrent
		     gnuzilla pulseaudio compton video fonts tmux
		     freedesktop fontutils web-browsers package-management
		     emacs-xyz ssh)

(define this-file
  (local-file (basename (assoc-ref (current-source-location) 'filename))
              "config.scm"))

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
                  (supplementary-groups '("wheel" "audio"))
		  (shell (file-append zsh "/bin/zsh"))
                  (home-directory "/home/mahmooz"))
                %base-user-accounts))

  (packages (append (list
		      (specification->package "nss-certs")

		      ;; fonts
		      fontconfig
		      font-fantasque-sans
		      font-dejavu

		      ;; media
		      mpv feh

		      ;; X
		      xf86-input-libinput xf86-video-fbdev
		      xf86-video-nouveau xf86-video-ati xf86-video-vesa
		      sxhkd xinit awesome
		      sxhkd setxkbmap
		      xorg-server
		      picom clipit
		      rofi 

		      ;; text editors
		      emacs
		      neovim

		      ;; commandline tools
		      curl git
		      zsh tmux
		      alacritty 
		      transmission
		      bat

		      ;; other
		      libnotify
		      network-manager
		      icecat
		      rsync r-lsd
		      pulseaudio pulsemixer
		      qutebrowser
		      firefox
		      flatpak
		      openssh
		      emacs-guix)
		    %base-packages))

  (services (append (list (service network-manager-service-type)
			  (service wpa-supplicant-service-type)
			  (service slim-service-type (slim-configuration
						       (auto-login? #t)
						       (default-user "mahmooz")
						       (display ":0")
						       (vt "vt2")))
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
