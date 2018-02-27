;;; package -- Summary:

;;; Commentary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(elpy-enable)
(require 'smartparens-config)
(show-smartparens-global-mode t)

(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(add-hook 'haskell-mode-hook #'pretty-mode)

(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; This hides the much-dreaded bars on the top of Emacs GUI

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; use-package declarations

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
  )

(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox-dark-soft t)
  )

;; (use-package rebecca-theme
;;   :ensure t
;;   :config (load-theme 'rebecca t))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :config (setq neo-theme 'icons)
  )

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
  )

(use-package ensime
  :ensure t
  :pin melpa-stable
  :config
  (setq scala-indent:align-forms t)
  (setq scala-indent:align-parameters t)
  (setq scala-indent:indent-value-expression t)
  )

(use-package auto-complete
  :ensure t
  :config (ac-config-default))

(use-package company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  )

(use-package multi-term
  :ensure t
  :config (setq multi-term-program "/bin/zsh")
  :bind ("M-s t" . multi-term)
  )

(use-package windmove
  :ensure t
  :bind (("C-c <up>" . windmove-up)
         ("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <down>" . windmove-down))
  )

;; misc keybindings

(global-set-key (kbd "M-n t") 'neotree-toggle) ;; Toggles/hides Neotree
(global-set-key (kbd "C-c C-s") 'cheatsheet) ;; Loads my custom cheatsheet located in ./.emacs.d/cheatsheet
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines) ;; Spawn multiple-cursors on selected region

;; avy keybindings

(global-set-key (kbd "C-:") 'avy-goto-char) ;; Go to with Avy (input: a single char)
(global-set-key (kbd "C-'") 'avy-goto-char-2) ;; Go to with Avy (input: two chars)
(global-set-key (kbd "M-s a") 'avy-goto-char-timer) ;; Go to with Avy (input: an arbitrary number of chars, waits for 0.5s)
(global-set-key (kbd "M-g M-g") 'avy-goto-line) ;; Go to line (input: zero chars/line number)
;; custom functions

(defun cheatsheet()
  "Opens cheathseet"
  (interactive)
  (find-file-read-only "~/.emacs.d/cheatsheet")
  )

(require 'multiple-cursors)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("0055e55e6a357a941027139152a67f93376616d3501055d06852f10fdc16bac0" "73bff6f2ef60f8a1238a9ca666235d258e3acdeeed85d092ca532788dd7a33c4" "75c5c39809c52d48cb9dcbf1694bf2d27d5f6fd053777c194e0b69d8e49031c0" "3cb2d5a795e1c93d1fbc8360d6ea41f0173aa1366d334b16e1b83b996b8d9ce6" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "f64c9f8b4241b680b186f4620afb9c82fa2a76cf4498a7431f90db59bb1892eb" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "9c4acf7b5801f25501f0db26ac3eee3dc263ed51afd01f9dcfda706a15234733" "6271fc9740379f8e2722f1510d481c1df1fcc43e48fa6641a5c19e954c21cc8f" "fec45178b55ad0258c5f68f61c9c8fd1a47d73b08fb7a51c15558d42c376083d" "5b8eccff13d79fc9b26c544ee20e1b0c499587d6c4bfc38cabe34beaf2c2fc77" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "ffe80c88e3129b2cddadaaf78263a7f896d833a77c96349052ad5b7753c0c5a5" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "87d46d0ad89557c616d04bef34afd191234992c4eb955ff3c60c6aa3afc2e5cc" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "722e1cd0dad601ec6567c32520126e42a8031cd72e05d2221ff511b58545b108" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default)))
 '(fci-rule-color "#3C3D37")
 '(global-linum-mode t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(ido-mode nil nil (ido))
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(package-archives
   (quote
    (("melpa" . "http://melpa.milkbox.net/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("org" . "http://orgmode.org/elpa/")
     ("melpa-stable" . "http://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (exec-path-from-shell gruvbox-theme powerline pretty-mode flycheck company-jedi multi-term cheatsheet haskell-mode smartparens avy multiple-cursors rebecca-theme jedi rainbow-mode use-package all-the-icons neotree elpy company auto-complete rainbow-delimiters flatland-theme base16-theme monokai-theme toxi-theme)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
