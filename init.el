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

;; Mandatory packages

(require 'use-package)
(require 'all-the-icons)

(require 'smartparens-config) ;; Do I need this?

;; With this Emacs starts automatically on fullscreen mode

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; This clears the message that shows at the scratch buffer by default

(setq initial-scratch-message "")

;; This way I can just type 'y' instead of 'yes<RET>' to confirm

(fset 'yes-or-no-p 'y-or-n-p)

;; Checks if Tamzen is on the system and uses it as a global font

(when (member "Tamzen" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Tamzen-15"))
  (set-face-attribute 'default t :font "Tamzen-15"))

;; Something to do with backups files lmao

(setq make-backup-files nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Settings just for macOS

(setq mac-option-key-is-meta t) ;; This way I can use Alt (option) key as Meta on Mac
(setq mac-right-option-modifier nil) ;; With this I can use the right Alt key as normally (rather than as Meta)

;; This hides the much-dreaded bars on the top of Emacs GUI

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; This prevents the cursor from blinking

(blink-cursor-mode 0)

;; This sets the Mode Line to display not just the line number
;; but also the column number (it's not that useful but ¯\_(ツ)_/¯)

(column-number-mode t)

;;

;;(define-key key-translation-map (kbd ",") ", ")

;;(global-unset-key (kbd ","))

;; Global keybindings

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;;;;;; use-package declarations ;;;;;;

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
  )

;; (use-package darkokai-theme
;;   :ensure t
;;   :config (load-theme 'darkokai t)
;;   )

;; (use-package doom-themes
;;   :ensure t
;;   :config (load-theme 'doom-one t)
;;   (doom-themes-neotree-config)
;;   (doom-themes-org-config)
;;   )

(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox-dark-soft t)
  )

;;(use-package rebecca-theme
;;  :ensure t
;;  :config (load-theme 'rebecca t)
;;  )

(use-package all-the-icons
  :ensure t
  )

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package rainbow-mode
  :ensure t
  :mode "\\.css\\'"
  )

(use-package pretty-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'pretty-mode)
  )

(use-package neotree
  :ensure t
  :init
  (setq neo-theme 'icons)
  (neotree)
  :bind ("M-n t" . neotree-toggle)
  )

(use-package ido
  :ensure
  :init
  (progn
    (ido-mode 1)
    (use-package ido-vertical-mode
      :ensure t
      :init (ido-vertical-mode 1)
      )
    (use-package flx-ido
      :ensure t
      :init (flx-ido-mode 1)
      )
    (use-package ido-completing-read+
      :ensure t
      )
    (use-package smex
      :ensure t
      :init (smex-initialize)
      :bind ("M-x" . smex)
      )
    )
  )

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
  (add-hook 'prog-mode-hook #'smartparens-mode)
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
  :config (ac-config-default)
  )

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package tex
  :ensure auctex
  :config
   (setq TeX-auto-save t)
   (setq TeX-parse-self t)
   (setq-default TeX-master nil)
   (setq TeX-save-query nil)
   (setq TeX-PDF-mode t)
   (add-hook 'LaTeX-mode-hook 'visual-line-mode)
   (add-hook 'LaTeX-mode-hook 'flyspell-mode)
   (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
   (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
   (setq reftex-plug-into-AUCTeX t)
)
  
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  )

(use-package elpy
  :ensure t
  :mode "\\.py\\'"
  :config
  (elpy-enable)
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

(use-package multiple-cursors
  :ensure t
  :bind ("C-S-c C-S-c" . mc/edit-lines)
  )

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-s a" . avy-goto-char-timer)
	 ("M-g M-g" . avy-goto-line))
  )

(use-package hlinum
  :ensure t
  :init
  (global-hl-line-mode 1)
  (global-linum-mode 1)
  (hlinum-activate)
  )

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  )

(use-package spaceline
  :ensure t)

(use-package spaceline-config
  :init
  (spaceline-all-the-icons-theme)
  (setq powerline-default-separator 'wave)
  )

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config (spaceline-all-the-icons-theme)
  )

(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-src-fontify-natively t
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t)
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;;;;;; Custom functions ;;;;;;

(defun cheatsheet()
  "Opens a custom cheatsheet located in .emacs.d/cheatsheet"
  (interactive)
  (find-file-read-only "~/.emacs.d/cheatsheet.org")
  )

(global-set-key (kbd "C-c C-s") 'cheatsheet)

;; These two functions were made by @Ironjanowar

(defun kill-buffers()
  (let (buffer buffers)
    (setq buffers (buffer-list))
    (dotimes (i (length buffers))
      (setq buffer (pop buffers))
      (if (not (string-equal (buffer-name buffer) "*scratch*")) (kill-buffer buffer) nil))))

(defun clean-buffers()
       (interactive)
       (if (yes-or-no-p "Do you really want to clean all buffers? ")
           (kill-buffers) nil))

(global-set-key (kbd "C-x C-S-k") 'clean-buffers)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a19265ef7ecc16ac4579abb1635fd4e3e1185dcacbc01b7a43cf7ad107c27ced" "3e160974b9e3e1b53270d1fb5bbaf56f0c689017e177972f72584bf096efc4cc" "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "3cb2d5a795e1c93d1fbc8360d6ea41f0173aa1366d334b16e1b83b996b8d9ce6" "73bff6f2ef60f8a1238a9ca666235d258e3acdeeed85d092ca532788dd7a33c4" "12b204c8fcce23885ce58e1031a137c5a14461c6c7e1db81998222f8908006af" "5f4e4c9f5de8156f964fdf8a1b8f8f659efbfeff88b38f49ce13953a84272b77" "a866134130e4393c0cad0b4f1a5b0dd580584d9cf921617eee3fd54b6f09ac37" "7666b079fc1493b74c1f0c5e6857f3cf0389696f2d9b8791c892c696ab4a9b64" "53d1bb57dadafbdebb5fbd1a57c2d53d2b4db617f3e0e05849e78a4f78df3a1b" "4e21fb654406f11ab2a628c47c1cbe53bab645d32f2c807ee2295436f09103c6" "2af26301bded15f5f9111d3a161b6bfb3f4b93ec34ffa95e42815396da9cb560" "75c5c39809c52d48cb9dcbf1694bf2d27d5f6fd053777c194e0b69d8e49031c0" default)))
 '(package-selected-packages
   (quote
    (firecode-theme moe-theme elpygen darkokai-theme use-package toxi-theme spaceline-all-the-icons smex smartparens rebecca-theme rainbow-mode rainbow-delimiters pretty-mode org-bullets neotree multiple-cursors multi-term monokai-theme magit jedi jdee ido-vertical-mode ido-ubiquitous hlinum helm haskell-mode gruvbox-theme flx-ido flatland-theme expand-region exec-path-from-shell esup ensime elpy doom-themes company-jedi cheatsheet base16-theme avy auctex)))
 '(spaceline-all-the-icons-separator-type (quote slant))
 '(spaceline-all-the-icons-separators-invert-direction t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
