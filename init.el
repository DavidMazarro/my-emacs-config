
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

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(setq make-backup-files nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; This hides the much-dreaded bars on the top of Emacs GUI

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; This prevents the cursor from blinking 

(blink-cursor-mode nil)

;; This sets the Mode Line to display not just the line number
;; but also the column number (it's not that useful but ¯\_(ツ)_/¯)

(column-number-mode t)

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

;;;;;; Custom functions ;;;;;;

(defun cheatsheet()
  "Opens a custom cheatsheet located in .emacs.d/cheatsheet"
  (interactive)
  (find-file-read-only "~/.emacs.d/cheatsheet")
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

(global-set-key (kbd "C-x C-k") 'clean-buffers)

 
