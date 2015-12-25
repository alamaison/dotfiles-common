(defvar *emacs-load-start* (float-time))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package diminish
             :ensure t)

(use-package session
             :ensure t
             :init
             (setq session-globals-max-size 150)
             (add-hook 'after-init-hook 'session-initialize))

(use-package comment-dwim-2
             :ensure t
             :bind ("M-;" . comment-dwim-2))

(use-package uniquify ;; Change buffer naming to filename<directory>
             :init
	     (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package midnight ;; Clean up stale buffers automatically
             :init
	     (setq clean-buffer-list-delay-general 2))

(use-package zenburn-theme
             :ensure t
             :init (load-theme 'zenburn t))

(use-package powerline
             :ensure t
             :init
             (powerline-default-theme)
             (setq powerline-display-mule-info nil)
             (setq powerline-display-hud nil)
             (setq powerline-display-buffer-size nil))

(use-package projectile
             :ensure t
             :config
	     (setq projectile-mode-line
		   '(:eval (list " ["
				 (propertize (projectile-project-name)
					     'face '(:foreground "#81a2be"))
				 "]")))
	     (projectile-global-mode))

(use-package magit
             :ensure t
             :bind ("C-x g" . magit-status)
             :init (setq magit-last-seen-setup-instructions "1.4.0")
             :config
             (use-package magit-gitflow
                          :config (add-hook
                                   'magit-mode-hook
                                   'turn-on-magit-gitflow)))
(use-package company
             :ensure t
	     :diminish "cmp"
             :init (setq
                    company-idle-delay 0.1
                    company-minimum-prefix-length 1
                    company-tooltip-limit 20)
             :config (add-hook 'after-init-hook 'global-company-mode))

(use-package ido
             :config
             (setq ido-enable-flex-matching t)
             (setq ido-everywhere t)
             (ido-mode t))

(use-package cmake-mode
             :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package flycheck
             :config
             (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package jedi
             :config
             (use-package company-jedi
                          :config
                          (add-hook 'python-mode-hook
                                    (lambda () (add-to-list 'company-backends
                                                            'company-jedi))))
             (setq jedi:use-shortcuts t)
             (add-hook 'python-mode-hook 'jedi:setup))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (setq-default indent-tabs-mode nil)
  (add-hook 'js2-mode-hook 'my-js2-mode)
  (defun my-js2-mode ()
    (set-variable 'indent-tabs-mode nil)
    (setq-default js2-basic-offset 2))
)

(use-package scss-mode
  :mode (("\\.scss$" . scss-mode))
  :config (setq scss-compile-at-save nil))

;;{{{ Fonts and Colours

(global-font-lock-mode t)                ; turn on syntax highlighting
(setq font-lock-maximum-decoration t)

(font-lock-add-keywords nil ; highlight XXX style code tags in source files
                          '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\)" 1
                             font-lock-warning-face prepend)))

;; Font selection graceful fallback: http://emacswiki.org/emacs/SetFonts
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (require 'cl)
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))
(set-face-attribute 'default
                    nil
                    :font (font-candidate "Consolas-10:weight=normal"
                                          "DejaVu Sans Mono-10:weight=normal"))

;;}}}

;;{{{ Miscellaneous

(global-set-key (kbd "C-c C-c") 'recompile)
(setq-default fill-column 80)
(prefer-coding-system 'utf-8)
(setq inhibit-startup-message t)         ; turn off splash screen
(tool-bar-mode -1)                       ; turn off tool bar
(scroll-bar-mode -1)                     ; turn off scroll bars
(show-paren-mode t)                      ; turn on paranthesis highlighting
(setq case-fold-search t)                ; make search ignore case
(setq compilation-skip-threshold 2)      ; skip warning on compilation next
(setq company-dabbrev-downcase nil)      ; preserve case in completions
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(eval-after-load 'compilation-mode
  '(progn (defun my-compilation-mode-hook ()      ; wrapping in compilation window
      (setq truncate-lines nil)
      (setq truncate-partial-width-windows nil))
          (add-hook 'compilation-mode-hook 'my-compilation-mode-hook)))

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-extra-args '("-d en_GB")))
 )

;;}}}

;;{{{ Backups

(setq backup-directory-alist `(("." . "~/.backups_emacs")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;;}}}

;;{{{ Flymake

; Show error under cursor in minibuffer
(eval-after-load 'flymake '(require 'flymake-cursor))

; Show error markers in fringe
(eval-after-load 'flymake '(require 'rfringe))

;;}}}

(message "My .emacs loaded in %.2fs" (- (float-time) *emacs-load-start*))
