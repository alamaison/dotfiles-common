(defvar *emacs-load-start* (float-time))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

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

;; I would disable abbrev if I knew how, but I don't know what's starting it so
;; just diminish it instead
(use-package abbrev
  :diminish abbrev-mode)

(use-package ace-window ;; Better window switching with >2 windows
  :ensure t
  :bind ("C-x o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package zenburn-theme
  :ensure t
  :init (load-theme 'zenburn t)
  :config
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(mode-line
       ((,class (:foreground "black" :background "#f9b593" :box nil))
        (t :inverse-video t)))
     `(mode-line-inactive
       ((t (:foreground ,zenburn-green-1 :background ,zenburn-bg-05 :box nil))))
     `(mode-line-buffer-id ((t (:foreground "black" :weight bold))))
     `(powerline-active1
       ((t (:foreground ,zenburn-green-1 :background ,zenburn-bg-05
                        :inherit mode-line))))
     `(powerline-active2 ((t (:background ,zenburn-bg+2 :inherit mode-line)))))))

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
  (defun my-format-projectile-modeline ()
    (propertize (format " |%s|" (projectile-project-name))
                'face '(:foreground "black" :background "#81a2be")))
  (defun my-conditional-projectile-modeline ()
    (if (condition-case nil (and projectile-require-project-root
                                 (projectile-project-root))
          (error nil))
        (my-format-projectile-modeline) ""))
  (setq projectile-mode-line '(:eval (my-conditional-projectile-modeline)))
  (projectile-global-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (use-package magit-gitflow
    :diminish "GitF"
    :config (add-hook
	     'magit-mode-hook
	     'turn-on-magit-gitflow)))

(use-package company
  :ensure t
  :diminish company-mode
  :init (setq
	 company-dabbrev-downcase nil      ; preserve case in completions
	 company-idle-delay 0.1
	 company-minimum-prefix-length 1
	 company-tooltip-limit 20)
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode t)
  (use-package flx-ido  ; better matching
    :ensure t
    :config (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c g" . helm-grep-do-git-grep))
  :config
  (require 'helm-config)
  (helm-mode 1)
  (use-package helm-projectile
    :config (helm-projectile-on)))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-project
  :config
  (defun maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
  (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
  (add-hook 'c++-mode-hook 'maybe-cmake-project-hook))

(use-package irony
  :diminish "Iy"
  :config
  (setq w32-pipe-read-delay 0)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (use-package company-irony
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))
    ;; completion at interesting places, such as after scope operator
    ;; std::|
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

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

(use-package restclient)

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode t))

(use-package expand-region
  :bind ("C-@" . er/expand-region))

(use-package diff-hl
    :defer t
    :init
    (setq diff-hl-side 'right)
    (add-hook 'prog-mode-hook 'diff-hl-mode)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package symon)

(defun my-c-mode-common-hook ()
  ;; force only spaces for indentation
  (setq indent-tabs-mode nil
        tab-width 4
        c-basic-offset tab-width))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun maybe-libssh2-style ()
  (when (and buffer-file-name
             (string-match "libssh2" buffer-file-name))
    (load "libssh2-style")
    (libssh2-c-mode-common-hook)))
(add-hook 'c-mode-hook 'maybe-libssh2-style)

(defun my-emacs-lisp-mode-hook ()
  ;; force only spaces for indentation
  (setq indent-tabs-mode nil))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(defun my-python-mode-hook ()
  ;; force only spaces for indentation
  (setq indent-tabs-mode nil))
(add-hook 'python-mode-hook 'my-python-mode-hook)

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
(add-hook                                ; strip trailing whitespace on save
 'before-save-hook 'delete-trailing-whitespace)

(defun my-compilation-mode-hook ()
  ;; wrapping in compilation window
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)

  ;; Recognise Boost.Test failures
  (add-to-list 'compilation-error-regexp-alist 'boost)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(boost
                 "^\\(.*\\)(\\([0-9]+\\)): fatal error in" 1 2)))
(eval-after-load 'compilation-mode
  '(progn (add-hook 'compilation-mode-hook 'my-compilation-mode-hook)))

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


;;{{{ Windows-specific

(when (string-equal system-type "windows-nt")
  ;; Prevent issues with the Windows null device (NUL)
  ;; when using Unix find with rgrep.
  ;; (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  ;;   (let ((grep-use-null-device nil))
  ;;     ad-do-it))

  ;; (ad-activate 'grep-compute-defaults)

  ;; Use binaries from ezwinports
  (setenv "PATH"
          (concat (expand-file-name "~/.emacs.d/bin/win32/")
                  ";" (getenv "PATH")))
  )

;;}}}

(message "My .emacs loaded in %.2fs" (- (float-time) *emacs-load-start*))
