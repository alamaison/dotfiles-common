(defvar *emacs-load-start* (float-time))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;{{{ Emacs Server

(load "server")
(unless (server-running-p) (server-start))

;;;}}}


;;{{{ Fonts and Colours

(global-font-lock-mode t)                ; turn on syntax highlighting
(setq font-lock-maximum-decoration t)

(font-lock-add-keywords nil ; highlight XXX style code tags in source files
                        '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\)" 1
                           font-lock-warning-face prepend)))

;; Font selection graceful fallback: http://emacswiki.org/emacs/SetFonts
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))
(set-face-attribute 'default
                    nil
                    :font (font-candidate "MesloLGS Nerd Font Mono-9"
                                          "Consolas-10"
                                          "Ubuntu Mono"
                                          "Droid Sans Mono-8"
                                          "DejaVu Sans Mono-10"))

;; Colour support in compilation mode
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;;}}}


;;{{{ Basic Emacs settings


(desktop-save-mode 1)                    ; maintain sessions
(delete-selection-mode 1)                ; overwrite text in region
(column-number-mode 1)                   ; show column number as well as line number
(setq-default fill-column 79)            ; wrap at Python default
(prefer-coding-system 'utf-8)
(setq inhibit-startup-message t)         ; turn off splash screen
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)                 ; turn off tool bar
      (scroll-bar-mode -1)))             ; turn off scroll bars
(show-paren-mode t)                      ; turn on paranthesis highlighting
(setq case-fold-search t)                ; make search ignore case
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; dont pop up ediff command window
(add-to-list                             ; Make log files auto-tail
 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))
(add-hook                                ; strip trailing whitespace on save
 'before-save-hook 'delete-trailing-whitespace)
(setq-default indicate-empty-lines t)    ; show end-of-file in fringe
(setq scroll-preserve-screen-position 'always) ; Restore point when scrolling back
(setq vc-follow-symlinks t)              ; Don't prompt to follow symlinks
(setq split-height-threshold nil)        ; Prefer horizontal split
(setq warning-minimum-level :error)      ; Calm the warnings popup

;; Backups
(setq backup-directory-alist `(("." . "~/.backups_emacs")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(global-set-key (kbd "C-j") 'newline)    ; Give C-J same behaviour as RET > 24.1
(global-set-key (kbd "M-s r") 'rgrep)

;;}}}

;;{{{ Package system

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Bootstrap Quelpa
(use-package quelpa
  :ensure)
(use-package quelpa-use-package
  :ensure
  :demand
  :config
  (quelpa-use-package-activate-advice))

(require 'use-package-ensure)

;;}}}

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
(use-package diminish
  :ensure t)
(use-package delight
  :ensure t)

(use-package emacs
  :diminish eldoc-mode)

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package uniquify ;; Change buffer naming to filename<directory>
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package midnight ;; Clean up stale buffers automatically
  )

;; I would disable abbrev if I knew how, but I don't know what's starting it so
;; just diminish it instead
(use-package abbrev
  :diminish abbrev-mode)

(use-package ace-jump-mode
  :ensure
  :bind ("C-c SPC" . ace-jump-mode))

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
     `(flycheck-warning ((((supports :underline (:style wave)))
                          :underline (:style wave :color "DarkGrey"))
                         (t
                          :underline t :inherit warning)))
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

;; (use-package powerline
;;   :ensure t
;;   :init
;;   (powerline-default-theme)
;;   (setq powerline-display-mule-info nil)
;;   (setq powerline-display-hud nil)
;;   (setq powerline-display-buffer-size nil))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-project-name t)
  (setq doom-modeline-check-simple-format t)

  ;; Function to check if the Nerd fonts are actually installed before letting Doom Modeline try and use them
  (defun my/nerd-icons-available-p ()
    "Check if a Nerd Font is available in `font-family-list`."
    (member "Symbols Nerd Font Mono" (font-family-list)))

  (setq doom-modeline-icon (not (seq-empty-p (my/nerd-icons-available-p)))))

(use-package nerd-icons
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-mode-line-prefix " Prj")
  (projectile-mode +1)
  ; Don't show Projectile in modeline unless in a project
  :delight '(:eval (if (condition-case nil (and projectile-require-project-root
                                                (projectile-project-root))
                         (error nil))
                       (projectile-default-mode-line) ""))
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
(use-package forge
  :ensure t
  :after magit)

(use-package company
  :ensure t
  :diminish company-mode
  :init (setq
	 company-dabbrev-downcase nil      ; preserve case in completions
	 company-idle-delay 0.1
	 company-minimum-prefix-length 1
	 company-tooltip-limit 20)
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  ;; (setq ido-everywhere t) ; Disabled because this conflicts with Helm.  See https://github.com/emacs-helm/helm/issues/1819
  (setq ido-auto-merge-work-directories-length -1) ; Disable annoying auto-match path
  (ido-mode t)
  (use-package flx-ido  ; better matching
    :ensure t
    :config (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil)))

(use-package helm
  :ensure t
  :config
  (setq helm-allow-mouse nil) ; needed because of bug ing helm-git-grep: https://github.com/yasuyk/helm-git-grep/issues/52
  (setq helm-split-window-inside-p t)
  (setq helm-use-frame-when-more-than-two-windows nil)
  (setq helm-move-to-line-cycle-in-source nil)
  (helm-autoresize-mode 1))

(use-package helm-mode
  :diminish helm-mode
  :config (helm-mode 1))

(use-package helm-command
  :bind (("M-x" . helm-M-x)))

(use-package helm-files
  :bind (("C-x C-f" . helm-find-files))
  :config (setq helm-ff-file-name-history-use-recentf t))

(use-package helm-buffers
  :bind (("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini))
  :config (setq helm-buffer-max-length nil))

(use-package helm-occur
  :bind (("M-s o" . helm-occur)))

(use-package helm-projectile
  :ensure
  :config (helm-projectile-on))

(use-package helm-git-grep
  :ensure
  :bind (("M-s g" . helm-git-grep-at-point)))

(use-package wgrep-helm :ensure)


(use-package cmake-mode
  :ensure
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-project
  :ensure
  :config
  (defun maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
  (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
  (add-hook 'c++-mode-hook 'maybe-cmake-project-hook))

(use-package clang-format
  :ensure
  :config
  (defun local-add-format-before-save ()
    ;; The fourth param makes this buffer-local
    (add-hook 'before-save-hook 'clang-format-buffer nil t))
   (add-hook 'c++-mode-hook 'local-add-format-before-save)
   (add-hook 'c-mode-hook 'local-add-format-before-save)
   (add-hook 'objc-mode-hook 'local-add-format-before-save))

(use-package irony
  :ensure
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
    :ensure
    :config
    (use-package company-irony-c-headers
      :ensure
      :config
      (eval-after-load 'company
        '(add-to-list 'company-backends '(company-irony-c-headers company-irony))))
    ;; completion at interesting places, such as after scope operator
    ;; std::|
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))
  (use-package flycheck-irony
    :ensure
    :config
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

(use-package flycheck
  :ensure
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint))))

(defun recompile-dwim ()
  (interactive)
  (if (condition-case nil (and projectile-require-project-root
                               (projectile-project-root))
        (error nil))
      (projectile-compile-project nil) (recompile)))

(use-package lsp-mode
  :ensure t
  :diminish lsp-lens-mode
  :hook (python-mode . lsp-deferred)
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration))))
  :commands (lsp lsp-deferred)
  :bind-keymap ("C-c l" . lsp-command-map)
  :config
  (setq lsp-pylsp-plugins-yapf-enabled t)
  (setq lsp-pylsp-plugins-rope-autoimport-enabled t)
  (setq lsp-pylsp-plugins-isort-enabled t)
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-show-code-actions t))
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package jedi-core
  :disabled
  :ensure
  :config
  (setq jedi:use-shortcuts t) ; M-. and M-,
  :hook (python-mode . jedi:setup))

(use-package company-jedi
  :disabled
  :after (company jedi-core)
  :ensure
  :hook (python-mode . (lambda () (add-to-list 'company-backends
                                               'company-jedi))))

(use-package anaconda-mode
  :disabled
  :ensure
  :hook ((python-mode)
         (python-mode . anaconda-eldoc-mode))
  :bind (:map anaconda-mode-map
              ("M-," . anaconda-mode-go-back)))

(use-package company-anaconda
  :after (company anaconda-mode)
  :ensure
  :hook (python-mode . (lambda () (add-to-list 'company-backends
                                               'company-anaconda))))

(use-package pytest :ensure)

(use-package auto-virtualenv
  :ensure t
  :config
  (setq auto-virtualenv-verbose t)
  (auto-virtualenv-setup))

(use-package py-autopep8
  :ensure
  :config
  (setq py-autopep8-options '("--max-line-length=99"))
  (defun python-mode-keys ()
    "Modify python-mode local key map"
    (local-set-key (kbd "C-c C-p") 'py-autopep8-buffer))
  (add-hook 'python-mode-hook 'python-mode-keys))

(add-hook 'python-mode-hook 'subword-mode)
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'recompile-dwim)))
;; Please/Blaze files are Python-like
(add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-build-mode))

(use-package web-mode
  :ensure
  :mode (("\\.jsx?$" . web-mode))
  :config
  (defun my-web-mode-hook ()
    ;; short circuit js mode and just do everything in jsx-mode
    (if (equal web-mode-content-type "javascript")
        (web-mode-set-content-type "jsx"))
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2)
    (set-variable 'indent-tabs-mode nil)
    (tern-mode t))
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  (add-hook 'web-mode-hook 'subword-mode))

(use-package js2-mode
  :ensure
  :config
  (setq-default indent-tabs-mode nil)
  (defun my-js2-mode ()
    (set-variable 'indent-tabs-mode nil)
    (setq-default js2-basic-offset 2)
    (setq mode-name "js")
    (tern-mode t))
  (add-hook 'js2-mode-hook 'my-js2-mode)
  (add-hook 'js2-mode-hook 'subword-mode)
  (use-package js2-refactor
    :ensure
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-m")))

(use-package json-mode
  :ensure
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

(use-package scss-mode
  :mode (("\\.scss$" . scss-mode))
  :config (setq scss-compile-at-save nil))

(use-package go-mode
  :commands go-mode
  :hook ((go-mode . flycheck-mode)
         (go-mode . subword-mode)
         (before-save . gofmt-before-save))
  :init
  (setq gofmt-command "goimports")
  (setq go-tab-width 4))
  ;;   (bind-key [remap find-tag] #'godef-jump))

(use-package flycheck-gometalinter
  :after go-mode flycheck
  :ensure
  :hook (flycheck-mode . flycheck-gometalinter-setup))

(use-package go-eldoc
  :after go-mode
  :ensure t
  :hook (go-mode . go-eldoc-setup))

(use-package company-go
  :after go-mode
  :ensure t
  :hook (go-mode . (lambda () (add-to-list 'company-backends
                                           'company-go))))

(use-package restclient :ensure)

(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode)))

(use-package docker :ensure)

(use-package which-key
  :ensure
  :diminish which-key-mode
  :config (which-key-mode t))

(use-package expand-region
  :ensure
  :bind ("C-@" . er/expand-region))

(use-package diff-hl
  :ensure
  :config (global-diff-hl-mode))

(use-package diff-hl-dired
  :init
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package symon :ensure)

(defun my-c-mode-common-hook ()
  ;; force only spaces for indentation
  (setq indent-tabs-mode nil
        tab-width 4
        c-basic-offset tab-width)
  (local-set-key (kbd "C-c C-c") 'recompile-dwim))
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

(c-add-style "my-bsd"
             '("bsd"
               (c-offsets-alist
                (innamespace . -))))
(setq c-default-style
      '((java-mode . "java") (awk-mode . "awk") (other . "my-bsd")))

(use-package protobuf-mode
  :ensure
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 4) (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook 'subword-mode)
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))

;;{{{ Miscellaneous

(global-set-key (kbd "C-c C-c") 'recompile-dwim)

(use-package multi-term
  :ensure
  :config
  (use-package helm-mt
    :ensure
    :bind ("C-x t" . helm-mt)
    :config (helm-mt/reroute-terminal-functions t)))

(defun my-compilation-mode-hook ()
  ;; wrapping in compilation window
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)

  (setq compilation-skip-threshold 2)      ; skip warning on compilation next
  (setq compilation-auto-jump-to-first-error t) ; scroll to first error

  ;; Recognise Boost.Test failures
  (add-to-list 'compilation-error-regexp-alist 'boost)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(boost
                 "^\\(.*\\)(\\([0-9]+\\)): \\(?:\\(?:fatal \\)?error\\|warnin\\(g\\)\\) in \"[^\"]+\"" 1 2 nil (3)))

  ;; Recognise addtional MSVC messages - like built-in MS rule, but accepts any
  ;; word after see. Used to catch "see reference"
  (add-to-list 'compilation-error-regexp-alist 'msft-see)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(msft-see "^ *\\([0-9]+>\\)? *\\(\\(?:[a-zA-Z]:\\)?[^:(	\n]+\\)(\\([0-9]+\\)) ?: \\(?:see\\|could be\\|or\\|note\\) " 2 3 nil
       0)))
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-extra-args '("-d en_GB")))
 )

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

;;{{{ Erc

;; Loads password using auth-source (from .authinfo)

(use-package erc
  :commands erc
  :config
  (setq erc-server "irc.freenode.net"
        erc-port 6667
        erc-nick "a_lamaison"
        erc-prompt-for-password nil
        erc-prompt-for-nickserv-password nil
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE"))

  (use-package erc-join
    :config
    (erc-autojoin-mode t)
    (setq erc-join-buffer 'bury
          erc-autojoin-channels-alist
          (list `(".*\\.freenode.net" "#emacs" "#libssh2"))))

  (use-package erc-track
    :config
    (erc-track-mode t)
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))))

;;}}}

(use-package nvm
  :quelpa ((nvm :fetcher github
                :repo "rejeep/nvm.el")
                :upgrade t)
  :config
  (nvm-use "20"))

(use-package copilot
  :ensure
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package visual-regexp-steroids
  :ensure
  :config
  (use-package pcre2el :ensure)
  (define-key global-map (kbd "C-M-%") 'vr/query-replace)
  (define-key esc-map (kbd "C-M-r") 'vr/isearch-backward) ;; C-M-r
  (define-key esc-map (kbd "C-M-s") 'vr/isearch-forward) ;; C-M-s
  (setq vr/engine 'pcre2el))

(use-package envrc
  :ensure t
  :config (envrc-global-mode))

(message "My .emacs loaded in %.2fs" (- (float-time) *emacs-load-start*))
