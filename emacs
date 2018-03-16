(require 'package)

;; (add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("elpy" .
;;              "http://jorgenschaefer.github.io/packages/"))

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(if (not (package-installed-p 'use-package))
    (package-install 'use-package))

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smex
  :ensure
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :config
  (progn
        (smex-initialize)))

(use-package flx-ido
  :ensure t
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)))

(use-package ido-vertical-mode
  :ensure t
  :config
  (progn
    (ido-vertical-mode 1)))

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status))
  :config
  (progn
    (setq magit-auto-revert-mode nil)
    (setq magit-last-seen-setup-instructions "1.4.0"))
)

(use-package color-theme
  :ensure
  :config
  (progn
    (color-theme-initialize)
    (color-theme-clarity)))

(use-package col-highlight
  :ensure t
  :config
  (progn
    (col-highlight-set-interval 1)
    (toggle-highlight-column-when-idle 1)
    (set-face-background 'col-highlight "color-235")))

;; brew install --HEAD ctags
;; brew install global --with-exuberant-ctags
(use-package ggtags
  :ensure
  :bind (("C-c C-g" . ggtags-mode)))

(use-package ace-jump-mode
  :ensure
  :bind (("C-j" . ace-jump-mode)))

(use-package ag
  :ensure)

(use-package thrift
  :ensure
  :config
  (setq thrift-indent-level 8))

(use-package textmate
             :ensure
             :bind (("M-t" . textmate-goto-file)
                    ("M-T" . textmate-goto-symbol))
             :config
             (textmate-mode))
(use-package rbenv
  :ensure)

(use-package yaml-mode
  :ensure
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))))

;; brew install homebrew/x11/xclip
(use-package xclip
  :ensure
  :config
  (progn
    (xclip-mode 1)))

(use-package pytest
  :ensure
  :bind (("C-c t" . pytest-module)))

(use-package go-mode
  :ensure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom auto-added faces and variables.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((t (:foreground "brightcyan")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("f0b0710b7e1260ead8f7808b3ee13c3bb38d45564e369cbe15fc6d312f0cd7a0" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(echo-keystrokes 0.1)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(go-test-verbose t)
 '(gofmt-command "goimports")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (go-rename gotest yaml-mode xclip use-package thrift textmate smex simpleclip rbenv pytest popup magit ido-vertical-mode go-mode ggtags flx-ido color-theme col-highlight ag ace-jump-mode)))
 '(safe-local-variable-values
   (quote
    ((eval setenv "UBER_ENVIRONMENT" "test")
     (eval setenv "UBER_CONFIG_DIR"
           (concat
            (getenv "GOPATH")
            "/src/code.uber.internal/pricing/wayfare/config"))
     (go-test-args . "-race")
     (go-test-args . -race)
     (eval setenv "PATH"
           (concat "env/bin:"
                   (getenv "PATH")))
     (eval setenv "PYTHONDONTWRITEBYTECODE" "1")
     (eval setenv "CLAY_CONFIG" "config/test.yaml"))))
 '(save-place-file "~/.emacs.d/emacs.places")
 '(save-place-mode t nil (saveplace))
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(visible-bell t))

;; disable annoyed ring bell
(setq ring-bell-function 'ignore)

;; increase font size
(set-face-attribute 'default nil :height 130)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; Go
;; (pushnew '(golang-test ("^\t([^:]+):([0-9]+):" 1 2))
;;          compilation-error-regexp-alist-alist)

(setenv "GOPATH" (concat (getenv "HOME") "/gocode"))
(setq exec-path (cons
                 (concat (getenv "HOME") "/gocode/bin")
                 exec-path))
(setenv "PATH" (concat (getenv "HOME") "/gocode/bin:" (getenv "PATH")))

(define-key go-mode-map (kbd "C-c f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-c t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-c p") 'go-test-current-project)
;; (define-key go-mode-map (kbd "C-x b") 'go-test-current-benchmark)
;; (define-key go-mode-map (kbd "C-x x") 'go-run)
(fset 'gopp
   "b, _ := json.MarshalIndent(, \"\", \"\\t\")\C-mfmt.Println(string(b))\C-p\C-[f\C-f")

;; ruby
(setenv "PATH" (concat
                (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")
                ))
(setq exec-path (cons
                 (concat (getenv "HOME") "/.rbenv/shims")
                 (cons (concat (getenv "HOME") "/.rbenv/bin")
                       exec-path)))


;; Javascript
(setq js-indent-level 4)

;; css
(setq css-indent-offset 2)

;; remote

;; (let ((default-directory "/ssh:<host>:/<dir>"))
;;   (start-file-process "grep" (get-buffer-create "*grep*")
;;                       "/bin/sh" "-c" "grep -e tramp *"))


;; global
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (cons
                 "/usr/local/bin"
                 exec-path))
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(setq objc-mode-hook
      (function (lambda ()
                  (setq indent-tabs-mode nil)
                  (setq c-indent-level 4))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'gofmt-before-save)

;; (add-hook 'go-mode-hook (lambda() (setq go-test-args "-race")))
;; (remove-hook 'go-mode-hook (lambda() (setq go-test-args "-race")))

(desktop-save-mode 1)

(global-auto-revert-mode t)

;; Ansi-term
;; (require 'term)
;; (defun visit-ansi-term ()
;;       "If the current buffer is:
;;      1) a running ansi-term named *ansi-term*, rename it,
;;         and create a new *ansi-term*
;;      2) a stopped ansi-term, kill it and create a new one.
;;      3) a non ansi-term, go to an already running ansi-term
;;         or start a new one while killing a defunt one"
;;       (interactive)
;;       (let ((is-term (string= "term-mode" major-mode))
;;             (is-running (term-check-proc (buffer-name)))
;;             (term-cmd "/bin/bash")
;;             (anon-term (get-buffer "*ansi-term*")))
;;         (if is-term
;;             (if is-running
;;                 (if (string= "*ansi-term*" (buffer-name))
;;                     (progn (call-interactively 'rename-buffer)
;;                            (ansi-term term-cmd))
;;                   (if anon-term
;;                       (switch-to-buffer "*ansi-term*")
;;                     (ansi-term term-cmd)))
;;               (kill-buffer (buffer-name))
;;               (ansi-term term-cmd))
;;           (if anon-term
;;               (if (term-check-proc "*ansi-term*")
;;                   (switch-to-buffer "*ansi-term*")
;;                 (kill-buffer "*ansi-term*")
;;                 (ansi-term term-cmd))
;;             (ansi-term term-cmd)))))

;; (global-set-key [f2] 'visit-ansi-term)
