(require 'package)

(add-to-list 'package-archives
            '("marmalade" . "https://marmalade-repo.org/packages/") t)
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

(use-package col-highlight
  :ensure t
  :config
  (progn
    (col-highlight-set-interval 1)
    (toggle-highlight-column-when-idle 1)
    (set-face-background 'col-highlight "color-235")))

;; brew install --HEAD ctags
;; brew install global --with-exuberant-ctags
;; (use-package ggtags
;;   :ensure
;;   :bind (("C-c C-g" . ggtags-mode)))

(use-package ace-jump-mode
  :ensure
  :bind (("C-j" . ace-jump-mode)))

(use-package ag
  :ensure)

(use-package thrift
  :ensure
  :config
  (setq thrift-indent-level 2))

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

(use-package pytest
  :ensure
  :bind (("C-c t" . pytest-module)))

(use-package go-mode
  :ensure)
(use-package gotest
  :ensure)
(use-package go-eldoc
  :ensure)
(use-package go-autocomplete
  :ensure)
(use-package go-dlv
  :ensure)
(use-package go-rename
  :ensure)

(defvar path-to-emacs-init (if load-file-name
                               (file-name-directory load-file-name)
                             default-directory))

;; (load (expand-file-name "datclip.el" path-to-emacs-init))
;; (load (expand-file-name "pbcopy.el" path-to-emacs-init))
;; (require 'pbcopy)
;; (turn-on-pbcopy)

;; Copy from Emacs to OS X clipboard:
;; select region then M-| pbcopy RET
;; Paste from OS X clipboard to Emacs:
;; C-u M-| pbpaste RET (replaces current region if it exists)




(setq path-to-ctags "/usr/local/bin/ctags")
(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "%s -f TAGS -e -R %s --exlucde=.git --exclude=log" path-to-ctags (directory-file-name dir-name)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom auto-added faces and variables.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(gofmt-args (quote ("-local" "${PROJECT_ROOT}")))
 '(gofmt-command "goimports")
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (yaml-mode use-package thrift textmate smex rbenv pytest magit ido-vertical-mode gotest go-rename go-eldoc go-dlv go-autocomplete ggtags flx-ido col-highlight ag ace-jump-mode)))
 '(safe-local-variable-values
   (quote
    ((eval setenv "PROJECT_ROOT" "abc")
    )))
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

(setenv "GOPATH" (concat (getenv "HOME") "/code/go"))
(setq exec-path (cons
                 (concat (getenv "HOME") "/code/go/bin")
                 exec-path))
(setenv "PATH" (concat (getenv "HOME") "/code/go/bin:" (getenv "PATH")))

(define-key go-mode-map (kbd "C-c f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-c t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-c p") 'go-test-current-project)
(define-key go-mode-map (kbd "M-.") 'godef-jump)
(define-key go-mode-map (kbd "M-*") 'pop-tag-mark)
;; (define-key go-mode-map (kbd "C-x b") 'go-test-current-benchmark)
(define-key go-mode-map (kbd "C-x x") 'go-run)
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

(global-set-key (kbd "M-SPC") 'mark-sexp)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
