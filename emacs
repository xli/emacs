(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "~/emacs")

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(if (not (package-installed-p 'use-package))
    (package-install 'use-package))

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :ensure)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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

(use-package ansi-color
  :ensure
  :config
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))

  (add-hook 'compilation-filter-hook
            #'endless/colorize-compilation)
  )

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-enable-file-watchers nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  ;; (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
)

;; (defun rk/rustic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm, but don't try to
;;   ;; save rust buffers that are not file visiting. Once
;;   ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
;;   ;; no longer be necessary.
;;   (when buffer-file-name
;;     (setq-local buffer-save-without-query t)))

(use-package lsp-mode
  :ensure
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :commands lsp
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
)
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package flycheck
  :ensure)

(use-package flycheck-rust
  :ensure
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
)

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package protocols
  :ensure)
(use-package protobuf-mode
  :ensure)

(use-package typescript-mode
  :ensure)

(defvar path-to-emacs-init (if load-file-name
                               (file-name-directory load-file-name)
                             default-directory))


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
 '(ag-arguments '("--smart-case" "--stats" "--ignore-dir" "target"))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1e1e1e" "#cf6a4c" "#8f9d6a" "#f9ee98" "#7587a6" "#9b859d" "#7587a6" "#a7a7a7"])
 '(column-number-mode t)
 '(custom-enabled-themes '(tango-dark))
 '(custom-safe-themes
   '("8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "722e1cd0dad601ec6567c32520126e42a8031cd72e05d2221ff511b58545b108" "074f60822c8a332b1500092daae2fe048e43a11072c70b6f0d249931bdbe55dc" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "59ba50f24540958f33699a5247255d10f34dd812f3975837e3eddccdc4caa32e" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "87d46d0ad89557c616d04bef34afd191234992c4eb955ff3c60c6aa3afc2e5cc" "8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" "d2bd16a8bcf295dce0b70e1d2b5c17bb34cb28224a86ee770d56e6c22a565013" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" default))
 '(gofmt-args '("-local" "${PROJECT_ROOT}"))
 '(gofmt-command "goimports")
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(typescript-mode lsp-ui lsp-mode rustic apib-mode protobuf-mode protocols yasnippet company flycheck-rust flycheck go-rename go-dlv go-autocomplete go-eldoc gotest go-mode pytest yaml-mode rbenv textmate thrift ag ace-jump-mode magit ido-vertical-mode flx-ido smex exec-path-from-shell use-package))
 '(safe-local-variable-values '((eval setenv "PROJECT_ROOT" "abc")))
 '(save-place-file "~/.emacs.d/emacs.places")
 '(save-place-mode t nil (saveplace))
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style 'post-forward nil (uniquify))
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


;; Javascript
(setq js-indent-level 4)

;; css
(setq css-indent-offset 2)

;; remote

;; (let ((default-directory "/ssh:<host>:/<dir>"))
;;   (start-file-process "grep" (get-buffer-create "*grep*")
;;                       "/bin/sh" "-c" "grep -e tramp *"))


;; global
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(setq objc-mode-hook
      (function (lambda ()
                  (setq indent-tabs-mode nil)
                  (setq c-indent-level 4))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'gofmt-before-save)

(setenv "GOROOT" "/Users/ilx/homebrew/opt/go/libexec")
(setenv "PATH" (concat (getenv "PATH") ":/Users/ilx/go/bin"))
(setq exec-path (append exec-path '("/Users/ilx/go/bin")))

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
 '(cursor ((t nil))))
