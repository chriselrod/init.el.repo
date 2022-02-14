(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

(unless (package-installed-p 'foggy-night-theme)
  (package-refresh-contents)
  (package-install 'foggy-night-theme))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242728" "#ff0066" "#63de5d" "#E6DB74" "#06d8ff" "#ff8eff" "#53f2dc" "#f8fbfc"])
 '(compilation-message-face 'default)
 '(custom-enabled-themes '(foggy-night))
 '(custom-safe-themes
   '("f6cdb429a64db06d3db965871b45ed1c666fdce2d3e2c4b810868e4cf4244c92" default))
 '(fci-rule-color "#323342")
 '(flycheck-checker-error-threshold 4000)
 '(global-display-line-numbers-mode t)
 '(highlight-changes-colors '("#ff8eff" "#ab7eff"))
 '(highlight-tail-colors
   '(("#323342" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#323342" . 100)))
 '(package-selected-packages
   '(flycheck-clang-analyzer flycheck-clang-tidy cmake-mode exec-path-from-shell toml-mode rust-playground rustic rust-mode company lsp-ui flycheck yasnippet dap-mode which-key treemacs-projectile helm-projectile helm-lsp lsp-treemacs lsp-mode markdown-mode hl-todo foggy-night-theme use-package julia-repl julia-mode magit))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp) (comp) (comp))))
;; Commented out font as this causes lsp-ui sideline text to wrap. https://github.com/emacs-lsp/lsp-ui/issues/231
(custom-set-faces)
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "Iosevka Term Curly" :foundry "UKWN" :slant normal :weight regular :height 120 :width normal)))))



(setq inhibit-splash-screen t)
;; Key-bindings
;; Undef
(global-unset-key (kbd "C-z"))
;; Go to previous window
(global-set-key (kbd "C-x p") (lambda ()
			(interactive)
			(other-window -1)))

(use-package magit)

(use-package treemacs
    :hook (after-init .#'treemacs))
(use-package treemacs-projectile)
;; (add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(use-package helm)
(use-package helm-projectile)
(use-package which-key
    :config
    (which-key-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  ((c-mode c++-mode julia-mode rustic) . lsp-deferred)
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-keymap-prefix "s-p")
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-restart 'auto-restart))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  ;; (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0.05))

(use-package julia-mode)
  ;; :hook (julia-mode . ((highlight-symbol-mode)
  ;; (lsp)
  ;; (lsp-ui-peek)))
  ;; :custom
  ;; julia-max-block-lookback 200000)
(use-package julia-repl
  :config
  (setq julia-repl-switches "-O3 -q -tauto")
  (setq julia-repl-executable-records
   '((default "/home/chriselrod/Documents/languages/julia/usr/bin/julia")
     (release "/home/chriselrod/Documents/languages/juliarelease/usr/bin/julia")))
  (add-hook 'julia-mode-hook 'julia-repl-mode))
(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.8"))

;; https://github.com/rksm/emacs-rust-config
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))
;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)


(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package flycheck)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))
 
(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Create / cleanup rust scratch projects quickly

(use-package rust-playground)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	   :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))

(setq load-path (cons (expand-file-name "~/.emacs.d/init.el.repo/llvm_mode") load-path))
(require 'llvm-mode)

(use-package cmake-mode)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

(require 'org)

(setq julia-indent-offset 2)
(setq c-basic-offset 4)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(global-auto-revert-mode 1)
(setq auto-revert-avoid-polling 1)

;; (setq lsp-restart auto-restart)

(put 'downcase-region 'disabled nil)

(use-package flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  )
(use-package flycheck-clang-analyzer
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

