(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
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
   '(company lsp-ui flycheck yasnippet dap-mode which-key treemacs-projectile helm-projectile helm-lsp lsp-treemacs lsp-mode markdown-mode ess hl-todo foggy-night-theme use-package julia-repl julia-mode company-math))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp) (comp) (comp))))
;; 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'company-math)
;; Key-bindings
;; Undef
(global-unset-key (kbd "C-z"))
;; Go to previous window
(global-set-key (kbd "C-x p") (lambda ()
			(interactive)
			(other-window -1)))


(defun chris/julia-hooks ()
  (highlight-symbol-mode)
  (lsp)
  (lsp-ui-peek))
(use-package julia-mode
  ;; :after ess
  :hook (julia-mode . bill/julia-hooks))

;; Julia-support
(require 'julia-mode)
(setq julia-max-block-lookback 200000)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode
(setq julia-repl-switches "-O3 -q -tauto --startup=no -L/home/chriselrod/.julia/config/emacs_startup.jl")
;; (setq julia-repl-switches "-O3 -q -t18 -C'native,-prefer-256-bit' --startup=no -L/home/chriselrod/.julia/config/emacs_startup.jl")
(setq julia-repl-executable-records
      '((default "/home/chriselrod/Documents/languages/julia-master/usr/bin/julia")
	(release "/home/chriselrod/Documents/languages/julia/usr/bin/julia")))

(add-hook 'julia-repl-hook #'julia-repl-use-emacsclient)
;; (setenv "JULIA_NUM_THREADS" "36")

(require 'treemacs)
(use-package treemacs
    :hook (after-init .#'treemacs))
(add-hook 'after-init-hook 'global-company-mode)

;; (add-to-list 'company-backends `(company-dabbrev))
;; (setq company-dabbrev-downcase nil)
;; treat underscores as a part of a word
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))


(add-hook 'after-init-hook #'global-flycheck-mode)
(company-mode)
(helm-mode)
(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(use-package lsp-ui :commands lsp-ui-mode)
;;(use-package lsp-ui)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package which-key
    :config
    (which-key-mode))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(setq lsp-keymap-prefix "s-p")

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

(setq load-path (cons (expand-file-name "/home/chriselrod/Documents/languages/llvm-project/llvm/utils/emacs") load-path))
(require 'llvm-mode)

(require 'cmake-mode)

(setq inhibit-splash-screen t)

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



