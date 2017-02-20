					;-*-Emacs-Lisp-*-

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)


;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(menu-bar-mode -1)
(tool-bar-mode -1)

; (use-package markdown-mode :ensure t)
(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (dolist (mode '(ag-mode
		  flycheck-error-list-mode
		  git-rebase-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ; (evil-add-hjkl-bindings occur-mode-map 'emacs
  ;   (kbd "/")       'evil-search-forward
  ;   (kbd "n")       'evil-search-next
  ;   (kbd "N")       'evil-search-previous
  ;   (kbd "C-d")     'evil-scroll-down
  ;   (kbd "C-u")     'evil-scroll-up
  ;   (kbd "C-w C-w") 'other-window)
  (add-hook 'occur-mode-hook
	    (lambda ()
	      (evil-add-hjkl-bindings occur-mode-map 'emacs
		(kbd "/")       'evil-search-forward
		(kbd "n")       'evil-search-next
		(kbd "N")       'evil-search-previous
		(kbd "C-d")     'evil-scroll-down
		(kbd "C-u")     'evil-scroll-up
		(kbd "C-w C-w") 'other-window)))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))

  ;(use-package evil-jumper
  ;  :ensure t
  ;  :config
  ;  (global-evil-jumper-mode))

  (use-package evil-numbers
    :ensure t)

  (use-package evil-nerd-commenter
    :ensure t)

  ; (use-package evil-tabs
  ;   :ensure t
  ;   :config
  ;   (global-evil-tabs-mode t))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t))

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)
  )

(defvar jr-emacs-tmp "~/.emacs-tmp/")
(defvar jr-backup-directory "~/.emacs-tmp/backup")
(if (not (file-exists-p jr-backup-directory))
    (make-directory jr-backup-directory t))
(setq backup-directory-alist `(("." . ,jr-backup-directory))
      make-backup-files t
      vc-make-backup-files t ;; Also backup files that are under version control.
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t
      version-control t            ;; Use version numbers for backups.
      delete-old-versions t        ;; Don't ask to delete excess backup versions.
      delete-by-moving-to-trash t
      kept-new-versions 20
      kept-old-versions 10
      )

(global-set-key (kbd "<f17>") 'buffer-list)

; Restore cursor position after reopening file
(setq save-place-file (concat jr-emacs-tmp "places")
      save-place-forget-unreadable-files nil)
(save-place-mode 1)

; (use-package smooth-scroll
;   :ensure t
;   :config
;   (smooth-scroll-mode 1)
;   (setq smooth-scroll/vscroll-step-size 5)
;   )
; (setq scroll-margin 3)
(setq redisplay-dont-pause t
  scroll-margin 3
  ; scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
;(use-package projectile :ensure t)
;(use-package magit :ensure t)
;(use-package powerline :ensure t)

; (use-package linum-relative
;   :ensure t
;   :config
;   (linum-relative-global-mode)

(use-package nlinum-relative
 :ensure t
 :config
 (nlinum-relative-setup-evil)
 (setq nlinum-relative-redisplay-delay 0)
 (setq nlinum-format "%4d")
 ; (global-nlinum-relative-mode)
 (add-hook 'prog-mode-hook #'nlinum-relative-mode))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light)
  (set-face-attribute 'fringe nil :background "#EDE8D6")
  (set-face-attribute 'linum nil :background "#EDE8D6")
  (set-face-attribute 'nlinum-relative-current-face nil :foreground "#A3292C" :background "#FCF6E4")
  )

(set-frame-font "Menlo 14")

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-stop-list '(4))

; (setq whitespace-style '(face trailing tab-mark newline-mark))
(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode)

; Use org-mode as default for all files with unspecified mode
(setq major-mode 'org-mode)

(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

(setq org-agenda-files '("~/org/" "~/org/project/"))

; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
; (setq org-agenda-custom-commands
;       '(("c" "Simple agenda view"
;          ; ((tags "PRIORITY=\"A\""
;          ;        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;          ;         (org-agenda-overriding-header "High-priority unfinished tasks:")))
;           (agenda "")
;           (alltodo "")))))
;(define-key org-agenda-mode-map "j" 'org-agenda-next-item)
;(define-key org-agenda-mode-map "k" 'org-agenda-previous-item)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

(column-number-mode)

(server-start)
