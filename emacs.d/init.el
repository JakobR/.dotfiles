					;-*-Emacs-Lisp-*-

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

; (setq package-enable-at-startup nil)
(package-initialize)
; (if (require 'quelpa nil t)
;     (quelpa-self-upgrade)
;   (with-temp-buffer
;     (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
;     (eval-buffer)))
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; install use-package and the quelpa handler
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)


;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
; (load custom-file 'noerror)

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)  ; To disable just for scrolling, see http://stackoverflow.com/a/11679758
(setq require-final-newline t)

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
    :ensure t
    :config
    (global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
    (global-set-key (kbd "S-M-/") 'evilnc-comment-or-uncomment-paragraph)
    )

  ; (use-package evil-tabs
  ;   :ensure t
  ;   :config
  ;   (global-evil-tabs-mode t))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  ; (use-package evil-indent-textobject
  ;   :ensure t)
  )

(use-package helm
  :ensure t
;   :diminish helm-mode
;   :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
;   (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
;   (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)
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

(use-package emacs-color-theme-solarized
  :quelpa (emacs-color-theme-solarized :repo "sellout/emacs-color-theme-solarized" :fetcher github))
(load-theme 'solarized t)  ; The :config section doesn't seem to work for this package...

; (load-theme 'solarized t)
; (use-package solarized-theme
;   :ensure t
;   :config
;   (load-theme 'solarized-light)
;   (set-face-attribute 'fringe nil :background "#EDE8D6")
;   (set-face-attribute 'linum nil :background "#EDE8D6")
;   (set-face-attribute 'nlinum-relative-current-face nil :foreground "#A3292C" :background "#FCF6E4")
;   (setq solarized-scale-org-headlines nil)
;   )

(set-frame-font "Menlo 14")

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-stop-list '(4))

; (setq whitespace-style '(face trailing tab-mark newline-mark))
(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode)

(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

(setq org-agenda-files '("~/org/" "~/org/project/")
      org-agenda-start-on-weekday nil  ;; Always start agenda on current day
      org-agenda-span 'week
      )

;; Keep track of when TODO item are finished
(setq org-log-done 'time)

;; The week starts on monday
(setq calendar-week-start-day 1)

; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
; (setq org-agenda-custom-commands
;       '(("c" "Simple agenda view"
;          ; ((tags "PRIORITY=\"A\""
;          ;        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;          ;         (org-agenda-overriding-header "High-priority unfinished tasks:")))
;           (agenda "")
;           (alltodo "")))))
(require 'org-agenda)  ;; TODO: maybe use hook instead
(define-key org-agenda-mode-map "j" 'org-agenda-next-item)
(define-key org-agenda-mode-map "k" 'org-agenda-previous-item)
(define-key mode-specific-map [?a] 'org-agenda)  ;; `C-c a` for org-agenda

;; Refresh agenda views when saving org files, see http://emacs.stackexchange.com/a/16328
;; TODO: This somehow break the agenda buffer... keys like b/f don't work any more, and it does not update on future saves, either
(defun my-redo-all-agenda-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-redo t)))))
(add-hook 'org-mode-hook (lambda() (add-hook 'after-save-hook 'my-redo-all-agenda-buffers nil 'make-it-local)))

; (setq org-todo-keywords
;       '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))


; (add-hook 'org-mode-hook
;           (lambda ()
;             (font-lock-add-keywords nil
;                                     ; '(("\\(^\\|\\s-+\\)\\(_[a-zA-Z0-9].*?:\\)" 2
;                                     ; '(("\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^\r\n>]*?\\)\\]" 0
;                                     '(("\\[[0-9]+\\]" 0
;                                        font-lock-warning-face t)))))

; TODO: this is supposed to change the highlighting of inactive timestamps, but I can't get it to override the existing one
; (font-lock-add-keywords 'org-mode
;                         '(
;                           ("\\(^\\|\\s-+\\)\\(_[a-zA-Z0-9].*?:\\)" 2 font-lock-warning-face t)
;                         ; '(("\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^\r\n>]*?\\)\\]" 0
;                             ("a\\[123\\]x" 0 'font-lock-warning-face t)
;                          )
;                         )

(column-number-mode)

(server-start)
