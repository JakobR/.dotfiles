					;-*-Emacs-Lisp-*-

(require 'package)

; To upgrade packages:
;   M-x package-refresh-contents
;   M-x list-packages U x

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(setq package-enable-at-startup nil)
(package-initialize)
; (setq quelpa-update-melpa-p nil)
; (unless (require 'quelpa nil t)
;   (with-temp-buffer
;     (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
;     (eval-buffer)))

;; install use-package and the quelpa handler
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))
; (quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
; (require 'quelpa-use-package)

(require 'diminish)

(setq byte-compile-warnings nil
      gnutls-min-prime-bits 4096)

;; Call package-refresh-contents before installing something
;; From https://github.com/belak/dotfiles/blob/master/emacs.d/README.org#package-setup
(defvar belak/refreshed-package-list nil
  "This will be t if the package list has been refreshed.")

(defun belak/ensure-refreshed ()
  "Ensure the package list has been refreshed this startup."
  (unless belak/refreshed-package-list
    (package-refresh-contents)
    (setq belak/refreshed-package-list t)))

(defun belak/package-ensure-installed (package)
  "Install a missing PACKAGE if it isn't already."
  (unless (package-installed-p package)
    (package-install package)))

(advice-add 'package-install
            :before
            (lambda (&rest args)
              (belak/ensure-refreshed)))


; (add-to-list 'load-path (expand-file-name "~/.emacs.d/plugin/"))

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
; (load custom-file 'noerror)

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)  ; To disable just for scrolling, see http://stackoverflow.com/a/11679758
(setq require-final-newline t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(use-package exec-path-from-shell
  :ensure t
  :config
  ; Initialize with zsh so I have my PATH as usual
  ; (this is required when Emacs is started from the GUI, because GUI apps are launched in a different environment)
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-arguments '())  ; use non-interactive non-login shell
    (exec-path-from-shell-initialize))
  ; But use sh internally to run commands (no need to load zsh config for every small background task -- but does that even matter if it's not an interactive shell?)
  (setq shell-file-name "/bin/sh"))

; Use latest version of org-mode instead of the bundled one
(use-package org
  :ensure org-plus-contrib
  :pin org  ; load from the "org" package archive
  :config
  ; org-mac-link is not a separate package but a file contained in org-plus-contrib
  (require 'org-mac-link)
  (add-hook 'org-mode-hook (lambda ()
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))
  )

; (use-package markdown-mode :ensure t)
(use-package evil
  :ensure t
  :config

  (use-package evil-leader
    :ensure t
    :config
    ;; Note: global-evil-leader-mode should be enabled before evil-mode
    (global-evil-leader-mode))

  (evil-mode 1)

  (add-to-list 'evil-emacs-state-modes 'ag-mode)
  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
  (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)

  ; (defun tab-to-tab-stop-reverse ()
  ;   "Like `tab-to-tab-stop', but toggle direction with prefix."
  ;   (interactive)
  ;   (let ((nexttab (indent-next-tab-stop (current-column) t)))
  ;     (delete-horizontal-space t)
  ;     (indent-to nexttab)))
  ; (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  ; TODO
  ; (define-key evil-insert-state-map (kbd "DEL") 'tab-to-tab-stop-reverse)
  (define-key evil-insert-state-map (kbd "A-TAB") 'tab-to-tab-stop)
  (setq backward-delete-char-untabify-method 'hungry)

  (add-to-list 'load-path (expand-file-name "~/.emacs.d/plugin/evil-plugins"))
  (require 'evil-little-word)  ; from evil-plugins at https://github.com/tarao/evil-plugins
  ;; TODO: can I use <leader>w for these?
  ;(define-key evil-motion-state-map (kbd "glw") 'evil-forward-little-word-begin)
  ;(define-key evil-motion-state-map (kbd "glb") 'evil-backward-little-word-begin)
  ;(define-key evil-motion-state-map (kbd "gle") 'evil-forward-little-word-end)
  (define-key evil-outer-text-objects-map (kbd "v") 'evil-a-little-word)
  (define-key evil-inner-text-objects-map (kbd "v") 'evil-inner-little-word)

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

  ;(use-package evil-jumper
  ;  :ensure t
  ;  :config
  ;  (global-evil-jumper-mode))

  (use-package evil-numbers
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
    )

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

  (use-package evil-org
    :ensure t
    :after org
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
    (evil-leader/set-key-for-mode 'org-mode "o" 'org-open-at-point)
    )
  )

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)

  (setq flycheck-python-pycompile-executable "python3")

  ; (use-package flycheck-color-mode-line
  ;   :ensure t
  ;   :config
  ;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

  (use-package flycheck-pos-tip
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode))
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package company
  :ensure t
  :config
  ; (add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode)
  (setq company-idle-delay 0)  ; 0.05
  (setq company-minimum-prefix-length 3)
  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-files)
  ; TODO: https://github.com/company-mode/company-mode/pull/706
  ; See https://github.com/company-mode/company-mode/blob/master/NEWS.md (company-tng-configure-default)
  ; Also look at https://github.com/abingham/emacs-ycmd (can probably just use the ycmd that's contained in the vim package?)
  )

(use-package helm
  :ensure t
  :diminish helm-mode
;   :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
;   (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
;   (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)

  ; TODO: this doesn't work. see also
  ; https://emacs.stackexchange.com/questions/4062/evil-mode-make-helm-quit-with-the-escape-key/4064
  ; https://github.com/syl20bnr/evil-escape
  ; https://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
  ; (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
  ; (define-key helm-find-files-map (kbd "ESC") 'helm-keyboard-quit)
  )

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  ;; (setq projectile-enable-caching t) ; apparently caching is not required with projectile-indexing-method set to "alien"? cf. https://emacs.stackexchange.com/a/2169
  (global-set-key (kbd "M-t") 'projectile-find-file)
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  )

; See http://commercialhaskell.github.io/intero/
(use-package intero
  :ensure t
  :after flycheck company
  :config
  (intero-global-mode 1)
  )

;; Doesn't seem to work with my Emacs GUI
; (use-package osx-pseudo-daemon
;   :ensure t
;   :config
;   (osx-pseudo-daemon-mode))
;;(require 'osx-pseudo-daemon)


(defvar jr-emacs-tmp "~/.tmp-emacs/")
(defvar jr-backup-directory "~/.tmp-emacs/backup/")
(if (not (file-exists-p jr-backup-directory)) (make-directory jr-backup-directory t))
(defvar jr-autosave-directory "~/.tmp-emacs/autosave/")
(if (not (file-exists-p jr-autosave-directory)) (make-directory jr-autosave-directory t))
;; Backup settings
(setq backup-directory-alist `(("." . ,jr-backup-directory))  ;; Keep backup files out of the way
      make-backup-files t
      vc-make-backup-files t        ;; Also back up files that are under version control.
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t
      version-control t             ;; Use version numbers for backups.
      delete-old-versions t         ;; Don't ask to delete excess backup versions.
      delete-by-moving-to-trash t
      kept-new-versions 20
      kept-old-versions 10
      )
;; Keep autosave files out of the way
(setq auto-save-file-name-transforms `((".*" ,jr-autosave-directory t)))
;; Reload files when they are changed by other programs
(global-auto-revert-mode t)

(global-set-key (kbd "<f17>") 'switch-to-buffer)

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
(setq redisplay-dont-pause t
  scroll-margin 3
  ; scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
;(use-package projectile :ensure t)
;(use-package magit :ensure t)
;(use-package powerline :ensure t)

(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  (setq nlinum-relative-redisplay-delay 0)
  (setq nlinum-format "%4d")
  (add-hook 'prog-mode-hook #'nlinum-relative-mode)
  )

(use-package color-theme
  :ensure t)
(use-package color-theme-solarized
  :ensure t
  :after color-theme
  :config
  (load-theme 'solarized t)
  )

(set-frame-font "Menlo 14" nil t)
(setq-default frame-title-format
              '(:eval
                (format "%s %s%s - Emacs"
                        (buffer-name)
                        (if (buffer-modified-p) "+ " "")
                        (cond
                         (buffer-file-truename
                          (concat "(" (directory-file-name (file-name-directory buffer-file-truename)) ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]")))))

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-stop-list '(4))
(add-hook 'haskell-mode-hook (lambda () (setq tab-width 2 tab-stop-list '(2))))

(defun set-yasnippet-fixed-indent ()
  (setq-local yas-indent-line 'fixed))
(add-hook 'haskell-mode-hook #'set-yasnippet-fixed-indent)

; (setq whitespace-style '(face trailing tab-mark newline-mark))
(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode)

(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

(setq org-agenda-files '("~/org/")
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
;; See https://emacs.stackexchange.com/a/21138 for a possible fix
(defun my-redo-all-agenda-buffers ()
  "Rebuild all open agenda buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)
        (message "[org agenda] refreshed!")))))
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

;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)
(require 'paren)
;; (set-face-background 'show-paren-match (face-background 'default))
(set-face-background 'show-paren-match "#ddd")
(set-face-foreground 'show-paren-match "#f00")
;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(column-number-mode)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
