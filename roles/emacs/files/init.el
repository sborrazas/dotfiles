(defvar my-packages '(exec-path-from-shell
                      markdown-mode
                      expand-region
                      magit
                      ido
                      haml-mode
                      sass-mode
                      coffee-mode
                      haskell-mode
                      yaml-mode
                      smex
                      web-mode
                      scala-mode
                      erc-terminal-notifier
                      tuareg
                      utop
                      merlin
                      multiple-cursors))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; check if packages in my-packages are installed; if not, install.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 my-packages)

;; Add paths to exec-path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (setq exec-path (append exec-path '("/usr/local/bin")))
;; (setq exec-path (append exec-path '("/usr/local/bin")))

;;;; Custom modules settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat user-emacs-directory
            (convert-standard-filename "modules/")))

(load-file
  (concat user-emacs-directory
    (convert-standard-filename "config.el")))

;; web-mode
(require 'web-mode)
(setq web-mode-script-padding 2)
(setq web-mode-style-padding 2)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-pairing nil)
(setq web-mode-enable-auto-opening nil)
(setq web-mode-tag-auto-close-style 0)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

;; grep-ed
(require 'grep-ed)

;; monokai theme
(require 'monokai-theme)
(load-theme 'monokai t)

;; syntax-subword
(require 'syntax-subword)

(global-syntax-subword-mode)

;; switch-window
(require 'switch-window)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; Default config (http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term/)
(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
            (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))))

(global-set-key (kbd "C-c C-t") 'multi-term)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;; Global settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide scrollbars
(scroll-bar-mode -1)
;; Hide toolbar
(tool-bar-mode -1)
;; Empty scratch buffer
(setq initial-scratch-message "")
;; Don't show startup screen
(setq inhibit-splash-screen t)
;; Highlight matching parentheses, or whole expr if not visible.
(show-paren-mode 1)
(setq show-paren-style 'mixed)
;; Enable region narrowing
(put 'narrow-to-region 'disabled nil)
;; Region is like a tipical selection, type and region is replaced
(pending-delete-mode t)
;; AltGr != Meta
(setq ns-right-alternate-modifier nil)
;; ispell bin location
(setq ispell-program-name "/usr/local/bin/ispell")
;; Always y/n o p
(fset 'yes-or-no-p 'y-or-n-p)
;; Add final newline on save
(setq require-final-newline t)
;; No backup files
(setq make-backup-files nil)
;; Don't autosave
(setq auto-save-default nil)
;; Show column number
(setq column-number-mode  t)
;; Delete trailing whitespaces after saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Don't use tabs to indent by default
(setq-default indent-tabs-mode nil)
;; Default tabs with = 2
(setq-default tab-width 2)
;; Indent line function = insert-tab
(setq indent-line-function 'insert-tab)
;; Display a visible bell for system alerts instead of the annoying bell sound
(setq visible-bell 1)
;; Make monday the first day on the week on the calendar
(setq calendar-week-start-day 1)

;; Smarter beginning of line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Go to symbol: jumps to next symbol definition (uses ido)
(defun goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))
      (recenter)))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names (substring-no-properties name))
          (add-to-list 'name-and-pos (cons (substring-no-properties name) position))))))))

(global-set-key (kbd "M-j") 'goto-symbol)

;; My indent and unindent for regions
(defun custom-indent-region (N)
  (interactive "p")
  (if mark-active
      (progn (indent-rigidly (min (mark) (point)) (max (mark) (point)) (* N tab-width))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun custom-unindent-region (N)
  (interactive "p")
  (if mark-active
      (progn (indent-rigidly (min (mark) (point)) (max (mark) (point)) (* N -1 tab-width))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(global-set-key ">" 'custom-indent-region)
(global-set-key "<" 'custom-unindent-region)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-at-pt-timer-delay 0.3)
 '(message-log-max 2000)
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(set-face-attribute 'default nil :height 110)

;;;; Mode specific settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; css/scss specific
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . sass-mode))
(setq css-indent-offset 2)
(setq sass-indent-offset 2)

;; js specific
(setq js-indent-level 2)

;; markdown specific
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; Ruby specific
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(setq ruby-deep-indent-paren nil)

;; html specific
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;;;; Packages settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smex package
;; Smex is a M-x enhancement for Emacs.
;; Built on top of Ido, it provides a convenient interface to your recently and
;; most frequently used commands. And to all the other commands, too.
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; ido-mode package
;; IDO is a helper for buffers and files management. Works well with smex.
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-virtual-buffers nil
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-enable-last-directory-history nil
      ido-record-commands nil)
(ido-mode t)

;; Uniquify package
;; Uniquify will make sure buffer names are unique.
;; If two buffer names are equal, uniquify will insert a number suffix on the buffer name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Magit package
;; Custom magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (set-face-background 'magit-item-highlight "black")))
(put 'scroll-left 'disabled nil)
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(setq magit-repo-dirs '("~/work"))

;; expand-region
;; This will select the current word first, then the current block,
;; then the current def function and so on. VERY practical.
(global-set-key (kbd "M-s") 'er/expand-region)

;;;; Custom keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
(global-set-key (kbd "C-0") 'kill-buffer-and-window)

;; Buffers keybindings
;; Switch current window to the next buffer on the buffer list
(global-set-key (kbd "C-.") 'next-buffer)
;; Switch current window to the previous buffer on the buffer list
(global-set-key (kbd "C-,") 'previous-buffer)
;; Open buffer list and go to it
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Close menu when it's open
(if (menu-bar-mode)
    (menu-bar-mode -1))

;; rcirc
(require 'setup-rcirc)

;; Logs directory
(setq rcirc-log-directory config-irc-log-directory)

;; Change user info
(setq rcirc-default-nick "sborrazas")
(setq rcirc-default-user-name "sborrazas")
(setq rcirc-default-full-name "Sebastian Borrazas")
(setq rcirc-default-port 6667)
(setq rcirc-fill-column 80)

;; Workgroups
(require 'workgroups)
(setq wg-query-for-save-on-emacs-exit nil)
(setq wg-query-for-save-on-workgroups-mode-exit nil)
(workgroups-mode 1)

(defun load-term-workgroup ()
  "Load the Terms workgroup"
  (interactive)
  (wg-create-workgroup "Terms")
  (multi-term)
  (split-window-right)
  (switch-window)
  (multi-term)
  (switch-window))

(defun load-project-workgroup () ;; TODO
  "Load the project git repo"
  (interactive)
  (wg-create-workgroup project-name)
  (split-window-right)
  (switch-window)
  (magit-status (convert-standard-filename "~/work")))

(defun load-irc-workgroup () ;; TODO: Split the window between Freenode and CB
  "Connect to IRC"
  (interactive)
  (wg-create-workgroup "IRC")
  (rcirc nil)
  (split-window-right)
  (switch-window)
  (switch-to-buffer "*irc.freenode.net*"))

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation-mode)

;; Special characters
(global-unset-key (kbd "M-="))
(global-set-key (kbd "M-= M-A") (lambda () (interactive) (insert-char ?Á)))
(global-set-key (kbd "M-= M-a") (lambda () (interactive) (insert-char ?á)))
(global-set-key (kbd "M-= M-E") (lambda () (interactive) (insert-char ?É)))
(global-set-key (kbd "M-= M-e") (lambda () (interactive) (insert-char ?é)))
(global-set-key (kbd "M-= M-I") (lambda () (interactive) (insert-char ?Í)))
(global-set-key (kbd "M-= M-i") (lambda () (interactive) (insert-char ?í)))
(global-set-key (kbd "M-= M-O") (lambda () (interactive) (insert-char ?Ó)))
(global-set-key (kbd "M-= M-o") (lambda () (interactive) (insert-char ?ó)))
(global-set-key (kbd "M-= M-U") (lambda () (interactive) (insert-char ?Ú)))
(global-set-key (kbd "M-= M-u") (lambda () (interactive) (insert-char ?ú)))
(global-set-key (kbd "M-= M-N") (lambda () (interactive) (insert-char ?Ñ)))
(global-set-key (kbd "M-= M-n") (lambda () (interactive) (insert-char ?ñ)))
(global-set-key (kbd "M-= M--") (lambda () (interactive) (insert-char ?—)))

;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(setq agda2-program-args '("-i" "/Users/sborrazas/.agda-libs/src"))
(custom-set-faces
  '(wg-mode-line-face ((t (:foreground "steel blue"))))
  '(agda2-highlight-datatype-face ((t (:inherit font-lock-type-face))))
  '(agda2-highlight-field-face ((t (:foreground "#ad7fa8"))))
  '(agda2-highlight-function-face ((t (:inherit font-lock-function-name-face))))
  '(agda2-highlight-inductive-constructor-face ((t (:inherit font-lock-variable-name-face))))
  '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
  '(agda2-highlight-module-face ((t (:inherit font-lock-builtin-face))))
  '(agda2-highlight-number-face ((t (:inherit font-lock-constant-face))))
  '(agda2-highlight-postulate-face ((t (:inherit font-lock-type-face))))
  '(agda2-highlight-primitive-face ((t (:inherit font-lock-type-face))))
  '(agda2-highlight-primitive-type-face ((t (:inherit font-lock-type-face))))
  '(agda2-highlight-record-face ((t (:inherit font-lock-type-face))))
  '(agda2-highlight-string-face ((t (:inherit font-lock-string-face)))))

;; JSX
(setq jsx-indent-level 2)

;; Bline mode (for lines with column > 80)
(setq whitespace-style '(lines))
(setq whitespace-line-column 80)
(global-whitespace-mode 1)

(defvar bline-minor-mode-font-lock-keywords
  ;; cf. `whitespace-color-on'
  (list
   (list
    (let ((line-column (or whitespace-line-column fill-column)))
      (format
       "^\\([^\t\n]\\{%s\\}\\|[^\t\n]\\{0,%s\\}\t\\)\\{%d\\}%s\\(.+\\)$"
       whitespace-tab-width
       (1- whitespace-tab-width)
       (/ line-column whitespace-tab-width)


       (let ((rem (% 84 whitespace-tab-width)))
         (if (zerop rem)
             ""
           (format ".\\{%d\\}" rem)))))
    0 ; whole line
    whitespace-line t)))

(define-minor-mode bline-minor-mode "Overlong lines can make you blined."
  nil nil nil
  (if bline-minor-mode
      (font-lock-add-keywords nil bline-minor-mode-font-lock-keywords t)
    (font-lock-remove-keywords nil bline-minor-mode-font-lock-keywords))
  (font-lock-mode 1))

(defun bline-minor-mode--insin ()
  (add-hook 'after-change-functions 'bline-minor-mode--uate nil t))

(defun bline-minor-mode--uate (&rest ignore)
  (bline-minor-mode 1)
  (remove-hook 'after-change-functions 'bline-minor-mode--uate t))

(add-hook 'prog-mode-hook 'bline-minor-mode--insin)

;; OCaml & utop configuration
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)
