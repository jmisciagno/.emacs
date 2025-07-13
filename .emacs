;;; open Emacs
(setq debug-on-error t)
(set-frame-height (selected-frame) 60)
(set-frame-width (selected-frame) 120)
(set-frame-position (selected-frame) 315 25)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq auto-save-default nil)
(fset 'yes-or-no-p 'y-or-n-p)
(display-time)

(require 'package)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "http://melpa.org/packages/")))

(add-to-list 'load-path "~")
(add-to-list 'load-path "~/emacs.d/elpa/helm-20230718.638")
(add-to-list 'load-path "~/emacs.d/elpa/vale-mode-20190725.125")
(add-to-list 'load-path "~/emacs.d/elpa/yaml-20250316.1721")
(add-to-list 'load-path "~/emacs.d/elpa/dash-20250312.1307")
(add-to-list 'load-path "~/emacs.d/elpa/flycheck-20250527.907")
(add-to-list 'load-path "~/Emacs")
(add-to-list 'load-path "~/Emacs/gforth")
(add-to-list 'load-path "~/Emacs/matlab")
(add-to-list 'load-path "~/Emacs/langtool")
(add-to-list 'load-path "~/Emacs/writer")
(add-to-list 'load-path "~/Emacs/language-mode")

;;; Set $PATH for shell-commands
(exec-path-from-shell-initialize)

;;; Helm
(require 'helm)
(require 'helm-find)
(require 'helm-command)

(setq helm-use-delayed-cursor nil) ; bug fix?
(define-key helm-find-files-map (kbd "C-c C-x") nil) ; remove binding
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(set-face-attribute 'helm-ff-directory nil :background "antique white" :foreground "DarkRed")
(setq delete-by-moving-to-trash t) ; move files to trash

; Open file externally
(global-set-key (kbd "C-c o") 'open-buffer-file-name-externally)
(define-key helm-find-files-map (kbd "C-c o") 'helm-ff-run-open-file-externally)
(defun open-buffer-file-name-externally ()
  (interactive)
  (if buffer-file-name
      (let* ((ext (file-name-extension buffer-file-name))
	     (res (assoc ext helm-external-programs-associations)))
	(cond ((not res) (princ "Failed to open.  Must set default program in Helm first."))
	      ((not (cdr res)) (princ "Emacs is the default program."))
	      (t (shell-command (concat (cdr res) " \"" buffer-file-name "\"")))))
      (princ "Failed to open.  The current bufffer is not a file.")))

; Filter Files
(setq helm-ignore-files-list (list ".DS_Store" ".localized"))
(defun helm-filter-files (files)
  (seq-filter
   (lambda (f)
     (let ((name (file-name-nondirectory f)))
       (not (member name helm-ignore-files-list))))
   files))

; Open Application
(global-set-key (kbd "C-c a") (lambda () (interactive) (helm :sources '(helm-open-application))))
(setq helm-open-application
      `((name . "Open Application")
        (candidates . ,(mapcar #'file-name-base (helm-filter-files (helm-list-directory "/Applications/"))))
        (action . (lambda (candidate) (shell-command (concat "open -a \"" candidate ".app\""))))))

; Adjust Directory
(advice-add 'helm-list-directory :filter-return #'helm-filter-files)

;;; highlight line configuration
(require 'hl-line)
(setq cursor-type nil)

;;; redo
(require 'redo)
(global-set-key (kbd "M-z") 'redo)

;;; windows-like behavior
(cua-mode t)
(delete-selection-mode 1)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq shift-select-mode nil)

;;; Word wrap in every mode and hide in modeline
(add-hook 'change-major-mode-after-body-hook (lambda () (visual-line-mode 1)))
(setcar (cdr (assq 'visual-line-mode minor-mode-alist)) "") ; no Mode Line display for word wrap

;;; Hide Buffer-face-mode (increase/decrease text size) in mode line

;;; configure mouse
(require 'mouse)
(unless window-system
  (xterm-mouse-mode 1)
  (defun track-mouse (e)) ;; disable
  (setq mouse-sel-mode t)
  (global-set-key [down-mouse-1] ; unmark on single click
    (lambda (e) 
      (interactive "e")
      (deactivate-mark)
      (mouse-set-point e)))
  (global-set-key [mouse-4] '(lambda (e) (interactive "e") (mouse-set-point e) (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda (e) (interactive "e") (mouse-set-point e) (scroll-up 1))))

(require 'maxframe)
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

;;; Emacs commands
(global-set-key (kbd "C-x C-c") 'soft-quit-emacs) ; replace kill-emacs with soft-quit
(global-set-key (kbd "C-c C-c") 'toggle-comment)
(global-set-key (kbd "C-x u") nil) ; unbind
(global-set-key (kbd "C-d") 'describe-key)
;(global-set-key (kbd "C-x <left>") (lambda ()(interactive) (my-other-window -1)))
;(global-set-key (kbd "C-x <right>") (lambda () (interactive (my-other-window 1))))
;(global-set-key (kbd "C-x <down>") 'next-buffer)  ;; intentionally swapped with C-x <right>
;(global-set-key (kbd "C-x <up>") 'previous-buffer)
(global-set-key (kbd "C-x 3") 'my-split-window-horizontally)
(global-set-key (kbd "C-x 2") 'my-split-window-vertically)
(global-set-key (kbd "C-x C-m") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-x SPC") 'minibuffer-complete-word)
(global-set-key (kbd "ESC <up>") 'beginning-of-buffer)
(global-set-key (kbd "C-c p") 'previous-buffer)  ;; intentionally swapped with C-x <right>
(global-set-key (kbd "C-c n") 'next-buffer)  ;; intentionally swapped with C-x <right>
(global-set-key (kbd "C-p") 'previous-line_)
(global-set-key (kbd "C-n") 'next-line_)
(global-set-key (kbd "M-p") 'beginning-of-buffer)
(global-set-key (kbd "M-n") 'end-of-buffer)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-l") 'select-current-line)
(global-set-key (kbd "C-w") 'select-current-word)
(global-set-key (kbd "C-x C-K") 'kill-all-buffers)
(global-set-key (kbd "C-x p") (lambda () (interactive (my-other-window -1))))
(global-set-key (kbd "C-x n") (lambda () (interactive (my-other-window 1))))
(global-set-key (kbd "M-y") (lambda nil (interactive)(insert "λ")))
(global-set-key (kbd "M-.") (lambda nil (interactive)(insert "∘")))
(global-set-key (kbd "M-*") (lambda nil (interactive)(insert "×")))
(global-set-key (kbd "M-j") (lambda nil (interactive)(insert "Δ")))
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (restore-frame)
	(maximize-frame)))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun soft-quit-emacs ()
  (interactive)
  (kill-all-buffers)
  (ns-do-hide-emacs)
  (load-file user-init-file)
  (princ ""))

(defun toggle-comment ()
  "comment or uncomment current line"
  (interactive)
  (setq toggle-comment 'comment-or-uncomment-region)
  (call-interactively toggle-comment))

(defun my-other-window (x)
  (interactive)
  (other-window x)
  (if (string= (buffer-name (current-buffer)) "*shell*")
      (end-of-buffer)))

(defun next-line_ ()
  (interactive)
  (if (string= (buffer-name (current-buffer)) "*shell*")
      (comint-next-input 1))
      (next-line))

(defun previous-line_ ()
  (interactive)
  (if (string= (buffer-name (current-buffer)) "*shell*")
      (comint-previous-input 1)
      (previous-line)))

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun select-current-word ()
  "Select the current word"
  (interactive)
  (forward-word 1) 
  (set-mark (point))
  (backward-word 1))

(defun select-current-symbol ()
  "Select the current word"
  (interactive)
  (forward-symbol 1) 
  (set-mark (point))
  (forward-symbol -1))

(defun secondary-select-current-word ()
  "Secondary-select the current word"
  (interactive)
  (forward-word 1) ; move to end of line
  (set-mark (point))
  (backward-word 1))

(defun my-split-window-horizontally ()
  "Split window horizontally and move cursor"
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun my-split-window-vertically ()
  "Split window vertically and move cursor"
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun open ()
  (interactive)
   (shell-command (concat "open " (read-from-minibuffer "Find file: "))))

;;; Org Mode
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-startup-latex-with-latex-preview t)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(require 'language-mode)
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "C-c l") #'language-mode)))

(require 'writer)
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "C-c w") #'writer-mode)))

;;; Emacs Lisp Mode
(add-to-list 'auto-mode-alist '("\\.emacs\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook (lambda () (define-key emacs-lisp-mode-map (kbd "C-c e") 'eval-buffer)))
(add-hook 'emacs-lisp-mode-hook (lambda () (define-key emacs-lisp-mode-map (kbd "M-e") 'eval-expression)))

;;; HTML Mode 
(defun html-eval ()
  (interactive)
  (shell-command (concat "open " (buffer-name))))

(add-hook 'html-mode-hook (lambda () (define-key html-mode-map (kbd "C-c e") 'html-eval)))
(add-hook 'js-mode-hook (lambda () (define-key js-mode-map (kbd "C-c e") 'html-eval)))

;;; C Mode
(defun c-eval ()
  (interactive)
  (let ((res (shell-command (concat "gcc -o /tmp/a.out" (buffer-name)))))
    (if (eq res 0) (shell-command "/tmp/a.out") res)))

(add-hook 'c-mode-hook (lambda () (define-key c-mode-map (kbd "C-c e") 'c-eval)))

;;; C++ Mode
(defun c++-eval ()
  (interactive)
  (let ((res (shell-command (concat "g++ -std=c++11 -o /tmp/a.out " (buffer-name)))))
    (if (eq res 0) (shell-command "/tmp/a.out") res)))

(add-hook 'c++-mode-hook (lambda () (define-key c++-mode-map (kbd "C-c e") 'c++-eval)))

;;; Forth Mode
(ignore-errors (require 'gforth))
(add-to-list 'auto-mode-alist '("\\.f\\'" . forth-mode))
(defun forth-eval ()
  (interactive)
  (shell-command (concat "/usr/local/Cellar/gforth/0.7.3_3/bin/gforth " (buffer-name) " ~/Emacs/gforth/end.f")))

(add-hook 'forth-mode-hook (lambda () (define-key forth-mode-map (kbd "C-c e") 'forth-eval)))

;;; Scheme Mode
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(defun scheme-eval ()
  (interactive)
  (shell-command (concat "/usr/local/bin/petite -q " (buffer-name))))
(add-hook 'scheme-mode-hook (lambda () (define-key scheme-mode-map (kbd "C-c e") 'scheme-eval)))


;;; ASP Prolog
(add-to-list 'auto-mode-alist '("\\.lp\\'" . prolog-mode))
(defun prolog-eval ()
  (interactive)
  (shell-command (concat "/usr/local/bin/clingo -V0 " (buffer-name))))
(add-hook 'prolog-mode-hook (lambda () (define-key prolog-mode-map (kbd "C-c e") 'prolog-eval)))

;;; Tuareg Mode (Ocaml)
(require 'tuareg)
(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))

(defun ocaml-eval ()
  (interactive)
  (shell-command (concat "ocaml " (buffer-name))))
  
(add-hook 'tuareg-mode-hook' (lambda () (define-key tuareg-mode-map (kbd "C-c e") 'ocaml-eval)))

;;; Matlab Mode
(require 'matlab)
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

(defun matlab-eval ()
  (interactive)
  (shell-command (concat "octave " (buffer-name))))

(add-hook 'matlab-mode-hook' (lambda () (define-key matlab-mode-map (kbd "C-c e") 'matlab-eval)))

;;; YAML
;(require 'yaml)
;(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml))

;;;;;;;;;;;;;;;;;;;;;;; Bash ;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x b") 'shell)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defadvice my-ansi-term (before force-bash) ; Force bash
  (interactive '("/bin/bash")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-external-programs-associations
   '(("app" . "open -a")
     ("org")
     ("emacs")
     ("f")
     ("scm")
     ("ml")
     ("m")
     ("lp")
     ("c")
     ("cpp")
     ("txt")
     ("rtf" . "open")
     ("pages" . "open")
     ("docx" . "open")
     ("rpp" . "open")
     ("xrns" . "open")
     ("jucer" . "open")
     ("pdf" . "open -a Preview")
     ("png" . "open -a Preview")
     ("jpeg" . "open -a Preview")))
 '(package-selected-packages
   '(org-bullets yaml exec-path-from-shell flycheck-vale helm-flyspell helm-core tuareg helm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
