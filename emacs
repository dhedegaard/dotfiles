; I like to share buffers between my windows. As such I can't remember
; the following arguments for starting emacs that way:
; $ emacsclient -c -n -a ""
;
; These are the extensions I use:
;
; * auto-complete
;   URL: http://cx4a.org/software/auto-complete/
;   For auto complete in python, elisp and css.
;
; * flake8
;   URL: https://pypi.python.org/pypi/flake8 (or pip install)
;   For flymake on .py
;
; * tidy
;   URL: http://tidy.sourceforge.net/
;   For flymake on .htm/.html/.asp
;
; * ropemacs
;   URL: http://rope.sourceforge.net/ropemacs.html
;   Rope integration on Emacs, depends on ropemode and pymacs.
;
; TODO:
; * getting emacs server to run and share sessions between processes.
; * Integrate auto-complete with ropemacs.
; * yasnippet with ac.
; * rsense for ruby ac.
; * flymake for ruby.
; * flymake-cursor for displaying errors in the mini-buffer.

;;; KEYBINDS ;;;
(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-7") 'comment-or-uncomment-region-or-line)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'recompile)
(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key [f9] 'menu-bar-mode)
(windmove-default-keybindings 'super)
;;; /KEYBINDS ;;;

;;; GLOBAL SETTINGS ;;;
(setq inhibit-startup-screen t)         ; don't show startup screen
(add-to-list 'load-path "~/.emacs.d/")  ; add .emacs.d to load path
(mouse-wheel-mode t)                    ; enable mouse wheel for scrolling
(setq-default fill-column 80)           ; default to 80 chars columns
(line-number-mode 1)                    ; linenumber in buffer status line
(column-number-mode 1)                  ; columnnumber in buffer status line
(global-hl-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)           ; y/n instead of yes/no
(global-linum-mode)                     ; linenumbers in the left side
(tool-bar-mode -1)                      ; remove toolbar
(menu-bar-mode -1)                      ; remove menubar
(setq-default indent-tabs-mode nil)     ; never tabs, always spaces
; ido-mode
(setq ido-enable-flex-matching t)       ; ido: flexible matching
(setq ido-everywhere t)                 ; use ido for everything
(ido-mode 1)                            ; enable ido mode
; flymake hook on find file
(add-hook 'find-file-hook 'flymake-find-file-hook)
; cperl-mode
(defalias 'perl-mode 'cperl-mode)       ; use cperl mode instead of perl mode
; latex-math mode instead of latex-mode
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; set the ispell dictionary to use.
(ispell-change-dictionary "english")
;(ispell-change-dictionary "dansk")
;;; /GLOBAL SETTINGS ;;;

;;; THEME ;;;
; req: https://github.com/bbatsov/zenburn-emacs/raw/master/zenburn-theme.el
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)
;;; /THEME ;;;

;;; FUNCTION comment/uncomment line function ;;;
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
;;; /FUNCTION comment/uncomment line function ;;;

;;; FLYMAKE python -> flake8 ;;;
  (when (load "flymake" t) 
         (defun flymake-pyflakes-init () 
           (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                              'flymake-create-temp-inplace)) 
              (local-file (file-relative-name 
                           temp-file 
                           (file-name-directory buffer-file-name)))) 
             (list "flake8" (list local-file)))) 

         (add-to-list 'flymake-allowed-file-name-masks 
                  '("\\.py\\'" flymake-pyflakes-init)))
;;; /FLYMAKE python -> flake8 ;;;

;;; FLYMAKE html -> tidy ;;;
(defun flymake-html-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
  	     'flymake-create-temp-inplace))
	      (local-file (file-relative-name
	      		        temp-file
					      (file-name-directory buffer-file-name))))
    (list "tidy" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
	          '("\\.htm$\\|\\.html$\\|\\.ctp|\\.asp" flymake-html-init))

(add-to-list 'flymake-err-line-patterns
	          '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
		         nil 1 2 4))
;;; /FLYMAKE html -> tidy ;;;

;;; AUTOCOMPLETE ;;;
; req: http://cx4a.org/software/auto-complete/
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/neo2k/.emacs.d/ac-dict")
(ac-config-default)
;;; /AUTOCOMPLETE ;;;

;;; ROPEMACS SPECIFICS ;;;
; pymacs: https://github.com/pinard/Pymacs/zipball/master
(require 'pymacs)
; ropemacs: http://rope.sourceforge.net/ropemacs.html
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
;;; /ROPEMACS SPECIFICS ;;;

;;; RSENSE (TODO) ;;;
; req: http://cx4a.org/software/rsense/
;(setq rsense-home "/usr/lib/rsense-0.3")
;(add-to-list 'load-path (concat rsense-home "/etc"))
;(require 'rsense)
;;; /RSENSE ;;;
