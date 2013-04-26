;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    ;;;
;;;    INTRODUCTION    ;;;
;;;                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; I like to share buffers between my windows. As such I can't remember
; the following arguments for starting emacs that way:
; $ emacsclient -c -n -a ""
;
; For elpy integration, make sure to pull some packages from pipy:
; $ pip install elpy pep8 pyflakes rope

; On first start, copy the following lines (without prefixed ';') to *stratch*
; and eval (C-j).

; (package-refresh-contents)
; (package-install 'zenburn-theme)
; (package-install 'flycheck)
; (package-install 'elpy)


;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;;    KEYBINDS    ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;


(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-7") 'comment-or-uncomment-region-or-line)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'recompile)
(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key [f9] 'menu-bar-mode)
(windmove-default-keybindings 'super)


;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;;    SETTINGS    ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;


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
; cperl-mode
(defalias 'perl-mode 'cperl-mode)       ; use cperl mode instead of perl mode
; latex-math mode instead of latex-mode
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; set the ispell dictionary to use.
(ispell-change-dictionary "english")
;(ispell-change-dictionary "dansk")


;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;;    PACKAGES    ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;


; Initialize package
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

; M-x package-install RET zenburn-theme RET
(load-theme 'zenburn t)

; M-x package-install RET flycheck RET
(add-hook 'after-init-hook #'global-flycheck-mode)

; $ pip install elpy rope pyflakes pep8
; M-x package-install RET elpy RET
(elpy-enable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           ;;;
;;;    SOME NICE FUNCTIONS    ;;;
;;;                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       ;;;
;;;    DEPRECATED BELOW   ;;;
;;;                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; AUTOCOMPLETE ;;;
; req: http://cx4a.org/software/auto-complete/
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "/home/neo2k/.emacs.d/ac-dict")
;; (ac-config-default)
;;; /AUTOCOMPLETE ;;;


;; ;;; FLYMAKE (DEPRECATED) ;;;
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
;; ;;; /FLYMAKE (DEPRECATED) ;;;
;; ;;; FLYMAKE python -> flake8 ;;;
;;   (when (load "flymake" t) 
;;          (defun flymake-pyflakes-init () 
;;            (let* ((temp-file (flymake-init-create-temp-buffer-copy 
;;                               'flymake-create-temp-inplace)) 
;;               (local-file (file-relative-name 
;;                            temp-file 
;;                            (file-name-directory buffer-file-name)))) 
;;              (list "flake8" (list local-file)))) 

;;          (add-to-list 'flymake-allowed-file-name-masks 
;;                   '("\\.py\\'" flymake-pyflakes-init)))
;; ;;; /FLYMAKE python -> flake8 ;;;

;; ;;; FLYMAKE html -> tidy ;;;
;; (defun flymake-html-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;   	     'flymake-create-temp-inplace))
;; 	      (local-file (file-relative-name
;; 	      		        temp-file
;; 					      (file-name-directory buffer-file-name))))
;;     (list "tidy" (list local-file))))

;; (add-to-list 'flymake-allowed-file-name-masks
;; 	          '("\\.htm$\\|\\.html$\\|\\.ctp|\\.asp" flymake-html-init))

;; (add-to-list 'flymake-err-line-patterns
;; 	          '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
;; 		         nil 1 2 4))
;;; /FLYMAKE html -> tidy ;;;

;;; ROPEMACS SPECIFICS ;;;
; pymacs: https://github.com/pinard/Pymacs/zipball/master
;; (require 'pymacs)
; ropemacs: http://rope.sourceforge.net/ropemacs.html
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
;;; /ROPEMACS SPECIFICS ;;;
