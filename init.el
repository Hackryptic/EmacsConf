(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package command-log-mode)

(column-number-mode)
(global-display-line-numbers-mode t)

(global-set-key (kbd "<Hangul>") 'toggle-input-method)

;;(defun display-line-numbers-equalize ()
;;  "Equalize The width"
;;  (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))
;;(add-hook 'find-file-hook 'display-line-numbers-equalize)

;;(message "hallo")
;; ":ensure t" is nececsary for installing package automatically
(use-package better-defaults
  :ensure t)
;;(setq
;; custom-file (expand-file-name "custom.el" user-emacs-directory))
;;(message "hi")
(load custom-file)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;(use-package modus-themes
;;  :ensure t
;;  :init
  ;; Add all your customizations prior to loading the themes
;;  (setq modus-themes-italic-constructs t
;;        modus-themes-bold-constructs nil
;;        modus-themes-region '(bg-only no-extend))



  ;; Load the theme files before enabling a theme
;;  (modus-themes-load-themes)
;;  :config
  ;; Load the theme of your choice:defcustom
;;  (modus-themes-load-vivendi) ;; OR (modus-themes-load-vivendi)
;;  :bind ("<f5>" . modus-themes-toggle))

(use-package doom-themes
  :init (load-theme 'doom-gruvbox))

(set-face-attribute 'default nil
                    ;;:font "NotoSansKR Bold"
                    :font "SourceCodePro SemiBold"                   
;;                    :family "SourceCodePro-Bold"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(set-fontset-font "fontset-default" 'hangul "NotoSansKR Bold")

 (custom-set-faces
  ;;`(mode-line ((t (:background ,(doom-color 'dark-violet)))))
  `(font-lock-comment-face ((t (:foreground ,(doom-color 'medium-sea-green))))))


(use-package diminish
  :ensure t)



(use-package eshell-git-prompt
  :ensure t)

(use-package python-mode
  :ensure t
  :custom
  (python-shell-interpreter "/home/hackryptic/anaconda3/envs/contest/bin/python"
))

(use-package exec-path-from-shell
  :ensure t)

(use-package conda
  :ensure t)

(use-package eglot
  :ensure t)

(use-package eldoc-box)

(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)

(use-package company
  
  ;;:after eglot--managed-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))

  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-delay 0.1))

(use-package company-box
  ;;:after company-mode                   
  :hook (company-mode . company-box-mode))

(use-package company-jedi)

(defun my/python-mode-hook ()
  ;;(company-mode +1)
  (print "called")
  (print "found")
  (add-to-list 'company-backends 'company-jedi)
  (print "added"))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(delete 'company-jedi company-backends)

;;(add-to-list 'company-backends 'company-jedi)


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))



(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))




(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :init (projectile-mode +1)
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
 :after projectile
 :config (counsel-projectile-mode 1))

;;(which-key-mode)

;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package treemacs
  :ensure t
  :defer t
  ;;:init
  ;;(with-eval-after-load 'winum
  ;;(define-key winum-keymap (kbd "M-0") #'treemacs-select-window)
  )

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(conda-env-initialize-eshell)
(conda-env-initialize-interactive-shells)

(custom-set-variables
 '(conda-anaconda-home "/home/hackryptic/anaconda3/"))

(eshell-git-prompt-use-theme 'multiline2)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config (setq org-superstar-special-todo-items t))

(use-package ov)




(with-eval-after-load "org" (load "~/.emacs.d/preview-latex.el"))



(setq tramp-default-method "ssh")
(setq tramp-verbose 10)


(setq shell-file-name "bash")
(setq shell-command-switch "-c")

(add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))

;; (push "/home/hackryptic/anaconda3/bin" exec-path)

;;(load "~/.emacs.d/exwm-desktop.el")
 (load "~/.emacs.d/non-exwm.el")



