;;package sources
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)

(setq packjage-archive-enable-alist '(("melpa" deft magit)))

(column-number-mode)

(add-to-list 'load-path "~/.emacs.d/mine/")
(load "hdrm-mode")
(load "pipeline-mode")

(add-to-list 'load-path "~/.emacs.d/htmlize/")
(load "htmlize")

;;install default packaged
(message "%s" "Refreshing package database...")
(package-initialize)
(when (not package-archive-contents)
    (package-refresh-contents))
(defvar my-packages     '(eimp
                          evil-leader
                          evil-paredit
                          evil-tabs
                          elscreen
                          evil
                          fsharp-mode
                          goto-chg
                          haskell-mode
                          ac-haskell-process
                          httpd
                          impatient-mode
                          flycheck
                          auto-complete
                          paredit
                          pkg-info
                          epl
                          dash
                          popup
                          pos-tip
                          project-explorer
                          es-windows
                          es-lib
                          relative-line-numbers
                          simple-httpd
                          undo-tree
                          tracking
                          shorten
                          s
                          csharp-mode
                          erlang
                          feature-mode
                          gist
                          graphviz-dot-mode
                          htmlize
                          markdown-mode
                          marmalade
                          writegood-mode
                          org
                          ;;ido
            		  helm 
                          evil-terminal-cursor-changer
                          neotree
                          restclient
                          web-mode
                          yaml-mode
                          cyberpunk-theme
			  evil-magit
			  rainbow-delimiters
			  tagedit
			  paredit
			  projectile
			  align-cljlet
                          lua-mode
                          ;purescript-mode
                          psc-ide
                          elm-mode
                          flycheck-elm
                          ;;elm-oracle
                          company

                          ;;- intero
                          ;dante
                          
                          go-mode
                          ;gocode
                          go-eldoc
                          alchemist
                          rtags
                          cmake-ide
                          ruby-mode
                          hasky-stack
                          elm-mode
                          dockerfile-mode
                          evil-org
                          find-file-in-project
                      ))

(dolist (p my-packages)
    (when (not (package-installed-p p))
          (package-install p)))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


(when (fboundp 'winner-mode)
  (winner-mode 1))

;;Auto-complete config
(ac-config-default)

;;NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(add-hook 'neotree-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;;UI defaults
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)


(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))


;;y=yes, n=no
(defalias 'yes-or-no-p 'y-or-n-p)

;;Flyspell settings
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")
(global-flycheck-mode t)

;;zsh hook
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;;2 space indent for JS mode
(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-custom)


;;markdown mode settings
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")


;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot        . t)
   (emacs-lisp . t)
   (haskell    . t)
   (org        . t)
   (perl       . t)
   (python     . t)
   (R          . t)
   (ruby       . t)
   (clojure  .t)
   (ocaml  .t)
   (js  .t)
   (scheme .t)
   (lisp .t)
   (sql .t)
   (sqlite     . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-log-done 'time)

;;EVIL mode
(add-to-list 'load-path "~/.emacs.d/evil")
(setq evil-search-module 'evil-search
  evil-want-C-u-scroll t
  evil-want-C-w-in-emacs-state t)
(require 'evil)
(evil-mode 1)

;; EVIL - esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;EVIL - colours
(set-face-attribute 'default nil :height 100)
(unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
        (evil-terminal-cursor-changer-activate) ; or (etcc-on)
            )
(setq evil-insert-state-cursor '((bar . 1) "green")
      evil-normal-state-cursor '(box "green")
      evil-emacs-state-cursor '(box "red")
      evil-visual-state-cursor '(box "orange")
      evil-replace-state-cursor '(bar "red")
      evil-operator-state-cursor '(hollow "red")
      )


;;EVIL - nav windows with hjkl
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;;EVIL - relative lin numbers on
(global-relative-line-numbers-mode t)

;;EVIL - leader key
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;;EVIL - leader keybindings
(evil-leader/set-key "bml" 'helm-bookmarks)
(evil-leader/set-key "bms" 'bookmark-set)
(evil-leader/set-key "bmd" 'bookmark-delete)
(evil-leader/set-key "g" 'magit)

;;EVIL - misc
(define-key global-map (kbd "RET") 'newline-and-indent)


;;; ORG Mode

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-src-fontify-natively t)

(setq org-agenda-files '("~/_data/org/"))


;;; bookmarks
;; save bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks"
      bookmark-save-flag 1) ;; save after every change

;;IDO
;;(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)
;;(ido-mode 1)

;;theme
;(if window-system
;    (load-theme 'solarized-light t)
;  (load-theme 'wombat t))
(load-theme 'cyberpunk t)

;;;Startup
(setq inhibit-splash-screen t)

(icomplete-mode 99)



(require 'fsharp-mode)

;;;Helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)


(setq backup-inhibited t)
(setq make-backup-files nil)



;;;C# mode
(defun my-csharp-mode-hook ()
  (electric-pair-mode 0)
  ;(c-default-style "linux")
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'statement-cont 0))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)


(setq c-basic-offset 4)
;;No tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 2
      indent-tabs-mode nil)


;; Python
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 2)
        (setq python-indent 2)))

;; ;; Clojure
;; (require 'clojure-mode)
;; (setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))
;; (setq inferior-lisp-program "lein repl")
;;  
;; ;; clj-refactor
;; (require 'clj-refactor)
;; (add-hook 'clojure-mode-hook (lambda ()
;;                                (clj-refactor-mode 1)
;;                                (cljr-add-keybindings-with-prefix "C-c C-o")))
;;  
;; ;; align-cljlet
;; (require 'align-cljlet)
;; (global-set-key (kbd "C-c C-a") 'align-cljlet)
;;  
;; ;; projectile
;; (require 'projectile)
;; (add-hook 'clojure-mode-hook 'projectile-mode)
 
;; paredit
(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)
 
;;Rainbox
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Greek chars
(global-set-key (kbd "M-g a") "α")
(global-set-key (kbd "M-g b") "β")
(global-set-key (kbd "M-g g") "γ")
(global-set-key (kbd "M-g d") "δ")
(global-set-key (kbd "M-g e") "ε")
(global-set-key (kbd "M-g z") "ζ")
(global-set-key (kbd "M-g h") "η")
(global-set-key (kbd "M-g q") "θ")
(global-set-key (kbd "M-g i") "ι")
(global-set-key (kbd "M-g k") "κ")
(global-set-key (kbd "M-g l") "λ")
(global-set-key (kbd "M-g m") "μ")
(global-set-key (kbd "M-g n") "ν")
(global-set-key (kbd "M-g x") "ξ")
(global-set-key (kbd "M-g o") "ο")
(global-set-key (kbd "M-g p") "π")
(global-set-key (kbd "M-g r") "ρ")
(global-set-key (kbd "M-g s") "σ")
(global-set-key (kbd "M-g t") "τ")
(global-set-key (kbd "M-g u") "υ")
(global-set-key (kbd "M-g f") "ϕ")
(global-set-key (kbd "M-g j") "φ")
(global-set-key (kbd "M-g c") "χ")
(global-set-key (kbd "M-g y") "ψ")
(global-set-key (kbd "M-g w") "ω")
(global-set-key (kbd "M-g A") "Α")
(global-set-key (kbd "M-g B") "Β")
(global-set-key (kbd "M-g G") "Γ")
(global-set-key (kbd "M-g D") "Δ")
(global-set-key (kbd "M-g E") "Ε")
(global-set-key (kbd "M-g Z") "Ζ")
(global-set-key (kbd "M-g H") "Η")
(global-set-key (kbd "M-g Q") "Θ")
(global-set-key (kbd "M-g I") "Ι")
(global-set-key (kbd "M-g K") "Κ")
(global-set-key (kbd "M-g L") "Λ")
(global-set-key (kbd "M-g M") "Μ")
(global-set-key (kbd "M-g N") "Ν")
(global-set-key (kbd "M-g X") "Ξ")
(global-set-key (kbd "M-g O") "Ο")
(global-set-key (kbd "M-g P") "Π")
(global-set-key (kbd "M-g R") "Ρ")
(global-set-key (kbd "M-g S") "Σ")
(global-set-key (kbd "M-g T") "Τ")
(global-set-key (kbd "M-g U") "Υ")
(global-set-key (kbd "M-g F") "Φ")
(global-set-key (kbd "M-g J") "Φ")
(global-set-key (kbd "M-g C") "Χ")
(global-set-key (kbd "M-g Y") "Ψ")
(global-set-key (kbd "M-g W") "Ω")

(global-set-key (kbd "C-M-g 0") "⊥")
(global-set-key (kbd "C-M-g t") "⊤")
(global-set-key (kbd "C-M-g a") "∀")
(global-set-key (kbd "C-M-g e") "∃")
(global-set-key (kbd "C-M-g i") "→")
(global-set-key (kbd "C-M-g b") "↔")
(global-set-key (kbd "C-M-g !") "¬")
(global-set-key (kbd "C-M-g c") "∧")
(global-set-key (kbd "C-M-g d") "∨")
(global-set-key (kbd "C-M-g x") "⊕")


;; Flycheck - automated checker module for emacs
;; import the flycheck symbols
(require 'flycheck)

(set-default 'truncate-lines t)

(put 'narrow-to-region 'disabled nil)


;;elm
(require 'elm-mode)
(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-elm))
(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

;;haskell
;;;   (require 'flycheck)
;;;   (require 'intero)
;;;   (add-hook 'haskell-mode-hook 'intero-mode)
;;;   (flycheck-add-next-checker 'intero
;;;                              '(warning . haskell-hlint))
;;;   (define-key evil-normal-state-map (kbd "<f12>") 'intero-goto-definition)
;;;   
;;;   
;;;      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      ;; https://github.com/commercialhaskell/intero/issues/569
;;;      ;;  :set -fno-defer-out-of-scope-variables
;;;      (defun intero-fix-ghci-panic ()
;;;        "Disable deferring of out of scope variable errors, which
;;;        triggers a bug in the interactive Emacs REPL printing a panic
;;;        under certain conditions."
;;;      
;;;        (interactive)
;;;        (let* ((root (intero-project-root))
;;;               (package-name (intero-package-name))
;;;               (backend-buffer (intero-buffer 'backend))
;;;               (name (format "*intero:%s:%s:repl*"
;;;                             (file-name-nondirectory root)
;;;                             package-name))
;;;               (setting ":set -fno-defer-out-of-scope-variables\n"))
;;;          (when (get-buffer name)
;;;            (with-current-buffer (get-buffer name)
;;;              (goto-char (point-max))
;;;              (let ((process (get-buffer-process (current-buffer))))
;;;                (when process (process-send-string process setting)))))))
;;;      
;;;      (advice-add 'intero-repl :after (lambda (&rest r) (intero-fix-ghci-panic))
;;;                  '((name . intero-panic-fix)))
;;;      (advice-add 'intero-repl-load :after (lambda (&rest r) (intero-fix-ghci-panic))
;;;                  '((name . intero-panic-fix)))

;; LSP
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package yasnippet
  :ensure t)
(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "ghcide")
 (setq lsp-haskell-process-args-hie '())
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;;(setq lsp-log-io t)
 (define-key evil-normal-state-map (kbd "<f12>") 'lsp-find-definition)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; (use-package dante
;   :ensure t
;   :after haskell-mode
;   :commands 'dante-mode
;   :init
;   (add-hook 'haskell-mode-hook 'dante-mode)
;   (add-hook 'haskell-mode-hook 'flycheck-mode))
; 
; (add-hook 'dante-mode-hook
;    '(lambda () (flycheck-add-next-checker 'haskell-dante
;                 '(warning . haskell-hlint))))


;; PureScript
(require 'psc-ide)

(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))


;;bash
'(sh-basic-offset 2)
'(sh-indentation 2)
'(smie-indent-basic 2)

(defun setup-sh-mode ()
  (interactive)
  (setq sh-basic-offset 2
        sh-indentation 2))
(add-hook 'sh-mode-hook 'setup-sh-mode)



;; C++
;;(require 'rtags) ;; optional, must have rtags installed
;;(cmake-ide-setup)
;;(define-key c-mode-base-map (kbd "<f12>")
;;  (function rtags-find-symbol-at-point)
;;  (require 'company)
;;  (define-key c-mode-base-map (kbd "M-.")
;;    (function rtags-find-symbol-at-point))
;;  (define-key c-mode-base-map (kbd "M-,")
;;    (function rtags-find-references-at-point))
;;  ;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
;;  ;;(define-key prelude-mode-map (kbd "C-c r") nil)
;;  ;; install standard rtags keybindings. Do M-. on the symbol below to
;;  ;; jump to definition and see the keybindings.
;;  (rtags-enable-standard-keybindings)
;;  ;; comment this out if you don't have or don't use helm
;;  (setq rtags-use-helm t)
;;  ;; company completion setup
;;  (setq rtags-autostart-diagnostics t)
;;  (rtags-diagnostics)
;;  (setq rtags-completions-enabled t)
;;  (push 'company-rtags company-backends)
;;  (global-company-mode)
;;  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;;  ;; use rtags flycheck mode -- clang warnings shown inline
;;  (require 'flycheck-rtags)
;;  ;; c-mode-common-hook is also called by c++-mode
;;  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags)
;;  )

;;(defun setup-flycheck-rtags ()
;;  (interactive)
;;  (flycheck-select-checker 'rtags)
;;  ;; RTags creates more accurate overlays.
;;  (setq-local flycheck-highlighting-mode nil)
;;  (setq-local flycheck-check-syntax-automatically nil))
;;
;;;; only run this if rtags is installed
;;(when (require 'rtags nil :noerror)
;;  ;; make sure you have company-mode installed
;;  (require 'company)
;;  (define-key c-mode-base-map (kbd "M-.")
;;    (function rtags-find-symbol-at-point))
;;  (define-key c-mode-base-map (kbd "M-,")
;;    (function rtags-find-references-at-point))
;;  ;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
;;  ;;(define-key prelude-mode-map (kbd "C-c r") nil)
;;  ;; install standard rtags keybindings. Do M-. on the symbol below to
;;  ;; jump to definition and see the keybindings.
;;  (rtags-enable-standard-keybindings)
;;  ;; comment this out if you don't have or don't use helm
;;  (setq rtags-use-helm t)
;;  ;; company completion setup
;;  (setq rtags-autostart-diagnostics t)
;;  (rtags-diagnostics)
;;  (setq rtags-completions-enabled t)
;;  (push 'company-rtags company-backends)
;;  (global-company-mode)
;;  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;;  ;; use rtags flycheck mode -- clang warnings shown inline
;;  (require 'flycheck-rtags)
;;  ;; c-mode-common-hook is also called by c++-mode
;;  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags))


;transparent
 (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
 (add-to-list 'default-frame-alist '(alpha . (85 . 50)))


;;  ;;;;;;;;;;;;;;;;;;
;;  ;;; Fira code
;;  ;; This works when using emacs --daemon + emacsclient
;;  (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;;  ;; This works when using emacs without server/client
;;  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;;  ;; I haven't found one statement that makes both of the above situations work, so I use both for now
;;  
;;  (defconst fira-code-font-lock-keywords-alist
;;    (mapcar (lambda (regex-char-pair)
;;              `(,(car regex-char-pair)
;;                (0 (prog1 ()
;;                     (compose-region (match-beginning 1)
;;                                     (match-end 1)
;;                                     ;; The first argument to concat is a string containing a literal tab
;;                                     ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
;;            '(("\\(www\\)"                   #Xe100)
;;              ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
;;              ("\\(\\*\\*\\*\\)"             #Xe102)
;;              ("\\(\\*\\*/\\)"               #Xe103)
;;              ("\\(\\*>\\)"                  #Xe104)
;;              ("[^*]\\(\\*/\\)"              #Xe105)
;;              ("\\(\\\\\\\\\\)"              #Xe106)
;;              ("\\(\\\\\\\\\\\\\\)"          #Xe107)
;;              ("\\({-\\)"                    #Xe108)
;;              ("\\(\\[\\]\\)"                #Xe109)
;;              ("\\(::\\)"                    #Xe10a)
;;              ("\\(:::\\)"                   #Xe10b)
;;              ("[^=]\\(:=\\)"                #Xe10c)
;;              ("\\(!!\\)"                    #Xe10d)
;;              ("\\(!=\\)"                    #Xe10e)
;;              ("\\(!==\\)"                   #Xe10f)
;;              ("\\(-}\\)"                    #Xe110)
;;              ("\\(--\\)"                    #Xe111)
;;              ("\\(---\\)"                   #Xe112)
;;              ("\\(-->\\)"                   #Xe113)
;;              ("[^-]\\(->\\)"                #Xe114)
;;              ("\\(->>\\)"                   #Xe115)
;;              ("\\(-<\\)"                    #Xe116)
;;              ("\\(-<<\\)"                   #Xe117)
;;              ("\\(-~\\)"                    #Xe118)
;;              ("\\(#{\\)"                    #Xe119)
;;              ("\\(#\\[\\)"                  #Xe11a)
;;              ("\\(##\\)"                    #Xe11b)
;;              ("\\(###\\)"                   #Xe11c)
;;              ("\\(####\\)"                  #Xe11d)
;;              ("\\(#(\\)"                    #Xe11e)
;;              ("\\(#\\?\\)"                  #Xe11f)
;;              ("\\(#_\\)"                    #Xe120)
;;              ("\\(#_(\\)"                   #Xe121)
;;              ("\\(\\.-\\)"                  #Xe122)
;;              ("\\(\\.=\\)"                  #Xe123)
;;              ("\\(\\.\\.\\)"                #Xe124)
;;              ("\\(\\.\\.<\\)"               #Xe125)
;;              ("\\(\\.\\.\\.\\)"             #Xe126)
;;              ("\\(\\?=\\)"                  #Xe127)
;;              ("\\(\\?\\?\\)"                #Xe128)
;;              ("\\(;;\\)"                    #Xe129)
;;              ("\\(/\\*\\)"                  #Xe12a)
;;              ("\\(/\\*\\*\\)"               #Xe12b)
;;              ("\\(/=\\)"                    #Xe12c)
;;              ("\\(/==\\)"                   #Xe12d)
;;              ("\\(/>\\)"                    #Xe12e)
;;              ("\\(//\\)"                    #Xe12f)
;;              ("\\(///\\)"                   #Xe130)
;;              ("\\(&&\\)"                    #Xe131)
;;              ("\\(||\\)"                    #Xe132)
;;              ("\\(||=\\)"                   #Xe133)
;;              ("[^|]\\(|=\\)"                #Xe134)
;;              ("\\(|>\\)"                    #Xe135)
;;              ("\\(\\^=\\)"                  #Xe136)
;;              ("\\(\\$>\\)"                  #Xe137)
;;              ("\\(\\+\\+\\)"                #Xe138)
;;              ("\\(\\+\\+\\+\\)"             #Xe139)
;;              ("\\(\\+>\\)"                  #Xe13a)
;;              ("\\(=:=\\)"                   #Xe13b)
;;              ("[^!/]\\(==\\)[^>]"           #Xe13c)
;;              ("\\(===\\)"                   #Xe13d)
;;              ("\\(==>\\)"                   #Xe13e)
;;              ("[^=]\\(=>\\)"                #Xe13f)
;;              ("\\(=>>\\)"                   #Xe140)
;;              ("\\(<=\\)"                    #Xe141)
;;              ("\\(=<<\\)"                   #Xe142)
;;              ("\\(=/=\\)"                   #Xe143)
;;              ("\\(>-\\)"                    #Xe144)
;;              ("\\(>=\\)"                    #Xe145)
;;              ("\\(>=>\\)"                   #Xe146)
;;              ("[^-=]\\(>>\\)"               #Xe147)
;;              ("\\(>>-\\)"                   #Xe148)
;;              ("\\(>>=\\)"                   #Xe149)
;;              ("\\(>>>\\)"                   #Xe14a)
;;              ("\\(<\\*\\)"                  #Xe14b)
;;              ("\\(<\\*>\\)"                 #Xe14c)
;;              ("\\(<|\\)"                    #Xe14d)
;;              ("\\(<|>\\)"                   #Xe14e)
;;              ("\\(<\\$\\)"                  #Xe14f)
;;              ("\\(<\\$>\\)"                 #Xe150)
;;              ("\\(<!--\\)"                  #Xe151)
;;              ("\\(<-\\)"                    #Xe152)
;;              ("\\(<--\\)"                   #Xe153)
;;              ("\\(<->\\)"                   #Xe154)
;;              ("\\(<\\+\\)"                  #Xe155)
;;              ("\\(<\\+>\\)"                 #Xe156)
;;              ("\\(<=\\)"                    #Xe157)
;;              ("\\(<==\\)"                   #Xe158)
;;              ("\\(<=>\\)"                   #Xe159)
;;              ("\\(<=<\\)"                   #Xe15a)
;;              ("\\(<>\\)"                    #Xe15b)
;;              ("[^-=]\\(<<\\)"               #Xe15c)
;;              ("\\(<<-\\)"                   #Xe15d)
;;              ("\\(<<=\\)"                   #Xe15e)
;;              ("\\(<<<\\)"                   #Xe15f)
;;              ("\\(<~\\)"                    #Xe160)
;;              ("\\(<~~\\)"                   #Xe161)
;;              ("\\(</\\)"                    #Xe162)
;;              ("\\(</>\\)"                   #Xe163)
;;              ("\\(~@\\)"                    #Xe164)
;;              ("\\(~-\\)"                    #Xe165)
;;              ("\\(~=\\)"                    #Xe166)
;;              ("\\(~>\\)"                    #Xe167)
;;              ("[^<]\\(~~\\)"                #Xe168)
;;              ("\\(~~>\\)"                   #Xe169)
;;              ("\\(%%\\)"                    #Xe16a)
;;              ;;("\\(x\\)"                     #Xe16b)
;;              ("[^:=]\\(:\\)[^:=]"           #Xe16c)
;;              ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
;;              ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))
;;  
;;  (defun add-fira-code-symbol-keywords ()
;;    (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))
;;  
;;  (add-hook 'prog-mode-hook
;;            #'add-fira-code-symbol-keywords)
;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;
;;; Default font
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (set-face-attribute 'default nil
                      ;;:family "Inconsolata"
                      :family "Fira Code Retina"
                      ;:family "Iosevka"
                      ;;:family "Hasklig"
                      :height 110 ;; 110
                      :weight 'normal
                      :width 'normal)

  (when (functionp 'set-fonts-ont)
    (set-fontset-font "fontset-default"
                      'unicode
                      ;;(font-spec :family "DejaVu Sans Mono" ;;
                      ;;(font-spec :family "Fira Code Symbol" ;;
                      (font-spec :family "Fira Code Retina" ;;
                      ;(font-spec :family "Iosevka" ;;
                      ;;(font-spec :family "Hasklig" ;;
                                 :width 'normal
                                 :size 8
                                 :weight 'normal))))
;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" ".stack-work" "node_modules" "deps")))
 '(package-selected-packages
   (quote
    (find-file-in-project lsp-haskell lsp-ui lsp-mode evil-org dockerfile-mode helm-mode-manager highlight-indent-guides adoc-mode asciidoc mmm-mode dante hasky-stack evil-magit ledger-mode hledger-mode yaml-mode writegood-mode web-mode tracking tagedit stack-mode shorten sauron restclient relative-line-numbers rainbow-delimiters purescript-mode psc-ide projectile project-explorer neotree marmalade markdown-mode magit lua-mode intero impatient-mode httpd helm graphviz-dot-mode go-eldoc gist ggo-mode fsharp-mode flycheck-elm feature-mode evil-terminal-cursor-changer evil-tabs evil-paredit evil-leader erlang elm-mode eimp cyberpunk-theme coffee-mode clojure-mode-extra-font-locking clj-refactor align-cljlet alchemist ac-haskell-process))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;(global-set-key (kbd "<next> h e") #'hasky-stack-execute)
;;(global-set-key (kbd "<next> h i") #'hasky-stack-new)


;;; Magit
(require 'evil-magit)
;; Protect against accident pushes to upstream
(defadvice magit-push-current-to-upstream
    (around my-protect-accidental-magit-push-current-to-upstream)
  "Protect against accidental push to upstream.

Causes `magit-git-push' to ask the user for confirmation first."
  (let ((my-magit-ask-before-push t))
    ad-do-it))

(defadvice magit-git-push (around my-protect-accidental-magit-git-push)
  "Maybe ask the user for confirmation before pushing.

Advice to `magit-push-current-to-upstream' triggers this query."
  (if (bound-and-true-p my-magit-ask-before-push)
      ;; Arglist is (BRANCH TARGET ARGS)
      (if (yes-or-no-p (format "Push %s branch upstream to %s? "
                               (ad-get-arg 0) (ad-get-arg 1)))
          ad-do-it
        (error "Push to upstream aborted by user"))
    ad-do-it))

(ad-activate 'magit-push-current-to-upstream)
(ad-activate 'magit-git-push)



(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq evil-want-C-i-jump nil)


(defun ffip-diff-mode-hook-setup ()
    (evil-local-set-key 'normal "p" 'diff-hunk-prev)
    (evil-local-set-key 'normal "n" 'diff-hunk-next)
    (evil-local-set-key 'normal "P" 'diff-file-prev)
    (evil-local-set-key 'normal "N" 'diff-file-next)
    (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
    (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)
