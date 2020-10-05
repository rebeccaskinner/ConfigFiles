;;; init --- Emacs configuration
;;; provide (init)
;;; Commentary:

;;; Code:
(require 'cl)

;; disable the menu bar
(menu-bar-mode -1)

;; Disable the splash screen
(setq inhibit-splash-screen t)

(defun emacs-cfg-dir ()
  "Emacs-cfg-dir set the config directory that Emacs to use."
  (if (boundp 'user-emacs-directory)
      user-emacs-directory
    (concat (getenv "HOME") "/.emacs.d/")
    )
  )

;(set-frame-parameter (selected-frame) 'alpha '(90 . 70))
;(add-to-list 'default-frame-alist '(alpha . (90 . 70)))

;; Set the default browser to firefox
(setq browse-url-browser-function 'browse-url-firefox)

;; Add "~/.emacs.d/lisp/" to the load path
(add-to-list 'load-path (concat
                         (emacs-cfg-dir)
                         (convert-standard-filename "lisp/")
                         ))
(add-to-list 'load-path "/Users/rebecca/repos/emacs/distel/elisp")
(add-to-list 'load-path "/Users/rebecca/repos/emacs/")

; (require 'cfg-pkg-deps)
; (setq cfg-pkg-installed-package-list (concat user-emacs-directory "installed-packages"))
; (setq cfg-pkg-installed-package-list (concat (emacs-cfg-dir) "installed-packages"))

;; packages
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")
                         ("melpa"        . "http://melpa.org/packages/")))
(package-initialize)

;; improve look and fee on macos
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(background-color . "#2a2e38"))

;; global custom commands
(require 'calendar)
(defun timestamp ()
 "Insert a timestamp."
   (interactive)
   (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))

;; Show the current time in the modeline
(display-time-mode 1)

(use-package direnv
  :config
  (direnv-mode))

;; enable emoji
; (add-hook 'after-init-hook #'global-emojify-mode)

(defun setup-global-keybindings()
  "Setup global keybindings."
  (global-set-key (kbd "M-P") 'ace-window)
  (global-set-key (kbd "<M-up>") 'ace-window)
  (global-set-key (kbd "C-'") 'goto-last-change)
  (global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-M-r") 'isearch-backward-regexp)
  )

(setup-global-keybindings)

(defun configure-temp-files()
  "Set the auto-save and backup files to /tmp/"
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  )

(configure-temp-files)

(defun require-package (package)
  "Install given PACKAGE."
  (setq-default highlight-tabs t)
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(setq-default indent-tabs-mode nil)

(defun add-to-paths (path)
  "Add PATH to the current Emacs search path."
  (setenv "PATH" (concat path (concat ":" (getenv "PATH"))))
  (add-to-list 'exec-path path)
  )

(defun make-home-path (path)
  "Make a PATH string relative to the current users home directory."
  (concat (getenv "HOME") (concat "/" path))
  )

(defun update-path ()
  "Add several useful paths to the default searchpath."
  (add-to-paths (make-home-path ".cabal/bin"))
  (add-to-paths (make-home-path ".gem/ruby/2.1.0/bin"))
  (add-to-paths "/usr/local/bin")
  (add-to-paths "/usr/local/go/bin")
  (add-to-paths (make-home-path ".local/bin"))
  (add-to-paths (make-home-path "go/bin"))
  (add-to-paths "/usr/local/texlive/2017/bin/x86_64-darwin/")
  (add-to-paths (make-home-path ".rbenv/shims"))
  (add-to-paths (make-home-path "go/bin"))
  (add-to-paths (make-home-path "bin"))
  (add-to-paths "/nix/var/nix/profiles/default/bin")
  (add-to-paths (make-home-path ".nix-profile/bin"))
  )

(defun update-eshell-path ()
  "Add several useful paths to the default searchpath."
  )

(update-path)

(defun ansible-editing ()
  "Add custom ansible hooks."
  (interactive)
  (ansible)
  (setq ansible::vault-password-file (make-home-path ".vault-pass"))
  (local-set-key (kbd "C-c C-b") 'ansible::decrypt-buffer)
  (local-set-key (kbd "C-c C-g") 'ansible::encrypt-buffer)
  )

(add-hook 'ansible-mode-hook 'ansible-editing)

;; Turn on visual line-wrapping mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'tex-mode-hook 'turn-on-visual-line-mode)

(defun enable-orgmode-graphiz-execution()
  "Enable dot rendering and editing with org-bable."
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (haskell . t)
     (shell . t)
     )) ; this line activates dot
  )

(defun enable-orgmode-ditaa-execution()
  "Enable ditaa rendering and editing with org-bable."
  (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")
  (org-babel-do-load-languages
   'org-bable-load-languages
   '((ditaa . t))) ; activate ditaa
  )

(defun enable-orgmode-inline-preview()
  (setq org-start-with-inline-images t)
  )

(defun enable-orgmode-ruby-execution()
  "Enable ruby rendering and execution with org-bable."
  (require 'ob-ruby)
  )

(defun enable-org-reveal()
  (require 'ox-reveal)
  (setq org-reveal-root (make-home-path "projects/reveal.js"))
  )

(add-hook 'org-mode-hook 'enable-org-reveal)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'enable-orgmode-graphiz-execution)
(add-hook 'org-mode-hook 'enable-orgmode-ditaa-execution)
(add-hook 'org-mode-hook 'enable-orgmode-inline-preview)
(add-hook 'org-mode-hook 'enable-orgmode-ruby-execution)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight semi-bold :heightf 140 :width normal :height 102))))
 '(go-test--error-face ((t (:foreground "dark red"))))
 '(go-test--ok-face ((t (:foreground "dark green"))))
 '(go-test--pointer-face ((t (:foreground "dark blue"))))
 '(go-test--standard-face ((t (:foreground "black"))))
 '(go-test--warning-face ((t (:foreground "dark yellow")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("f633d825e380caaaefca46483f7243ae9a663f6df66c5fad66d4cab91f731c86" "ffca7ac44bfe9d585363f6bbf29f19529de216f85dce7a831dfc28883959ec05" "adde823697efd8b7532eb0406b60903310979b334e431f35d282399d6655512e" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "776a586a87389c6184c66a7b93a1eced8b04c9769c71e47421ba6893e0de0948" "bb2fd25d5f0cea2327bd5377556e5d2ae575b7a7c218ca1ec1114dd31e7b5504" "cf1b0cfbe1708be0a24242f24e974e3af22660a7099c425a2d9c5f39c5d791d9" "ff59cdca8a044e205c093017e0da9b6b4b7a12f03108fef709f87bb959507e55" "33dad529287820c29b9589c5100e69749bb81bf9d509799ba2107760ec0fbf97" "107693012b59b3a94faa869756333b8fe7224670f762ce97eb1dda89f03f5bcd" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" "ee89863f86247942d9fc404d47b2704475b146c079c1dcd2390d697ddfa9bdf4" "55baf0e5235a0268ea0b9b32f7099eb5e85a8e347fa63d6e2c9d6046362e1efb" "f6311cfb637f0e4fdc56576c06c9fb695dd5336a78e704d6733ca9489ad66595" "8b313e1793da427e90c034dbe74f3ad9092ac291846c0f855908c42a6bda1ff4" "c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" "3c06231f8aa4ad2ebc07d70ade7a1d310cc2adab02251c77a1882787e30f8394" "3d5720f488f2ed54dd4e40e9252da2912110948366a16aef503f3e9e7dfe4915" "0ee3fc6d2e0fc8715ff59aed2432510d98f7e76fe81d183a0eb96789f4d897ca" "e83c94a6bfab82536cef63610ec58d08dfddd27752d860763055daf58d028aad" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "61d3bac1f6ac6fa6df27ca63f5f8ad5a17a911d836b49cb6bdda327fedc04abf" "87073e92c4437df15f127e18cb05b2832c99689201c4d81dee3c20c7197d62e7" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "0c387e27a3dd040b33c6711ff92e13bd952369a788eee97e4e4ea2335ac5528f" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "1f3113447a652b8436a9938bbac71ecaf022cc73ecd0d76182eb9713aa781f17" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "551596f9165514c617c99ad6ce13196d6e7caa7035cea92a0e143dbe7b28be0e" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "4bfced46dcfc40c45b076a1758ca106a947b1b6a6ff79a3281f3accacfb3243c" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" default)))
 '(dante-methods-alist
   (quote
    ((new-impure-nix dante-cabal-new-nix
                     ("nix-shell" "--run"
                      (concat "cabal new-repl "
                              (or dante-target
                                  (dante-package-name)
                                  "")
                              " --builddir=dist/dante")))
     (new-nix dante-cabal-new-nix
              ("nix-shell" "--pure" "--run"
               (concat "cabal new-repl "
                       (or dante-target
                           (dante-package-name)
                           "")
                       " --builddir=dist/dante")))
     (nix dante-cabal-nix
          ("nix-shell" "--pure" "--run"
           (concat "cabal repl "
                   (or dante-target "")
                   " --builddir=dist/dante")))
     (impure-nix dante-cabal-nix
                 ("nix-shell" "--run"
                  (concat "cabal repl "
                          (or dante-target "")
                          " --builddir=dist/dante")))
     (new-build "cabal.project.local"
                ("cabal" "new-repl"
                 (or dante-target
                     (dante-package-name)
                     nil)
                 "--builddir=dist/dante"))
     (nix-ghci
      #[257 "\300\301\302#\207"
            [directory-files t "shell.nix\\|default.nix"]
            5 "

(fn D)"]
      ("nix-shell" "--pure" "--run" "ghci"))
     (stack "stack.yaml"
            ("stack" "repl" dante-target))
     (mafia "mafia"
            ("mafia" "repl" dante-target))
     (bare-cabal
      #[257 "\300\301\302#\207"
            [directory-files t "..cabal$"]
            5 "

(fn D)"]
      ("cabal" "repl" dante-target "--builddir=dist/dante"))
     (bare-ghci
      #[257 "\300\207"
            [t]
            2 "

(fn _)"]
      ("ghci")))))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(display-time-mode t)
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(fci-rule-color "#eeeeee")
 '(fringe-mode 10 nil (fringe))
 '(gnus-logo-colors (quote ("#259ea2" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(org-agenda-files (quote ("~/Desktop/interviews.org")))
 '(org-trello-current-prefix-keybinding "C-c o")
 '(package-selected-packages
   (quote
    (shakespeare-mode pdf-tools use-package dante dhall-mode direnv proof-general rebecca-theme ox-gfm nix-buffer nix-mode nix-sandbox nix-update e2wm erlang flycheck-rebar3 ac-R ess ess-R-data-view ess-smart-equals ess-smart-underscore ess-view hive protobuf-mode restclient company-coq company-erlang company-inf-ruby company-terraform helm-company clues-theme creamsody-theme darcula-theme faff-theme hemera-theme hydandata-light-theme iodine-theme kaolin-themes cherry-blossom-theme autumn-light-theme bliss-theme bubbleberry-theme alect-themes auctex-latexmk slime-volleyball steam typing edbi-sqlite emacsql format-sql tsql edbi sql-indent inf-ruby enh-ruby-mode flycheck-swift3 swift3-mode coffee-mode highlight-indent-guides ample-zen-theme arjen-grey-theme atom-one-dark-theme eziam-theme pyenv-mode auto-virtualenvwrapper python-environment virtualenv ac-python cucumber-goto-step elpy feature-mode flycheck-pycheckers flycheck-pyflakes flymake-python-pyflakes importmagic py-autopep8 py-import-check flyspell-correct-helm helm helm-codesearch helm-commandlinefu helm-cscope helm-flymake helm-flyspell helm-grepint helm-helm-commands helm-hoogle helm-idris helm-lobsters helm-org-rifle edit-indirect auctex go-guru flycheck-purescript psci purescript-mode js-comint ob-typescript ts-comint typescript-mode spacemacs-theme svg-clock ox-reveal auto-virtualenv csv-mode idris-mode abyss-theme afternoon-theme ample-theme assemblage-theme atom-dark-theme badger-theme goto-last-change yaml-mode save-packages rustfmt ruby-guard realgud rainbow-delimiters python-mode paredit ox-pandoc ox-asciidoc org-trello org-plus-contrib ob-restclient ob-http ob-go ob-diagrams markdown-mode+ man-commands magit-find-file magit-filenotify latex-preview-pane latex-pretty-symbols latex-extra kerl json-mode intero inf-clojure hippie-namespace hippie-exp-ext helm-google helm-go-package helm-ghc helm-flycheck helm-ack hcl-mode grapnel graphviz-dot-mode go-stacktracer go-scratch go-rename go-playground-cli go-playground go-gopath go-errcheck go-eldoc go-dlv go-direx go-complete go-autocomplete ghci-completion gh-md flycheck-rust flycheck-haskell flycheck-gometalinter flycheck-elm fill-column-indicator expand-region etags-select eshell-manual ert-runner ert-modeline ert-expectations ert-async elm-mode color-theme-twilight color-theme-solarized color-theme-emacs-revert-theme cmake-mode closure-lint-mode clojure-quick-repls clojure-mode-extra-font-locking clojure-here clojure-env clojure-cheatsheet cider-spy cider-profile cider-eval-sexp-fu cider-decompile cdlatex cargo buffer-extension auto-indent-mode auto-complete-etags auto-complete-c-headers ascii ansible-doc ansible align-cljlet ace-window ac-math ac-helm ac-etags ac-cider ac-c-headers)))
 '(pos-tip-background-color "#1A3734")
 '(pos-tip-foreground-color "#FFFFC8")
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343")
 '(safe-local-variable-values
   (quote
    ((dante-project-root "/home/rebecca/projects/streamtest"))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#eeeeee")
 '(vc-annotate-color-map
   (quote
    ((20 . "#dd5542")
     (40 . "#CC5542")
     (60 . "#fb8512")
     (80 . "#baba36")
     (100 . "#bdbc61")
     (120 . "#7d7c61")
     (140 . "#6abd50")
     (160 . "#6aaf50")
     (180 . "#6aa350")
     (200 . "#6a9550")
     (220 . "#6a8550")
     (240 . "#6a7550")
     (260 . "#9b55c3")
     (280 . "#6CA0A3")
     (300 . "#528fd1")
     (320 . "#5180b3")
     (340 . "#6380b3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

;; Rainbow Delimiters
(require 'rainbow-delimiters)

;;; Setup Fill-Mode
(require 'fill-column-indicator)

;; Visual fci config
(setq fci-rule-width 1)
(setq fci-rule-color "darkgrey")

;; Turn on fci mode by default
(add-hook 'after-init-hook 'fci-mode)
(add-hook 'cmake-mode 'rainbow-delimiters-mode)

(defun soft-wrap-config (&optional width)
  "Configure soft-wrap to WIDTH columns of text, and set a visual fill column at the boundry."
  (unless width (setq width 80))
  (set-fill-column width)
  (fci-mode 1)
  (auto-fill-mode -1)
  (turn-on-visual-line-mode)
  )

(defun enable-expand-region ()
  "Configures the 'expand-region' command for development modes."
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

;; (defun line-nums ()
;;   "Enable global line numbers for programming modes."
;; ;  (global-linum-mode 1)
;;   (add-hook 'prog-mode-hook 'linum-on)
;;   (setq linum-format "%4d \u2502 ")
;;   )

;; mode specific configs
(defun default-programming-config ()
  "Configure some sane defaults shared across various programming-related major modes."
  (auto-fill-mode 1)
  (auto-complete-mode 1)
  (rainbow-delimiters-mode 1)
  (fci-mode 1)
  (set-fill-column 80)
  (enable-expand-region)
;  (line-nums)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (setq tab-width 2)
  (global-set-key (kbd "C-)") 'forward-sexp)
  (global-set-key (kbd "C-(") 'backward-sexp)
  )

(defun my-dhall-mode-config ()
  "Configure basic settings when editing in dhall-mode."
  (default-programming-config)
  )

(add-hook 'dhall-mode 'my-dhall-mode-config)

(defun my-nix-mode-config ()
  "Configure defaults for editing nix expressions."
  (default-programming-config)
  )

(add-hook 'nix-mode 'my-nix-mode-config)

;; Extra functions for pml mode
(defun pml-mode-tools()
  "The pml-mode-tools enable some extra functions to make it nicer to edit PML."
  (defvar tag-contents-history '())
  (defvar tag-name-history '())
  (defvar code-block-history '())
  (defvar method-name-history '())
  (interactive)
  (defun insert-tag-with-value(tag val)
    (insert (format "<%s>%s</%s>" tag val tag))
    )
  (defun make-tag()
    (interactive)
    "The make-tag function gets a tag name and value and inserts the tag."
    (let ((tag (read-string "tag: " nil 'tag-name-history )))
      (add-to-history 'tag-name-history tag)
      (let ((contents (read-string "contents: " nil 'tag-contents-history )))
        (add-to-history 'tag-contents-history contents)
        (insert-tag-with-value tag contents)
        )
      )
    )
  (defun insert-code-block-without-contents(lang)
    (insert (format "{:language=\"%s\"}" lang))
    (newline-and-indent)
    (insert "~~~")
    (newline-and-indent)
    (insert "~~~")
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    )
  (defun insert-code-block-with-contents(lang contents)
    (insert-code-block-without-contents lang)
    (insert contents)
    (next-line)
    (end-of-line)
    (newline-and-indent)
    )

  (defun add-objc-method ()
    "Add an objcmethod tag."
    (interactive)
    (let ((method (read-string "method: " nil 'method-name-history)))
      (insert-tag-with-value "objcmethod" method)
      )
    )


  (defun add-code-block ()
    "Add a code block without spawning a mini-window."
    (interactive)
    (let ((lang (read-string "language: " nil 'code-block-history)))
      (add-to-history 'code-block-history lang)
      (insert-code-block-without-contents lang)
      )
    )

  (global-set-key (kbd "C-\"") "“")
  (global-set-key (kbd "M-\"") "”")
  (global-set-key (kbd "C-c t") 'make-tag)
  (global-set-key (kbd "C-c b") 'add-code-block)
  (global-set-key (kbd "C-c m") 'add-objc-method)
  )

;; Configure Coffee Mode for coffeescript

(defun my-coffee-mode-hook()
  "Configure some defaults for working with coffeescript."
  (defun my-coffee-indent-line ()
    "Indent current line as CoffeeScript."
    (setq coffee-tab-width 2)
    (interactive)
    (if (= (point) (point-at-bol))
        (insert-tab)
      (save-excursion
        (let ((prev-indent 0) (cur-indent 0))
          (setq prev-indent (coffee-previous-indent))
          (setq cur-indent (current-indentation))
          (beginning-of-line)
          (insert-tab)
          (when (= (point-at-bol) (point))
            (forward-char coffee-tab-width))
          (when (> (- (current-indentation) prev-indent) coffee-tab-width)
            (backward-to-indentation 0)
            (delete-region (point-at-bol) (point)))))))
  (default-programming-config)
  (setq indent-line-function 'my-coffee-indent-line)
  )
(add-hook 'coffee-mode-hook 'my-coffee-mode-hook)

(defun typescript-config()
  "Configuration for working with typescript."
  (default-programming-config)
  )
(add-to-list 'auto-mode-alist '(".tsx" . typescript-mode))
(add-hook 'typescript-mode-hook 'typescript-config)

(defun purescript-config()
  "Configuration for working with purescript."
  (default-programming-config)
  )

;; emacs lisp mode configuration
(defun elisp-config ()
  "Configuration for elisp-mode."
  (default-programming-config)
  )

(add-hook 'emacs-lisp-mode-hook 'elisp-config)

(defun hcl-mode-config()
  (defun hclfmt()
    (interactive)
    (setq p (point))
    (shell-command-on-region (point-min) (point-max)  "hclfmt" nil t)
    (goto-char p)
    )
  (defun hcl-save-hook()
    (when (eq major-mode 'hcl-mode)
      (hclfmt)
      )
    )
  (add-hook 'before-save-hook 'hcl-save-hook)
  (global-set-key (kbd "C-<tab>") 'hclfmt)
  )

(add-hook 'hcl-mode-hook 'hcl-mode-config)

(defun json-mode-config ()
  "Configuration for JSON-mode."
  (rainbow-delimiters-mode)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (window-margin-mode)
  )

;; Configure go for eshell
;; Set GOPATH in the emacs environment
(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "GOROOT" "/usr/local/go")

(defun my-javascript-mode-hook ()
  "Configuration for javascript."
  (default-programming-config)
  (setq js-indent-level 2)
  )

(add-hook 'javascript-mode-hook 'my-javascript-mode-hook)

;; Python Mode

(defun eshell-python-mode ()
  "Configure eshell to work with the python virtual environment at PATH."
  ;;; see https://github.com/porterjamesj/virtualenvwrapper.el
  (require 'virtualenvwrapper)
  (venv-initialize-eshell)
  (venv-set-location)
  (venv-workon)
  )

(defun my-python-mode-hook ()
  "Configure settings for python."
  (default-programming-config)
  (highlight-indentation-mode)
  (set-face-background 'highlight-indentation-face "#444466")
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; configure elm-mode
(defun elm-mode-config ()
  "Configure settings when programming elm."
  (add-hook 'flycheck-mode-hook 'flycheck-elm-setup)
  (default-programming-config)
  )
(add-hook 'elm-mode-hook 'elm-mode-config)

;; Go Mode
(defun my-go-mode-hook ()
  "Use goimports instead of go-fmt."
  (default-programming-config)
  (soft-wrap-config)

  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  (local-set-key (kbd "C-<tab>") 'gofmt-before-save)

  ;; Add $GOPATH/bin/ to the search path for executable binaries
  (add-to-paths (make-home-path "go/bin"))

  (local-set-key (kbd "M-.") 'godef-jump)

  ;; setup autocomplete for go
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default)

  ;; Set the tab-width to something reasonable
  (setq tab-width 2)

  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")

  ; Use C-<return> to go-run the current file outside of go-playground files
  (local-set-key (kbd "C-<return>") 'go-run)

  ; Use 'C-Shift-<return>' to run tests on the current file
  (local-set-key (kbd "C-S-<return>") 'go-test-current-file)

  (defun go-if-err()
    "Insert golang error handling boilerplate."
    (interactive)
    (insert "if err != nil { return nil, err }")
    (reindent-then-newline-and-indent)
    )

  (defun go-mode-new-test(name)
    "Create a new test called NAME."
    (insert (format "// %s runs a test" name))
    (reindent-then-newline-and-indent)
    (insert (format "func %s (t *testing.T) {" name))
    (reindent-then-newline-and-indent)
    (insert "t.Parallel()")
    (reindent-then-newline-and-indent)
    (reindent-then-newline-and-indent)
    (insert "}")
    (reindent-then-newline-and-indent)
    (previous-line)
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    )

  (defun go-mode-new-subtest(description)
    "Use t.Run to create a new subtest with DESCRIPTION."
    (insert (format "t.Run(\"%s\", func (t *testing.T) {" description))
    (reindent-then-newline-and-indent)
    (insert "t.Parallel()")
    (reindent-then-newline-and-indent)
    (reindent-then-newline-and-indent)
    (insert "})")
    (reindent-then-newline-and-indent)
    (previous-line)
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    )

  (defun new-test()
    "Get a test name and insert it."
    (interactive)
    (let ((name (read-string "Test Name: ")))
      (go-mode-new-test name))
    )

  (defun new-sub-test()
    "Get a subtest description and insert it."
    (interactive)
    (let ((desc (read-string "Description: ")))
      (go-mode-new-subtest desc))
    )

  (global-set-key (kbd "C-c t") 'new-test)
  (global-set-key (kbd "C-c r") 'new-sub-test)
  (global-set-key (kbd "C-c e") 'go-if-err)
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-playground-mode 'my-go-mode-hook)

;; Ruby Mode

(defun turn-on-enhanced-ruby-mode ()
  "Enable enh-ruby-mode and add some configuration options."
  (require 'enh-ruby-mode)
  (add-hook 'enh-ruby-mode-hook 'fci-mode)
  (add-hook 'enh-ruby-mode-hook 'turn-on-auto-fill)
  (add-hook 'enh-ruby-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'enh-ruby-mode-hook 'auto-complete-mode)
  )

;; (defun configure-inf-ruby ()
;;  "Use Pry instead of irb as the REPL for inferior ruby mode."
;;  (setq inf-ruby-default-implementation "pry")
;;  )

(defun ruby-config ()
  "Setup 'ruby-mode` and enh-ruby-mode parameters for ruby editing."
  (default-programming-config)
;  (configure-inf-ruby)
  (turn-on-enhanced-ruby-mode)
  )

(add-hook 'ruby-mode-hook 'ruby-config)

(add-hook 'json-mode-hook 'json-mode-config)

(defun my-markdown-mode-hook ()
  "Add some nice extensions for dealing with various markdown modes."

  (defun hugo-extras ()
    "Add a bunch of extra functions for hugo-specific markdown."

    (defun insert-relative-link (name to)
      "Inserts a relative link called NAME to the section named TO."
      (insert (format "[%s]({{<relref \"#%s\">}})" name to))
      )

    (defun rel-link ()
      "Query the user for a link name and section heading, then insert a
      relative link."
      (interactive)
      (let ((name (read-string "Link Name: ")))
        (let ((to (read-string "Link To: ")))
          (insert-relative-link name to9)
          )
        )
      )
    )
  )

(add-hook 'markdown-mode-hook 'default-programming-config)
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

;; add sql-indent when loading sql files
(eval-after-load "sql"
  '(load-library "sql-indent"))


(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo `find-tag'."
  "If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run `etags' on all peer files in current dir and reload them silentlyf, \
if EXTENSION is specified, use it for refreshing etags, or default to .el."

  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

(defun create-tags(format)
  (eshell-command
   (format "find %s -type f -name \"%s\" | etags -" (pwd) format)
   )
  )

;;  (defun my-correct-symbol-bounds (pretty-alist)
;;     "Prepend a TAB character to each symbol in this alist,
;; this way compose-region called by prettify-symbols-mode
;; will use the correct width of the symbols
;; instead of the width measured by char-width."
;;     (mapcar (lambda (el)
;;               (setcdr el (string ?\t (cdr el)))
;;               el)
;;             pretty-alist))

;; (defun my-ligature-list (ligatures codepoint-start)
;;   "Create an alist of strings to replace with
;; codepoints starting from codepoint-start."
;;   (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
;;     (-zip-pair ligatures codepoints)))

;;                                         ; list can be found at https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
;; (setq my-hasklig-ligatures
;;       (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
;;                      "==" "===" "==>" "=>" "=<<" "!!" ">>"
;;                      ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
;;                      "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
;;                      "<<" "<<<" "<+>" ".." "..." "++" "+++"
;;                      "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->"
;;                      )))
;;         (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

;; ;; nice glyphs for haskell with hasklig
;; (defun my-set-hasklig-ligatures ()
;;   "Add hasklig ligatures for use with prettify-symbols-mode."
;;   (setq prettify-symbols-alist
;;         (append my-hasklig-ligatures prettify-symbols-alist))
;;   (prettify-symbols-mode))



(defun my-haskell-mode-hooks()
  (defun hsfmt()
    "Apply stylish-haskell to the current buffer."
    (interactive)
    (defvar-local p (point))
    (shell-command-on-region (point-min) (point-max)  "stylish-haskell" nil t)
    (goto-char p)
    )
  (defun hs-save-hook()
    (when (eq major-mode 'haskell-mode)
      (hsfmt)
      )
    )
  (global-set-key (kbd "C-<tab>") 'hsfmt)
  )

;; Haskell Mode
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  )

(add-hook 'haskell-mode-hook 'my-haskell-mode-hooks)

;; Setup haskell-cabal-mode
(defun my-haskell-cabal-mode()
  (defun hsfmt()
    "Apply stylish-cabal to the current buffer."
    (interactive)
    (defvar-local p (point))
    (shell-command-on-region (point-min) (point-max)  "stylish-cabal" nil t)
    (goto-char p)
    )
  (defun hs-save-hook()
    (when (eq major-mode 'haskell-mode)
      (hsfmt)
      )
    )
  (global-set-key (kbd "C-<tab>") 'hsfmt)
  )
(add-hook 'haskell-cabal-mode-hook 'my-haskell-cabal-mode)

(defun my-rust-mode-hook()
  (defun rsfmt()
    "Call rustfmt."
    (interactive)
    (setq p (point))
    (shell-command-on-region (point-min) (point-max)  "rustfmt" nil t)
    (goto-char p)
    )
  (defun rust-save-hook()
    (when (eq major-mode 'rust-mode)
      (rsfmt)
      )
    )
  (add-hook 'before-save-hook 'rust-save-hook)
  (local-set-key (kbd "C-<tab>") 'rsfmt)
  (default-programming-config)
  (setq rust-indent-offset 2)
  )

(add-hook 'rust-mode-hook 'my-rust-mode-hook)

;; Speedbar Configuration
(require 'speedbar)
;; Enable speedbar for haskell files
(speedbar-add-supported-extension ".hs")

;; TeX Mode
(defun beamer-utils()
  (defun beamer-new-frame(name)
  (insert "\\begin{frame}")
  (reindent-then-newline-and-indent)
  (insert "\\frametitle{")
  (insert name)
  (insert "}")
  (reindent-then-newline-and-indent)
  (insert "\\end{frame}")
  (reindent-then-newline-and-indent)
  (previous-line)
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  )

(defun new-slide()
  "Get a slide NAME and insert it."
  (interactive)
  (let ((name (read-string "Frame Title: ")))
    (beamer-new-frame name))
  )
(global-set-key (kbd "C-c f") 'new-slide)

(defun simplified-block()
  (interactive)
  (insert "\\begin{exampleblock}{In Plain English}")
  (reindent-then-newline-and-indent)
  (insert "\\end{exampleblock}")
  (reindent-then-newline-and-indent)
  (previous-line)
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  )
(global-set-key (kbd "C-c s") 'simplified-block)
)

;; AUCTeX-mode
(setq TeX-parse-self t); Enable automatic parsing
(setq TeX-auto-save t); Enable parse on save

(defun extra-cc-keybindings()
  (global-set-key (kbd "C-?") (kbd "M-x manual-entry RET"))
  )

;; Cc Mode
;; Set the indentation to 4 spaces
(setq-default c-basic-offset 4
              c-default-style "bsd")

;; Enable 80-column fill indicator for C files
(add-hook 'cc-mode-hook 'turn-on-auto-fill)

;; set up auto-complete-mode for C files
(add-hook 'cc-mode-hook 'auto-complete-mode)
(add-hook 'cc-mode-hook 'etags-c-tags)
(add-hook 'cc-mode-hook 'auto-complete-mode)
(add-hook 'cc-mode-hook 'extra-cc-keybindings)

;; Require ox-confluence
(require 'ox-confluence)

(ruby-config)

(pdf-loader-install)

;; Setup theme
(defun configure-theme()
;  (load-theme 'spacemacs-dark)
                                        ;(load-theme 'leuven)
   (load-theme 'rebecca)
  )

(configure-theme)

;;; init.el ends here
