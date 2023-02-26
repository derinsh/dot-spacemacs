;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;;;; Spacemacs distro

     ;; spacemacs-org
     ;; spacemacs-purpose
     ;; spacemacs-project
     ;; spacemacs-navigation
     ;; spacemacs-modeline
     ;; spacemacs-layouts
     ;; spacemacs-completion
     ;; spacemacs-defaults
     ;; spacemacs-editing
     ;; spacemacs-editing-visual
     ;; better-defaults

     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)

     ;;;; Prog tools

     lsp
     version-control
     git
     dap
     ;tree-sitter
     (auto-completion :variables
                      spacemacs-default-company-backends '(company-files company-capf company-dabbrev-code)
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-use-company-box t)
     (syntax-checking :variables
                      syntax-checking-auto-hide-tooltips 5
                      syntax-checking-enable-tooltips t
                      syntax-checking-enable-by-default t)

     ;;;; Lang

     ;; Lisp
     emacs-lisp
     common-lisp
     (clojure :variables clojure-enable-linters '(clj-kondo joker)
              clojure-backend 'lsp)
     (scheme :variables scheme-implementations '(racket))
     racket
     extempore

     ;; Fun
     erlang
     elixir
     (haskell :variables
              haskell-completion-backend 'lsp
              haskell-enable-hindent t)

     ;; OO
     c-c++
     rust
     go
     ;(java :variables java-backend 'lsp)

     ;; Web
     html
     javascript
     typescript
     web-beautify
     react
     node
     json

     ;; Script
     python
     ruby

     ;; Windows
     windows-scripts
     autohotkey
     docker

     ;; Misc
     markdown
     shell-scripts
     csv
     latex
     sql

     ;;;; Emacs

     helm
     org
     ibuffer
     imenu-list
     ranger
     (treemacs :variables
               treemacs-use-all-the-icons-theme t
               treemacs-use-git-mode 'deferred)

     ;;;; UI

     (unicode-fonts :variables unicode-fonts-enable-ligatures t)
     typography
     theming
     colors
     ;tabs
     ;dtrt-indent

     ;; Doc
     pdf
     ;dash
     helpful

     ;xkcd
     copy-as-format
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(flycheck-posframe company-box rustic)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.

   dotspacemacs-excluded-packages '(systemd term-cursor)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  ;; (defvar trailing)
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(10000000 0.1) ;(list (* 1024 1024 256) 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024 8)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil, show vim-like empty line indicators at the end of files.
   ;; Takes effect only if `spacemacs-evil' layer is enabled.
   ;; NOTICE: `spacemacs-evil' is included in `spacemacs' distribution.
   ;; See `dotspacemacs-distribution'.
   dotspacemacs-evil-show-empty-line-indicators t

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 8)
                                (projects . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(all-the-icons :separator cup :separator-scale 1.8)
   ;; dotspacemacs-mode-line-theme 'all-the-icons

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("FiraCode NF"
                               :size 10.0
                               :weight regular
                               :width normal
                               :powerline-scale 1.2)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 2

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols 'display-graphic-p

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(:absolute t
                                         :disabled-for-modes dired-mode
                                         doc-view-mode
                                         pdf-view-mode
                                         :size-limit-kb 1000)

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "rg" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%b @ Emacs"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))


(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (defconst dd/using-native-comp (and (fboundp 'native-comp-available-p)
                                      (native-comp-available-p)))
  (setq native-comp-async-query-on-exit t)
  (setq native-comp-async-report-warnings-errors nil)
  ;(setq use-package-always-demand t)

  (setenv "LSP_USE_PLISTS" "1")
  (setq-default lsp-use-plists t)

  (let ((default-directory "~/.emacs.d/usr"))
    (normal-top-level-add-subdirs-to-load-path))

  (require 'use-package)
  )



(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")



(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;;;; Local packages

  (use-package flycheck-posframe
    :ensure t
    :after flycheck
    :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

  (require 'nerd-fonts)

  (use-package all-the-icons
    :config
    ;; Make sure the icon fonts are good to go
    (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
    (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
    (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
    (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
    (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
    (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append))

  (use-package all-the-icons-nerd-fonts
    :load-path "usr/all-the-icons-nerd-fonts"
    :after all-the-icons
    :demand t
    :config) ;(all-the-icons-nerd-fonts-prefer))

  (use-package spaceline-all-the-icons
    :after spaceline
    :config (spaceline-all-the-icons-theme))

  ;; Native tree-sitter, but not working...
  ;(require 'treesit)
  ;(setq-default treesit-extra-load-path "usr/tree-sitter-module/dist")

  (use-package company
   :config
   (setq lsp-completion-provider :capf))

  ;;;; Preload

  (require 'helm-files)
  (require 'helm-command)

  ;;;; Prog config

  (add-hook 'company-completion-started-hook
              #'(lambda (&rest _)
                  (setq-local lsp-inhibit-lsp-hooks t)
                  (lsp--capf-clear-cache))
              nil
              t)

  ;(add-hook 'clojure-mode-hook #'lsp-deferred)

  (add-hook 'geiser-repl-mode-hook
            ;; Shift-return otherwise
            (evil-define-key '(normal insert visual emacs) geiser-repl-mode (kbd "C-<enter>") #'geiser-repl--newline-and-indent))

  (add-hook 'prog-mode-hook 'undo-tree-mode)
  (remove-hook 'cider-repl-mode-hook 'vim-empty-lines-mode)
  (evil-set-initial-state 'cider-repl-mode 'insert)

  ;;;; UI

  (blink-cursor-mode 1)

  (set-window-scroll-bars (minibuffer-window) 0 nil)

  ;; Scrollbar config

  (defun update-scroll-bars ()
    (interactive)
    (mapc (lambda (win)
            (set-window-scroll-bars win nil))
          (window-list))
    (set-window-scroll-bars (selected-window) 10 'right))

  (add-hook 'window-configuration-change-hook #'update-scroll-bars)
  (add-hook 'buffer-list-update-hook #'update-scroll-bars)

  (defun customkeys ()
    (evil-define-key '(normal insert visual motion emacs) 'global [mouse-4] #'previous-buffer)
    (evil-define-key '(normal insert visual motion emacs) 'global [mouse-5] #'next-buffer)
    (evil-define-key '(normal insert visual emacs) dired-mode-map [mouse-1] #'dired-find-file))

  (spaceline-toggle-all-the-icons-eyebrowse-workspace-off)
  (spaceline-toggle-all-the-icons-time-off)
  (spaceline-toggle-all-the-icons-bookmark-on)
  (spaceline-toggle-all-the-icons-dedicated-on)
  (spaceline-toggle-all-the-icons-window-number-off)
  (spaceline-toggle-projectile-root-off)
  (spaceline-toggle-all-the-icons-projectile-off)
  (setq spaceline-all-the-icons-slim-render nil)

  ;;;; Misc

  (setq-default find-program "c:/lib/msys64/usr/bin/find.exe")
  (add-hook 'Info-selection-hook 'info-colors-fontify-node)
  (setq-default bidi-inhibit-bpa t)
  (setq-default inhibit-compacting-font-caches t)
  (customkeys)
  )




(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons--cache-limit 4096)
 '(all-the-icons-alltheicon-scale-factor 1.4)
 '(all-the-icons-scale-factor 1.4)
 '(bidi-paragraph-direction 'left-to-right)
 '(byte-compile-docstring-max-column 120)
 '(byte-compile-verbose nil)
 '(cider-auto-jump-to-error nil)
 '(cider-auto-select-error-buffer nil)
 '(cider-clojure-cli-command "pwsh")
 '(cider-jack-in-default 'clojure-cli)
 '(cider-repl-use-content-types t)
 '(cider-show-error-buffer 'except-in-repl)
 '(context-menu-mode t)
 '(epg-gpg-home-directory "c:/program files (x86)/gnupg/bin/gpg.exe")
 '(global-prettify-symbols-mode t)
 '(image-use-external-converter t)
 '(ls-lisp-use-insert-directory-program t)
 '(lsp-configure-hook
   '(lsp-lens--enable lsp-modeline-workspace-status-mode lsp-modeline-diagnostics-mode lsp-modeline-code-actions-mode lsp-headerline-breadcrumb-mode
                      (closure
                          (t)
                          nil
                        (if lsp-auto-configure
                            (progn
                              (lsp-diagnostics--enable))))
                      (closure
                          (t)
                          nil
                        (if
                            (and lsp-auto-configure lsp-completion-enable)
                            (progn
                              (lsp-completion--enable))))))
 '(lsp-eldoc-render-all t)
 '(lsp-enable-indentation nil)
 '(lsp-file-watch-threshold 10000)
 '(lsp-headerline-breadcrumb-enable-diagnostics nil)
 '(lsp-keep-workspace-alive nil)
 '(lsp-rust-analyzer-cargo-watch-command "clippy")
 '(lsp-rust-analyzer-display-chaining-hints t)
 '(lsp-rust-analyzer-display-closure-return-type-hints t)
 '(lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
 '(lsp-rust-analyzer-server-display-inlay-hints t)
 '(lsp-treemacs-sync-mode t)
 '(magit-git-executable "C:/Program Files/Git/bin/git.exe")
 '(menu-bar-mode t)
 '(mouse-1-click-follows-link t)
 '(native-comp-async-jobs-number 4)
 '(package-gnupghome-dir "~/.emacs.d/elpa/gnupg")
 '(package-selected-packages
   '(rustic tree-sitter tide typescript-mode geiser-racket geiser company-go counsel-gtags counsel swiper ivy flycheck-golangci-lint go-eldoc go-fill-struct go-gen-test go-guru go-impl go-rename go-tag go-mode godoctor lsp-mode docker aio docker-tramp dockerfile-mode which-key use-package font-lock+ dotenv-mode diminish yasnippet-snippets xterm-color ws-butler writeroom-mode winum web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen unicode-fonts undo-tree typo treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toml-mode toc-org terminal-here term-cursor tagedit symon symbol-overlay string-inflection string-edit-at-point spaceline-all-the-icons smeargle slime-company slim-mode shfmt shell-pop seeing-is-believing scss-mode sass-mode rvm rust-mode ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop ron-mode robe rjsx-mode restart-emacs rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters racket-mode quickrun pug-mode prettier-js powershell popwin pdf-view-restore password-generator paradox overseer orgit org-superstar org-rich-yank org-projectile org-present org-pomodoro org-mime org-download org-contrib org-cliplink open-junk-file npm-mode nodejs-repl nameless mvn multi-term multi-line mmm-mode minitest maven-test-mode markdown-toc lsp-ui lsp-origami lsp-latex lsp-java lorem-ipsum livid-mode link-hint ligature json-reformat json-navigator json-mode js2-refactor js-doc inspector insert-shebang info+ indent-guide impatient-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt helpful helm-xref helm-themes helm-swoop helm-rtags helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-gtags helm-git-grep helm-descbinds helm-ctest helm-css-scss helm-company helm-cider helm-c-yasnippet helm-ag groovy-mode groovy-imports google-translate google-c-style golden-ratio gnuplot gitignore-templates git-timemachine git-modes git-messenger git-link git-gutter-fringe gh-md ggtags gendoxy fuzzy flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-elsa flycheck-clj-kondo flycheck-bashate flx-ido floobits fish-mode fancy-battery eyebrowse extempore-mode expand-region evil-visualstar evil-visual-mark-mode evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help emr emmet-mode elisp-slime-nav elisp-def editorconfig dumb-jump drag-stuff disaster dired-quick-sort devdocs define-word csv-mode cpp-auto-include company-ycmd company-web company-statistics company-shell company-rtags company-reftex company-quickhelp company-math company-c-headers company-auctex common-lisp-snippets column-enforce-mode color-identifiers-mode cmake-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu chruby centered-cursor-mode ccls cargo bundler browse-at-remote bmx-mode auto-yasnippet auto-highlight-symbol auto-compile auctex-latexmk ahk-mode aggressive-indent ace-link ace-jump-helm-line ac-ispell hybrid-mode holy-mode evil-evilified-state vim-powerline spacemacs-whitespace-cleanup spacemacs-purpose-popwin space-doc rspec-mode help-fns+ evil-unimpaired evil-tex))
 '(projectile-globally-ignored-directories
   '("^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$" "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$" "^\\.clj-kondo" "^\\.lsp" "^\\.cpcache"))
 '(projectile-globally-ignored-file-suffixes '("~undo-tree~"))
 '(projectile-indexing-method 'alien)
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-custom-colors
   '((bg2 . "#353535")
     (bg1 . "#181818")
     (highlight . "#0e587c")
     (lnum . "#67a08f")))
 '(tab-bar-auto-width-max '(350 20))
 '(tab-bar-mode t)
 '(tab-bar-tab-name-truncated-max 30)
 '(treemacs-width 30)
 '(treemacs-window-background-color '("#1f1720" . "#45395b"))
 '(use-package-always-demand nil)
 '(warning-minimum-level :emergency)
 '(warning-suppress-types '((comp)))
 '(yas-global-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(button ((t (:inherit link :box (:line-width (2 . 2) :color "dark slate gray" :style released-button) :underline nil))))
 '(company-tooltip-common ((t (:inherit company-tooltip :background "#34323e" :foreground "#4f97d7" :underline nil :weight bold))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "#4f97d7" :underline nil :weight bold))))
 '(error ((t (:foreground "#e67f43" :family "Source Code Pro"))))
 '(font-lock-comment-face ((t (:foreground "#2aa1ae" :slant italic :height 0.8))))
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t)
 '(line-number ((t (:background "#353535" :foreground "#67a08f" :family "JetBrainsMonoNL Nerd Font"))))
 '(minibuffer-header-face ((t (:extend t :background "DarkSeaGreen1" :foreground "gray10"))))
 '(mode-line ((t (:height 1.025 :box (:line-width (1 . 1) :color "#5d4d7a") :foreground "#b2b2b2" :background "#222226"))))
 '(powerline-active1 ((t (:background "#5d4d7a" :foreground "SystemScrollbar"))))
 '(spaceline-evil-insert ((t (:background "chartreuse3" :foreground "systembackground" :inherit 'mode-line))))
 '(spaceline-evil-normal ((t (:background "DarkGoldenrod2" :foreground "systembackground" :inherit 'mode-line))))
 '(spaceline-highlight-face ((t (:inherit 'mode-line :foreground "systembackground" :background "DarkGoldenrod2"))))
 '(spaceline-read-only ((t (:background "plum3" :foreground "systembackground" :inherit 'mode-line))))
 '(spaceline-unmodified ((t (:background "DarkGoldenrod2" :foreground "systembackground" :inherit 'mode-line))))
 '(tab-bar ((t (:background "#181818" :foreground "#b9b9b9" :weight regular :height 110 :family "NotoSans NF"))))
 '(tab-bar-tab ((t (:background "cornsilk3" :foreground "gray14" :box (:line-width (3 . 3) :color "cornsilk4" :style flat-button) :weight bold :height 100 :width normal))))
 '(tab-bar-tab-inactive ((t (:background "gray14" :foreground "dark gray" :box (:line-width (3 . 3) :color "gray20" :style released-button) :weight semi-bold :height 100 :width normal))))
 '(tab-line ((t (:background "#181818" :foreground "#b2b2b2" :weight regular :height 100 :family "NotoSans NF"))))
 '(tab-line-highlight ((t (:inherit tab-line :background "grey85" :foreground "black" :box (:line-width (2 . 2) :style released-button) :weight bold))))
 '(tab-line-tab ((t (:inherit tab-line :box (:line-width (2 . 2) :color "gray14" :style released-button) :weight bold))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab))))
 '(tab-line-tab-inactive ((t (:inherit tab-line :background "#353535" :foreground "#686868" :box (:line-width (2 . 2) :color "gray14" :style flat-button) :weight semi-bold))))
 '(tab-line-tab-special ((t (:background "#353535" :box (:line-width (2 . 2) :color "gray14" :style flat-button) :weight bold))))
 '(tooltip ((t (:background "#4b4062" :foreground "#b2b2b2" :underline nil :slant normal :weight normal)))))
)
