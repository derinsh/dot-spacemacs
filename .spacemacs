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

     spacemacs-org
     spacemacs-purpose
     spacemacs-project
     spacemacs-navigation
     ;; spacemacs-modeline
     spacemacs-layouts
     dtrt-indent
     spacemacs-completion
     spacemacs-defaults
     ;; spacemacs-editing
     ;; spacemacs-editing-visual
     ;; better-defaults

     (shell :variables
            shell-default-shell 'vterm
            ;; shell-default-term-shell "/bin/zsh"
            shell-default-height 30
            shell-default-position 'bottom)

     ;;;; Prog tools

     version-control
     git
     dap

     (auto-completion :variables
                      spacemacs-default-company-backends '(company-files company-capf company-dabbrev-code)
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t)

     (lsp :variables
          ;; Formatting and indentation - use Cider instead
          lsp-enable-on-type-formatting nil
          ;; Set to nil to use CIDER features instead of LSP UI
          lsp-enable-indentation nil
          lsp-enable-snippet nil  ;; to test again
          ;; symbol highlighting - `lsp-toggle-symbol-highlight` toggles highlighting
          ;; subtle highlighting for doom-gruvbox-light theme defined in dotspacemacs/user-config
           ;;lsp-enable-symbol-highlighting t

          ;; Show lint error indicator in the mode line
          lsp-modeline-diagnostics-enable t
          ;; lsp-modeline-diagnostics-scope :workspace

          ;; popup documentation boxes
           lsp-ui-doc-enable t          ;; disable all doc popups
          ;; lsp-ui-doc-show-with-cursor t   ;; doc popup for cursor
          ;; lsp-ui-doc-show-with-mouse t   ;; doc popup for mouse
          ;; lsp-ui-doc-delay 2             ;; delay in seconds for popup to display
          lsp-ui-doc-include-signature t    ;; include function signature
          ;; lsp-ui-doc-position 'at-point  ;; positioning of doc popup: top bottom at-point
          ;; lsp-ui-doc-alignment 'window      ;; relative location of doc popup: frame window

          ;; code actions and diagnostics text as right-hand side of buffer
          ;; lsp-ui-sideline-enable nil
          ;; lsp-ui-sideline-show-code-actions nil
          ;; lsp-ui-sideline-delay 500

          ;; lsp-ui-sideline-show-diagnostics nil

          ;; reference count for functions (assume their maybe other lenses in future)
          ;; lsp-lens-enable t

          ;; Efficient use of space in treemacs-lsp display
          treemacs-space-between-root-nodes nil

          ;; Optimization for large files
          lsp-file-watch-threshold 10000
          lsp-log-io nil


          lsp-use-lsp-ui t
          lsp-headerline-breadcrumb-enable t)

     (syntax-checking :variables syntax-checking-auto-hide-tooltips 5
                      syntax-checking-enable-tooltips t
                      syntax-checking-enable-by-default t)

     ;;;; Lang

     ;; Lisp
     emacs-lisp
     common-lisp
     clojure
     (scheme :variables
             scheme-implementations '(racket))
     racket
     extempore

     ;; OO
     c-c++
     rust
     go
     (java :variables java-backend 'lsp)

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

     ;; Misc
     markdown
     shell-scripts
     systemd
     csv
     latex

     ;;;; Emacs

     helm
     org
     ibuffer
     imenu-list
     ranger
     (treemacs :variables treemacs-use-all-the-icons-theme t
                          treemacs-use-git-mode 'deferred)

     ;;;; UI

     (unicode-fonts :variables unicode-fonts-enable-ligatures t)
     typography
     theming
     colors
     ;; tabs

     ;; Doc
     pdf
     dash
     helpful

     xkcd
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
   dotspacemacs-additional-packages '(flycheck-posframe all-the-icons minibuffer-header company-box)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.

   dotspacemacs-excluded-packages '()

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
   dotspacemacs-gc-cons '(100000000 0.1)

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
   dotspacemacs-default-font '("FiraCode Nerd Font"
                               :size 12.5
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

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
   dotspacemacs-active-transparency 90

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
   dotspacemacs-smooth-scrolling t

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
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

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
   dotspacemacs-frame-title-format "%b @ Emacs";nil ;"%I@%S"

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
  (spacemacs/load-spacemacs-env)
  )

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

  ;; (defvar slime-repl-font-lock-keywords lisp-el-font-lock-keywords-2)
  ;; (defun slime-repl-font-lock-setup ()
  ;;   (setq font-lock-defaults
  ;;         '(slime-repl-font-lock-keywords
  ;;           ;; From lisp-mode.el
  ;;           nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
  ;;           (font-lock-syntactic-face-function
  ;;            . lisp-font-lock-syntactic-face-function))))

  ;; (add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)

  ;; (defadvice slime-repl-insert-prompt (after font-lock-face activate)
  ;;   (let ((inhibit-read-only t))
  ;;     (add-text-properties
  ;;      slime-repl-prompt-start-mark (point)
  ;;      '(font-lock-face
  ;;        slime-repl-prompt-face
  ;;        rear-nonsticky
  ;;        (slime-repl-prompt read-only font-lock-face intangible)))))

  (defvar string-edit-mode nil)

  (setq-default lsp-use-plists t)

  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;;;; Local packages

  (let ((default-directory "~/.emacs.d/usr"))
    (normal-top-level-add-subdirs-to-load-path))

  (require 'use-package)

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

  (use-package markdown-xwidget
    :after markdown-mode
    :load-path "usr/markdown-xwidget"
    :bind (:map markdown-mode-command-map
                ("x" . markdown-xwidget-preview-mode))
    :custom
    (markdown-xwidget-command "pandoc")
    (markdown-xwidget-github-theme "light")
    (markdown-xwidget-mermaid-theme "default")
    (markdown-xwidget-code-block-theme "default"))

  ;;;; Preload

  (require 'helm-files)
  (require 'helm-command)

  ;;;; Prog config

  (use-package company
    :hook (scala-mode . company-mode)
    :config
    (setq lsp-completion-provider :capf))

  (add-hook 'company-completion-started-hook
            #'(lambda (&rest _)
                (setq-local lsp-inhibit-lsp-hooks t)
                (lsp--capf-clear-cache))
            nil
            t)

  (add-hook 'geiser-repl-mode-hook
            ;; Shift-return otherwise
            (evil-define-key '(normal insert visual emacs) geiser-repl-mode (kbd "C-<enter>") #'geiser-repl--newline-and-indent))

  (add-hook 'clojure-mode-hook #'lsp-deferred)

  ;; (use-package slime-company
  ;;   :after (slime company)
  ;;   :config (setq slime-company-completion 'simple
  ;;                 slime-company-after-completion 'nil))

  (add-hook 'prog-mode-hook 'undo-tree-mode)
  (remove-hook 'cider-repl-mode-hook 'vim-empty-lines-mode)
  (evil-set-initial-state 'cider-repl-mode 'insert)

  ;;;; UI

  (blink-cursor-mode 1)

  (set-window-scroll-bars (minibuffer-window) 0 'none)

  (defun update-scroll-bars ()
    (interactive)
    (mapc (lambda (win)
            (set-window-scroll-bars win nil))
          (window-list))
    (set-window-scroll-bars (selected-window) 10 'right))

  (add-hook 'window-configuration-change-hook #'update-scroll-bars)
  (add-hook 'buffer-list-update-hook #'update-scroll-bars)

  ;; No quote pair in minib eval
  ;; Not working
  ;; (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil :unless '(my-in-eval-expression-p))
  ;; (defun my-in-eval-expression-p (id action context) (equalp last-command 'eval-expression))

  (defun customkeys ()
    (evil-define-key '(normal insert visual motion emacs) 'global (kbd "<C-left>") #'previous-buffer)
    (evil-define-key '(normal insert visual motion emacs) 'global (kbd "<C-right>") #'next-buffer)
    (evil-define-key '(normal insert visual emacs) dired-mode-map [mouse-1] #'dired-find-file))


  (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  (spaceline-toggle-all-the-icons-eyebrowse-workspace-off)
  (spaceline-toggle-all-the-icons-time-off)
  (spaceline-toggle-all-the-icons-bookmark-on)
  (spaceline-toggle-all-the-icons-dedicated-on)
  (spaceline-toggle-all-the-icons-window-number-off)
  (spaceline-toggle-projectile-root-off)
  (spaceline-toggle-all-the-icons-projectile-off)
  (setq spaceline-all-the-icons-slim-render nil)
  ;;(spaceline-compile)

  ;;;; Misc

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
 '(all-the-icons-nerd-fonts-family "FiraCode Nerd Font")
 '(bidi-paragraph-direction 'left-to-right)
 '(blink-cursor-mode t)
 '(centaur-tabs-auto-scroll-flag nil)
 '(centaur-tabs-gray-out-icons 'buffer)
 '(centaur-tabs-set-bar 'left)
 '(centaur-tabs-set-modified-marker nil)
 '(cider-auto-jump-to-error nil)
 '(cider-auto-select-error-buffer nil)
 '(cider-eldoc-display-context-dependent-info t)
 '(cider-eldoc-display-for-symbol-at-point t)
 '(cider-jack-in-default 'clojure-cli)
 '(cider-repl-display-help-banner t)
 '(cider-repl-pop-to-buffer-on-connect t)
 '(cider-show-error-buffer nil)
 '(column-number-mode t)
 '(comint-input-ignoredups t)
 '(company-tooltip-align-annotations t)
 '(context-menu-mode t)
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(dap-internal-terminal 'dap-internal-terminal-vterm)
 '(frame-background-mode 'dark)
 '(global-display-line-numbers-mode t)
 '(global-prettify-symbols-mode t)
 '(global-term-cursor-mode nil)
 '(global-undo-tree-mode nil)
 '(helm-M-x-show-short-doc nil)
 '(helm-apropos-show-short-doc t)
 '(helm-frame-alpha 98)
 '(helm-popup-tip-mode t)
 '(image-use-external-converter t)
 '(initial-buffer-choice '(closure (t) nil (get-buffer-create "*spacemacs*")))
 '(ls-lisp-use-insert-directory-program t)
 '(lsp-completion-provider :capf)
 '(lsp-eldoc-render-all t)
 '(lsp-enable-folding nil)
 '(lsp-enable-indentation nil)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-headerline-breadcrumb-enable-diagnostics nil)
 '(lsp-java-workspace-dir "/home/psi/code/inlmning")
 '(lsp-lens-enable t)
 '(lsp-progress-spinner-type 'horizontal-breathing)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-sideline-enable nil)
 '(menu-bar-mode t)
 '(mouse-1-click-follows-link 50)
 '(mouse-wheel-progressive-speed nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.6 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(mustache company-go counsel-gtags counsel swiper ivy flycheck-golangci-lint ggtags go-eldoc go-fill-struct go-gen-test go-guru go-impl go-rename go-tag go-mode godoctor helm-gtags racket-mode geiser-racket geiser which-key use-package hybrid-mode holy-mode font-lock+ evil-evilified-state dotenv-mode diminish zeal-at-point yasnippet-snippets yapfify xterm-color xkcd ws-butler writeroom-mode winum web-mode web-beautify volatile-highlights vim-powerline vi-tilde-fringe uuidgen unicode-fonts undo-tree typo treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toml-mode toc-org tide terminal-here term-cursor tagedit systemd symon symbol-overlay string-inflection string-edit-at-point sphinx-doc spacemacs-whitespace-cleanup spacemacs-purpose-popwin spaceline-all-the-icons space-doc smeargle slime-company slim-mode shfmt shell-pop seeing-is-believing scss-mode sass-mode rvm rust-mode ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode ron-mode robe rjsx-mode restart-emacs rbenv ranger rake rainbow-mode rainbow-identifiers rainbow-delimiters quickrun pytest pylookup pyenv-mode pydoc py-isort pug-mode prettier-js popwin poetry pippel pipenv pip-requirements pdf-view-restore password-generator paradox overseer orgit-forge org-superstar org-rich-yank org-projectile org-present org-pomodoro org-mime org-download org-contrib org-cliplink open-junk-file npm-mode nose nodejs-repl nameless mvn multi-vterm multi-term multi-line mmm-mode minitest minibuffer-header maven-test-mode markdown-toc lsp-ui lsp-python-ms lsp-pyright lsp-origami lsp-latex lsp-java lorem-ipsum livid-mode live-py-mode link-hint ligature json-reformat json-navigator json-mode js2-refactor js-doc journalctl-mode inspector insert-shebang info+ indent-guide importmagic impatient-mode ibuffer-projectile hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt helpful help-fns+ helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-git-grep helm-descbinds helm-dash helm-css-scss helm-company helm-cider helm-c-yasnippet helm-ag groovy-mode groovy-imports google-translate google-c-style golden-ratio gnuplot gitignore-templates git-timemachine git-modes git-messenger git-link git-gutter-fringe gh-md gendoxy fuzzy flycheck-ycmd flycheck-rust flycheck-rtags flycheck-posframe flycheck-pos-tip flycheck-package flycheck-elsa flycheck-bashate flx-ido fish-mode fancy-battery eyebrowse extempore-mode expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-tex evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help emr emmet-mode elisp-slime-nav elisp-def editorconfig dumb-jump dtrt-indent drag-stuff disaster dired-quick-sort devdocs define-word cython-mode csv-mode cpp-auto-include copy-as-format company-ycmd company-web company-statistics company-shell company-rtags company-reftex company-quickhelp company-math company-c-headers company-auctex company-anaconda common-lisp-snippets column-enforce-mode color-identifiers-mode code-cells clojure-snippets clean-aindent-mode cider-eval-sexp-fu chruby centered-cursor-mode ccls cargo bundler browse-at-remote blacken auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent ace-link ace-jump-helm-line ac-ispell))
 '(pdf-view-display-size 'fit-height)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#262626"))
 '(pdf-view-use-scaling t)
 '(pixel-scroll-precision-interpolate-mice nil)
 '(pixel-scroll-precision-mode t)
 '(pixel-scroll-precision-momentum-min-velocity 40.0)
 '(pixel-scroll-precision-momentum-seconds 0.2)
 '(pixel-scroll-precision-use-momentum t)
 '(powerline-default-separator 'curve)
 '(powerline-gui-use-vcs-glyph t)
 '(powerline-text-scale-factor 1.075)
 '(prettify-symbols-unprettify-at-point t)
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-directories
   '("^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$" "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$" ".clj-kondo" ".lsp" ".cpcache"))
 '(projectile-globally-ignored-file-suffixes '("~undo-tree~"))
 '(projectile-indexing-method 'alien)
 '(slime-company-after-completion nil)
 '(slime-company-transform-arglist 'downcase)
 '(slime-repl-history-size 1000)
 '(spaceline-all-the-icons-clock-always-visible nil)
 '(spaceline-all-the-icons-icon-set-flycheck-slim 'outline)
 '(spaceline-all-the-icons-icon-set-git-ahead 'commit)
 '(spaceline-all-the-icons-icon-set-window-numbering 'square)
 '(spaceline-all-the-icons-separator-type 'cup)
 '(spaceline-all-the-icons-separators-invert-direction nil)
 '(spaceline-all-the-icons-slim-render nil)
 '(spaceline-show-default-input-method t)
 '(spacemacs-theme-custom-colors
   '((bg2 . "#353535")
     (bg1 . "#181818")
     (comment-bg)
     (highlight . "#0e587c")
     (lnum . "#67a08f")))
 '(tab-bar-auto-width-max '(360 30))
 '(tab-bar-mode t)
 '(tab-bar-new-tab-choice 'helm-recentf)
 '(tab-bar-new-tab-to 'rightmost)
 '(tab-bar-select-tab-modifiers '(meta))
 '(tab-bar-show t)
 '(tab-bar-tab-hints nil)
 '(tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
 '(tab-bar-tab-name-truncated-max 28)
 '(tab-line-exclude-modes '(completion-list-mode shell pdf-view-mode))
 '(tab-line-new-tab-choice 'helm-recentf)
 '(tab-line-tab-name-function 'tab-line-tab-name-truncated-buffer)
 '(tab-line-tabs-function 'tab-line-tabs-window-buffers)
 '(tool-bar-mode nil)
 '(undo-limit 3200000)
 '(undo-outer-limit 42000000)
 '(undo-strong-limit 520000)
 '(undo-tree-auto-save-history t)
 '(use-system-tooltips nil)
 '(warning-minimum-level :emergency)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(button ((t (:inherit link :box (:line-width (2 . 2) :color "dark slate gray" :style released-button) :underline nil))))
 '(centaur-tabs-active-bar-face ((t (:background "#4f97d7" :foreground "light gray" :family "NotoMono Nerd Font"))))
 '(error ((t (:foreground "#e67f43" :family "Source Code Pro"))))
 '(font-lock-comment-face ((t (:foreground "#2aa1ae" :slant italic :height 0.8))))
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t)
 '(line-number ((t (:foreground "#67a08f" :background "#353535" :inherit JetBrainsMonoNL\ Nerd\ Fonts))))
 '(lsp-ui-sideline-global ((t (:weight semi-bold :height 120 :family "Iosevka"))))
 '(minibuffer-header-face ((t (:extend t :background "DarkSeaGreen1" :foreground "gray10"))))
 '(mode-line ((t (:background "#222226" :foreground "light slate gray" :box (:line-width (1 . 1) :color "#5d4d7a")))))
 '(powerline-active1 ((t (:background "#5d4d7a" :foreground "gainsboro"))))
 '(scroll-bar ((t (:background "gray8"))))
 '(spaceline-evil-insert ((t (:background "chartreuse3" :foreground "systembackground" :inherit 'mode-line))))
 '(spaceline-evil-normal ((t (:background "DarkGoldenrod2" :foreground "systembackground" :inherit 'mode-line))))
 '(spaceline-highlight-face ((t (:inherit 'mode-line :foreground "systembackground" :background "DarkGoldenrod2"))))
 '(spaceline-read-only ((t (:background "plum3" :foreground "systembackground" :inherit 'mode-line))))
 '(spaceline-unmodified ((t (:background "DarkGoldenrod2" :foreground "systembackground" :inherit 'mode-line))))
 '(tab-bar ((t (:background "#181818" :foreground "#b9b9b9" :height 115 :family "Noto Sans Nerd Font"))))
 '(tab-bar-tab ((t (:background "cornsilk3" :foreground "gray14" :box (:line-width (3 . 3) :color "cornsilk4" :style flat-button) :weight extra-bold :height 111 :width normal))))
 '(tab-bar-tab-inactive ((t (:background "gray14" :foreground "dark gray" :box (:line-width (3 . 3) :color "gray20" :style released-button) :weight bold :height 110 :width semi-condensed))))
 '(tab-line ((t (:background "#181818" :foreground "#b2b2b2" :weight bold :family "Noto Sans Nerd Font"))))
 '(tab-line-highlight ((t (:background "grey85" :foreground "black" :box (:line-width (1 . 1) :style released-button)))))
 '(tab-line-tab ((t (:inherit tab-line :box (:line-width (2 . 2) :color "gray14" :style released-button) :weight bold))))
 '(tab-line-tab-inactive ((t (:background "#353535" :foreground "#686868" :weight regular))))
 '(tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width (1 . 1) :style released-button)))))
 '(tooltip ((t (:stipple "" :background "#4b4062" :foreground "#b2b2b2" :underline nil :slant normal :weight medium :height 1.05 :family "NotoSans Nerd Font")))))
)
