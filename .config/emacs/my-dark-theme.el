;;; -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-one-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-one-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-one-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-one-comment-bg doom-one-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-one-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-one-theme
  :type '(choice integer boolean))

;;
(def-doom-theme my-dark
				"My fork of one-dark"

				;; name        default   256       16
				((bg         '("#1e222a" nil       nil            ))
				 (bg-alt     '("#191d23" nil       nil            ))
				 (base0      '("#1B2229" "black"   "black"        ))
				 (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
				 (base2      '("#202328" "#2e2e2e" "brightblack"  ))
				 (base3      '("#1D2026" "#262626" "brightblack"  ))
				 (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
				 (base5      '("#5B6268" "#525252" "brightblack"  ))
				 (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
				 (base7      '("#9ca0a4" "#979797" "brightblack"  ))
				 (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
				 (fg         '("#bbc2cf" "#bfbfbf" "brightwhite"  ))
				 (fg-alt     '("#5B6268" "#2d2d2d" "white"        ))

				 (grey       base4)
				 (red        '("#e06c75" "#ff6655" "red"          ))
				 (orange     '("#d19a66" "#dd8844" "brightred"    ))
				 (green      '("#98c379" "#99bb66" "green"        ))
				 (teal       '("#56b6c2" "#44b9b1" "brightgreen"  ))
				 (yellow     '("#e5c07b" "#ECBE7B" "yellow"       ))
				 (blue       '("#61afef" "#51afef" "brightblue"   ))
				 (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
				 (magenta    '("#c678dd" "#c678dd" "brightmagenta"))
				 (violet     '("#a9a1e1" "#a9a1e1" "magenta"      ))
				 (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
				 (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

				 ;;    base0A = #e5c07b,
				 ;;    base04 = #565c64,
				 ;;    base07 = #c8ccd4,
				 ;;    base05 = #abb2bf,
				 ;;    base0E = #c678dd,
				 ;;    base0D = #61afef,
				 ;;    base0C = #56b6c2,
				 ;;    base0B = #98c379,
				 ;;    base02 = #3e4451,
				 ;;    base0F = #be5046,
				 ;;    base03 = #545862,
				 ;;    base08 = #e06c75,
				 ;;    base01 = #353b45,
				 ;;    base00 = #1e222a,
				 ;;    base09 = #d19a66,
				 ;;    base06 = #b6bdca

				 ;; face categories -- required for all themes
				 (highlight      blue)
				 (vertical-bar   (doom-darken base1 0.1))
				 (selection      dark-blue)
				 (builtin        magenta)
				 (comments       (if doom-one-brighter-comments dark-cyan base5))
				 (doc-comments   (doom-lighten (if doom-one-brighter-comments dark-cyan base5) 0.25))
				 (constants      violet)
				 (functions      magenta)
				 (keywords       blue)
				 (methods        cyan)
				 (operators      blue)
				 (type           yellow)
				 (strings        green)
				 (variables      (doom-lighten magenta 0.4))
				 (numbers        orange)
				 (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
				 (error          red)
				 (warning        yellow)
				 (success        green)
				 (vc-modified    orange)
				 (vc-added       green)
				 (vc-deleted     red)

				 ;; custom categories
				 (hidden `(,(car bg) "black" "black"))
				 (-modeline-bright doom-one-brighter-modeline)
				 (-modeline-pad
				  (when doom-one-padded-modeline
					(if (integerp doom-one-padded-modeline) doom-one-padded-modeline 4)))

				 (modeline-fg     fg)
				 (modeline-fg-alt base5)

				 (modeline-bg
				  (if -modeline-bright
					  (doom-darken blue 0.475)
					`(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
				 (modeline-bg-l
				  (if -modeline-bright
					  (doom-darken blue 0.45)
					`(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
				 (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
				 (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


				;; --- extra faces ------------------------
				((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

				 (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

				 ((line-number &override) :foreground base4)
				 ((line-number-current-line &override) :foreground fg)

				 (font-lock-comment-face
				  :foreground comments
				  :background (if doom-one-comment-bg (doom-lighten bg 0.05)))
				 (font-lock-doc-face
				  :inherit 'font-lock-comment-face
				  :foreground doc-comments)

				 (mode-line
				  :background modeline-bg :foreground modeline-fg
				  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
				 (mode-line-inactive
				  :background modeline-bg-inactive :foreground modeline-fg-alt
				  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
				 (mode-line-emphasis
				  :foreground (if -modeline-bright base8 highlight))

				 (solaire-mode-line-face
				  :inherit 'mode-line
				  :background modeline-bg-l
				  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
				 (solaire-mode-line-inactive-face
				  :inherit 'mode-line-inactive
				  :background modeline-bg-inactive-l
				  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

				 ;; Doom modeline
				 (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
				 (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
				 (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
				 (doom-modeline-buffer-project-root :foreground green :weight 'bold)

				 ;; ivy-mode
				 (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

				 ;; --- major-mode faces -------------------
				 ;; css-mode / scss-mode
				 (css-proprietary-property :foreground orange)
				 (css-property             :foreground green)
				 (css-selector             :foreground blue)

				 ;; LaTeX-mode
				 (font-latex-math-face :foreground green)

				 ;; markdown-mode
				 (markdown-markup-face :foreground base5)
				 (markdown-header-face :inherit 'bold :foreground red)
				 ((markdown-code-face &override) :background (doom-lighten base3 0.05))

				 ;; org-mode
				 (org-hide :foreground hidden)
				 (solaire-org-hide-face :foreground hidden)

				 ;; lsp-mode
				 (lsp-headerline-breadcrumb-separator-face :foreground green)

				 ;; rjsx
				 (rjsx-tag :foreground red)
				 (rjsx-attr :foreground orange)
				 (orderless-match-face-0 :weight 'bold :foreground blue    :background (doom-blend blue    bg 0.05))
				 (orderless-match-face-1 :weight 'bold :foreground magenta :background (doom-blend magenta bg 0.05))
				 (orderless-match-face-2 :weight 'bold :foreground green   :background (doom-blend green   bg 0.05))
				 (orderless-match-face-3 :weight 'bold :foreground yellow  :background (doom-blend yellow  bg 0.05))
				 (vertico-current :background (doom-darken dark-blue 0.1)))

				;; --- extra variables ---------------------
				())

;;; doom-one-theme.el ends here
