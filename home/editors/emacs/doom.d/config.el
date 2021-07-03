; open links in firefox
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag t)

; press C-x 7 to open next buffer in a new frame
(other-frame-window-mode)

; just set a bigger font
(set-face-attribute 'default nil :height 110)

; fixed size for tabs since dynamic fucks everything up
(setq centaur-tabs-label-fixed-length 18)

; pairs for neuron
(sp-with-modes 'neuron-mode
  (sp-local-pair "\\begin{pmatrix}" "\\end{pmatrix}" :trigger "\\pma")
  (sp-local-pair "$$\\begin{aligned}" "\\end{aligned}$$" :trigger "\\ali") ; maybe this one should be a snippet, maybe add newlines too
  (sp-local-pair "$$" "$$")
  ;(sp-local-tag "\\b" "\\begin{_}" "\\end{_}")
)

; minor fix for the painful scrolling
;(require 'smooth-scrolling)
;(smooth-scrolling-mode 1)
