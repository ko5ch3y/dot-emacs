;; Author: Adam Lloyd <lloyda2@rpi.edu>
;;
;; Note: Based on the molokai theme for vim by Tomas Restrepo, which
;; is in turn based on the monokai theme for textmate by Wimer
;; Hazenberg and a darker variant by Hamish Stuart Macpherson.

(eval-when-compile
  (require 'color-theme))

(defun color-theme-my-way ()
  (interactive)
  (color-theme-install
   '(color-theme-my-way
     ((foreground-color . "#CDCDCD")
      (background-color . "#323232")
      (cursor-color . "#A4CD28")
      (background-mode . dark))
     (default ((t (:foreground "#CDCDCD" :background "#323232"))))

     (region ((t (:background "#000000"))))
     (hl-line ((t (:background "#191919"))))
     (show-paren-match-face ((t (:background "#CD3D09"))))

     ;; green
     (font-lock-builtin-face ((t (:foreground "#A4CD28"))))
     (font-lock-keyword-face ((t (:foreground "#A4CD28"))))
     (font-lock-variable-name-face ((t (:foreground "#A4CD28"))))
     (mode-line ((t (:foreground "#191919" :background "#323232" :box (:line-width 1 :color "#A4CD28")))))
     (mode-line-inactive ((t (:foreground "#191919" :background "#323232" :box (:line-width 1 :color "#191919")))))
     ;(mode-line-buffer-id ((t (:foreground nil :background "#CD5907" :weight semi-bold))))

     ;; orange
     (font-lock-preprocessor-face ((t (:foreground "#CD5128"))))

     ;; grey
     (font-lock-comment-face ((t (:foreground "#646464"))))
     (font-lock-doc-face ((t (:foreground "#646464" :slant italic))))
     (font-lock-comment-delimiter-face ((t (:foreground "#646464" :slant italic))))

     ;; red
     (font-lock-function-name-face ((t (:foreground "#CD2851"))))

     ;; purple
     (font-lock-constant-face ((t (:foreground "#6D28CD"))))

     ;; pink
     (font-lock-string-face ((t (:foreground "#CD28A4"))))

     ;; cyan
     (font-lock-type-face ((t (:foreground "#28A4CD"))))

     ;; ---------

     (isearch ((t (:background "#293739"))))
     (isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))
     (lazy-highlight ((t (:background "#293739"))))
     ;; (lazy-highlight ((t (:background "#403D3D"))))
     (secondary-selection ((t (:background "#272822"))))
     (show-paren-mismatch-face ((t (:foreground "#960050"))))
     (highlight ((t (:foreground "#000000" :background "#C4BE89"))))

     (font-lock-negation-char-face ((t (:weight bold))))
     (font-lock-regexp-grouping-backslash ((t (:weight bold))))
     (font-lock-regexp-grouping-construct ((t (:weight bold))))
     (font-lock-warning-face ((t (:foreground "#FFFFFF" :background "#333333"))))

     (custom-face-tag ((t (:foreground "#89BDEF" :weight bold))))
     (custom-state ((t (:foreground "#A6E22E"))))

     (css-selector ((t (:foreground "#F92672"))))
     (css-property ((t (:foreground "#89BDEF"))))

     (diff-added ((t (:foreground "#A6E22E" :weight bold))))
     (diff-context ((t (:foreground "#F8F8F2"))))
     (diff-file-header ((t (:foreground "#89BDEF" :background nil))))
     (diff-indicator-added ((t (:foreground "#A6E22E"))))
     (diff-indicator-removed ((t (:foreground "#F92672"))))
     (diff-header ((t (:foreground "#F8F8F2" :background "#232526"))))
     (diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
     (diff-removed ((t (:foreground "#F92672" :weight bold))))

     (escape-glyph ((t (:foreground "#E6DB74"))))

     (minibuffer-prompt ((t (:foreground "#89BDEF"))))
     (mode-line-mousable ((t (:foreground "#BCBCBC" :background "#000000"))))
     (mode-line-mousable-minor-mode ((t (:foreground "#BCBCBC" :background "#000000"))))
     (widget-inactive-face ((t (:background "#ff0000"))))

     (fringe ((t (:background "#232526"))))

     (bold ((t (:weight bold))))
     (bold-italic ((t (:weight bold :slant italic))))
     (italic ((t (:slant italic))))
     (underline ((t (:underline t))))

     (icompletep-choices ((t (:foreground "#F92672"))))
     (icompletep-determined ((t (:foreground "#A6E22E"))))
     (icompletep-keys ((t (:foreground "#F92672"))))
     (icompletep-nb-candidates ((t (:foreground "#AE81FF"))))

     (markdown-italic-face ((t (:slant italic))))
     (markdown-bold-face ((t (:weight bold))))
     (markdown-header-face ((t (:weight normal))))
     (markdown-header-face-1 ((t (:foreground "#89BDEF"))))
     (markdown-header-face-2 ((t (:foreground "#F92672"))))
     (markdown-header-face-3 ((t (:foreground "#A6E22E"))))
     (markdown-header-face-4 ((t (:foreground "#AE81FF"))))
     (markdown-header-face-5 ((t (:foreground "#E6DB74"))))
     (markdown-header-face-6 ((t (:foreground "#89BDEF"))))
     (markdown-inline-code-face ((t (:foreground "#89BDEF"))))
     (markdown-list-face ((t (:foreground "#A6E22E"))))
     (markdown-blockquote-face ((t (:slant italic))))
     (markdown-pre-face ((t (:foreground "#AE81FF"))))
     (markdown-link-face ((t (:foreground "#89BDEF"))))
     (markdown-reference-face ((t (:foreground "#89BDEF"))))
     (markdown-url-face ((t (:foreground "#E6DB74"))))
     (markdown-link-title-face ((t (:foreground "#F92672"))))
     (markdown-comment-face ((t (:foreground "#465457"))))
     (markdown-math-face ((t (:foreground "#AE81FF" :slant italic))))

     (mumamo-background-chunk-major ((t (:background "#272822"))))
     (mumamo-background-chunk-submode ((t (:background "#1B1D1E"))))

     (outline-1 ((t (:foreground "#89BDEF"))))
     (outline-2 ((t (:foreground "#F92672"))))
     (outline-3 ((t (:foreground "#A6E22E"))))
     (outline-4 ((t (:foreground "#AE81FF"))))
     (outline-5 ((t (:foreground "#E6DB74"))))
     (outline-6 ((t (:foreground "#89BDEF"))))
     (outline-7 ((t (:foreground "#F92672"))))
     (outline-8 ((t (:foreground "#A6E22E"))))

     (woman-addition ((t (:foreground "#AE81FF"))))
     (woman-bold ((t (:foreground "#F92672"))))
     (woman-italic ((t (:foreground "#A6E22E"))))
     (woman-unknown ((t (:foreground "#89BDEF"))))
     )))

(provide 'color-theme-my-way)
