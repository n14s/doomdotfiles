;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq user-full-name "Nik"
      user-mail-address "n14s@web.de"
      )

;; Basic Stuff: Appearance etc
;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-Iosvkem)

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                                         ; Set width for tabs
 uniquify-buffer-name-style 'forward      ; Uniquify buffer names
 window-combination-resize t                    ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                                           ; Stretch cursor to the glyph width

(setq
      doom-font (font-spec :family "Iosevka" :size 24)
      display-line-numbers-type t
      undo-limit 80000000                          ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                             ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                                    ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t      ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                             ; Replace selection when inserting text
(display-time-mode 1)                                   ; Enable time in the mode-line
(global-subword-mode 1)                           ; Iterate through CamelCase words
(setq line-spacing 0.3)                                   ; seems like a nice line spacing balance.

;; org
(setq
   org_notes (concat (getenv "HOME") "/drive/org/")
   zot_bib (concat (getenv "HOME") "/drive/org/master.bib")
   org-directory org_notes
   deft-directory org_notes
   org-roam-directory org_notes
   )



(use-package! org-download
  :after org
  :config
  (setq-default org-download-image-dir "./images/"
                org-download-screenshot-method "gnome-screenshot -a -f %s"
                org-download-method 'directory
                org-download-heading-lvl 1
                )
  )




;; latex

;; Set after the default-packages list anyway
(after! org
(setq org-latex-packages-alist 'nil)
(setq org-latex-default-packages-alist
  '(("AUTO" "inputenc"  t ("pdflatex"))
    ("T1"   "fontenc"   t ("pdflatex"))
    (""     "graphicx"  t)
    (""     "grffile"   t)
    (""     "minted"   t)
    ;; ("dvipsnames,svgnames*,x11names*,table"     "xcolor"   t)
    (""     "longtable" nil)
    (""     "wrapfig"   nil)
    (""     "rotating"  nil)
    ("normalem" "ulem"  t)
    (""     "amsmath"   t)
    (""     "amssymb"   t)
    (""     "unicode-math"   t)
    (""     "mathtools"   t)
    (""     "textcomp"  t)
    (""     "capt-of"   nil)
    (""     "hyperref"  nil)))
;; (add-to-list 'org-latex-default-packages-alist '("" "fontspec" t))
;; (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
;; (add-to-list 'org-latex-packages-alist '("" "unicode-math"))
(plist-put org-format-latex-options :scale 2.2)
(add-to-list 'org-preview-latex-process-alist '(dvixelatex :programs
         ("xetex" "convert")
         :description "pdf > png" :message "you need to install the programs: xetex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
         (1.0 . 1.0)
         :latex-compiler
         ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
         :image-converter
         ("dvisvgm %f -n -b min -c %S -o %O")))

(add-to-list 'org-preview-latex-process-alist '(imagexetex :programs
         ("xelatex" "convert")
         :description "pdf > png" :message "you need to install the programs: xelatex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
         (1.0 . 1.0)
         :latex-compiler
         ("xelatex -interaction nonstopmode -output-directory %o %f")
         :image-converter
         ("convert -density %D -trim -antialias %f -quality 100 %O")))

;images for latex
(setq org-preview-latex-default-process 'imagexetex)
(setq org-startup-with-inline-images 'nil)
(setq org-image-actual-width 500)
)
;; Compiler
(setq org-latex-pdf-process (list "latexmk -shell-escape -f -xelatex %f"))


;; functions
;;

; Pdf
(defun haozeke/org-save-and-export-pdf ()
  (if (eq major-mode 'org-mode)
    (org-latex-export-to-pdf :async t)))

; LaTeX
(defun haozeke/org-save-and-export-latex ()
  (if (eq major-mode 'org-mode)
    (org-latex-export-to-latex)))

;; notes
;;

;; org-ref
(use-package! org-ref
    ;; :init
    ; code to run before loading org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list "/home/mrpeanutbutter/drive/org/master.bib")
         org-ref-bibliography-notes "/home/mrpeanutbutter/drive/org/bibnotes.org"
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "/home/mrpeanutbutter/drive/org"
         org-ref-notes-function 'orb-edit-notes
    ))

;; org-bibtex
(after! org-ref
  (setq
   bibtex-completion-notes-path "/home/mrpeanutbutter/drive/org"
   bibtex-completion-bibliography "/home/mrpeanutbutter/drive/org/master.bib"
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "#+ROAM_TAGS: ${keywords}\n"
    "* TODO Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n"
    )
   )
)

;; org-roam
;;
(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS:

- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

;; pdftools

(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :config
  (after! pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;; org-noter
(use-package! org-noter
  :commands (org-noter)
  :after (:any org pdf-view)
  :config
;;  (setq
   ;; The WM can handle splits
   ;; org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   ;; org-noter-always-create-frame nil
   ;; I want to see the whole file
   ;; org-noter-hide-other nil
   ;; Everything is relative to the rclone mega
   ;; org-noter-notes-search-path (list org_notes)
;;   )
  (add-hook! org-noter-notes-mode (require 'org-noter-pdftools))
  )


;; org-capture
;;; buffer size
(set-popup-rule! "^CAPTURE-.*\\.org$" :size 0.5 :quit nil :select t :autosave t)

;;; functions
;;;; Fix some link issues
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
  )

;;; templates
;;; Actually start using templates
(after! org-capture
  ;; Firefox
  (add-to-list 'org-capture-templates
               '("P" "Protocol" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
                 :prepend t
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("L" "Protocol Link" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
                 :prepend t
                 :kill-buffer t))
  ;; Misc
  (add-to-list 'org-capture-templates
         '("a"               ; key
           "Article"         ; name
           entry             ; type
           (file+headline +org-capture-notes-file "Article")  ; target
           "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?"  ; template
           :prepend t        ; properties
           :empty-lines 1    ; properties
           :created t        ; properties
           ))
)




;; keys
;;
(setq-default evil-escape-key-sequence "fd")
(map! :leader
      :desc "Open like spacemacs" "SPC" #'counsel-M-x)

(use-package! general)
;; Creating a constant for making future changes simpler
(defconst my-leader "SPC")
;; Tell general all about it
(general-create-definer my-leader-def
  :prefix my-leader)
  ;; :prefix my-leader)
;; (general-create-definer my-local-leader-def
;;   ;; :prefix my-local-leader
;;   :prefix "SPC m")

;; I like short names
(general-evil-setup t)
;; Stop telling me things begin with non-prefix keys
(general-auto-unbind-keys)

(general-define-key
 :keymaps '(insert visual normal)
 "C-SPC" 'evil-force-normal-state)

;; org noter
(map! :localleader
      :map (org-mode-map pdf-view-mode-map)
      (:prefix ("o" . "Org")
        (:prefix ("n" . "Noter")
          :desc "Noter" "n" 'org-noter
          )))

;; org
(after! org (map! :localleader
      :map org-mode-map
      :desc "Eval Block" "e" 'ober-eval-block-in-repl
      (:prefix "o"
        :desc "Tags" "t" 'org-set-tags
        :desc "Roam Bibtex" "b" 'orb-note-actions
        (:prefix ("p" . "Properties")
          :desc "Set" "s" 'org-set-property
          :desc "Delete" "d" 'org-delete-property
          :desc "Actions" "a" 'org-property-action
          )
        )
      (:prefix ("i" . "Insert")
        :desc "Link/Image" "l" 'org-insert-link
        :desc "Item" "o" 'org-toggle-item
        :desc "Citation" "c" 'org-ref-helm-insert-cite-link
        :desc "Footnote" "f" 'org-footnote-action
        :desc "Table" "t" 'org-table-create-or-convert-from-region
        :desc "Screenshot" "s" 'org-download-screenshot
        (:prefix ("h" . "Headings")
          :desc "Normal" "h" 'org-insert-heading
          :desc "Todo" "t" 'org-insert-todo-heading
          (:prefix ("s" . "Subheadings")

            :desc "Normal" "s" 'org-insert-subheading
            :desc "Todo" "t" 'org-insert-todo-subheading
            )
          )
        (:prefix ("e" . "Exports")
          :desc "Dispatch" "d" 'org-export-dispatch
          )
        )
      )
  )

;; pdf-tools
(after! pdf-view
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t
        pdf-view-resize-factor 1.1)
   ;; faster motion
 (map!
   :map pdf-view-mode-map
   :n "g g"          #'pdf-view-first-page
   :n "G"            #'pdf-view-last-page
   :n "J"            #'pdf-view-next-page-command
   :n "K"            #'pdf-view-previous-page-command
   :n "k"            #'evil-collection-pdf-view-previous-line-or-previous-page
   :n "j"            #'evil-collection-pdf-view-next-line-or-next-page
   :n "C-<"        #'pdf-annot-list-display-annotation-from-id
   :localleader
   (:prefix "o"
    (:prefix "n"
     :desc "Insert" "i" 'org-noter-insert-note
     ))
   )
; (add-hook 'pdf-annot-list-mode-hook (define-key 'evil-normal-state-local-map "<C-tab>" 'pdf-annot-list-display-annotation-from-id))
 )

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   :map helm-map
   ("C-j" . helm-next-line)
   ("C-k" . helm-previous-line))
)

(use-package helm-files
  :bind
  (:map helm-find-files-map
   ("C-h" . helm-find-files-up-one-level)
   ("C-l" . helm-execute-persistent-action))
)

; avy-goto-char with C-f
(define-key! evil-motion-state-map "C-f" nil)
(define-key! magit-mode-map "C-f" nil)
(define-key! :keymaps +default-minibuffer-maps "C-f" nil)
(map!
 :g "C-f" #'avy-goto-char
 (:map minibuffer-local-map
   "C-f" #'avy-goto-char))
(define-key minibuffer-local-map (kbd "C-f") #'avy-goto-char)
(global-set-key (kbd "C-f") 'avy-goto-char)

;;example for keybinding
;;(map! :leader
;;     :desc "Jump to char" "/ c" #'avy-goto-char)
