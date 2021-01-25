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
(setq doom-theme 'doom-gruvbox)

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
  :hook (org-mode . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :config
  (after! pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! org-noter
  :commands (org-noter)
  :config
  (add-hook! org-noter-notes-mode (require 'org-noter-pdftools)))
;; ;; (use-package! org-pdftools
;; ;;   :hook (org-load . org-pdftools-setup-link))
;; (use-package org-pdftools
;; 	:config
;; 	(with-eval-after-load 'org
;; 		(org-pdftools-setup-link)))

;; (use-package! org-noter-pdftools
;;   :config
;;   (after! pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;; ;; org-noter
;; (use-package! org-noter
;;   :commands (org-noter)
;;   :after (:any org pdf-view)
;;   :config
;; ;;  (setq
;;    ;; The WM can handle splits
;;    ;; org-noter-notes-window-location 'other-frame
;;    ;; Please stop opening frames
;;    ;; org-noter-always-create-frame nil
;;    ;; I want to see the whole file
;;    ;; org-noter-hide-other nil
;;    ;; Everything is relative to the rclone mega
;;    ;; org-noter-notes-search-path (list org_notes)
;; ;;   )
;;   (add-hook! org-noter-notes-mode (require 'org-noter-pdftools))
;;   )


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


(defun move-alltheway-left ()
  (image-backward-hscroll 500)
  )
(defun move-alltheway-right ()
  (image-forward-hscroll 500)
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
   (:prefix "m"
    (:desc "move left" "h"   'move-alltheway-left)
    (:desc "move right" "l"   'move-alltheway-right))
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


;;popups


(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :config
  (after! pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! org-noter
  :commands (org-noter)
  :config
  (add-hook! org-noter-notes-mode (require 'org-noter-pdftools)))
;; ;; (use-package! org-pdftools
;; ;;   :hook (org-load . org-pdftools-setup-link))
;; (use-package org-pdftools
;; 	:config
;; 	(with-eval-after-load 'org
;; 		(org-pdftools-setup-link)))

;; (use-package! org-noter-pdftools
;;   :config
;;   (after! pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;; ;; org-noter
;; (use-package! org-noter
;;   :commands (org-noter)
;;   :after (:any org pdf-view)
;;   :config
;; ;;  (setq
;;    ;; The WM can handle splits
;;    ;; org-noter-notes-window-location 'other-frame
;;    ;; Please stop opening frames
;;    ;; org-noter-always-create-frame nil
;;    ;; I want to see the whole file
;;    ;; org-noter-hide-other nil
;;    ;; Everything is relative to the rclone mega
;;    ;; org-noter-notes-search-path (list org_notes)
;; ;;   )
;;   (add-hook! org-noter-notes-mode (require 'org-noter-pdftools))
;;   )


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
  ;; (setq pdf-annot-activate-created-annotations t
  ;;       pdf-view-resize-factor 1.1)
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
(map!
 :g "C-<" #'pdf-annot-list-display-annotation-from-id)

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
(global-set-key (kbd "C-q") 'evil-avy-goto-char-2)

;;example for keybinding
;;(map! :leader
;;     :desc "Jump to char" "/ c" #'avy-goto-char)


(rg-enable-default-bindings)
;;popups
;; (defun open-popup-on-side-or-below (buffer &optional alist)
;;   (+popup-display-buffer-stacked-side-window-fn
;;    buffer (append `((side . ,(if (one-window-p)
;;                                  'right
;;                                'bottom)))
;;                   alist)))

;; ;; Wrap in an `after!' block so that you popup rule takes precedence over
;; ;; default ones.
;; (after! python
;;   ;; Your regexp was incorrect, I've corrected it here:
;;   (set-popup-rule! "^\\*rg" :actions '(open-popup-on-side-or-below)))
(set-popup-rule! "^\\*rg" :side 'right :size 0.4)


;; window split config
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)
;; vertical split by default
(setq split-height-threshold nil)
(setq split-width-threshold 0)


;; fix shaky ivy posframe
(after! ivy-posframe
  (defun ivy-posframe--display (str &optional poshandler)
    "Show STR in ivy's posframe with POSHANDLER."
    (message "posframe display ...")
    ;; NOTE: Added .width and .height variables
    (let ((.width 90)
          (.height 18))
      (if (not (posframe-workable-p))
          (ivy-display-function-fallback str)
        (with-ivy-window
          (apply #'posframe-show
                 ivy-posframe-buffer
                 :font ivy-posframe-font
                 :string str
                 :position (point)
                 :poshandler poshandler
                 ;; NOTE: width and height have been added, this is done in the
                 ;;       helm-posframe code (which doesn't jump around.)
                 :width
                 (max (cl-typecase .width
                        (integer .width)
                        (float (truncate (* (frame-width) .width)))
                        (function (funcall .width))
                        (t 0))
                      .width)
                 :height
                 (max (cl-typecase .height
                        (integer .height)
                        (float (truncate (* (frame-height) .height)))
                        (t 0))
                      .height)
                 :background-color (face-attribute 'ivy-posframe :background nil t)
                 :foreground-color (face-attribute 'ivy-posframe :foreground nil t)
                 :internal-border-width ivy-posframe-border-width
                 :internal-border-color (face-attribute 'ivy-posframe-border :background nil t)
                 :override-parameters ivy-posframe-parameters
                 (funcall ivy-posframe-size-function))
          (ivy-posframe--add-prompt 'ignore)))))

  (defun ivy-posframe--minibuffer-setup (fn &rest args)
    "Advice function of FN, `ivy--minibuffer-setup' with ARGS."
    (if (not (display-graphic-p))
        (apply fn args)
      (let ((ivy-fixed-height-minibuffer nil))
        (apply fn args))
      (when (and ivy-posframe-hide-minibuffer
                 (posframe-workable-p)
                 ;; if display-function is not a ivy-posframe style display-function.
                 ;; do not hide minibuffer.
                 ;; The hypothesis is that all ivy-posframe style display functions
                 ;; have ivy-posframe as name prefix, need improve!

                 ;; NOTE: This has been disabled
                 ;; (string-match-p "^ivy-posframe" (symbol-name ivy--display-function))
                 )
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'ivy-posframe t)
          (overlay-put ov 'face
                       (let ((bg-color (face-background 'default nil)))
                         `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil)))))

  (setq ivy-posframe-display-functions-alist
        '((t . +ivy-display-at-frame-center-near-bottom-fn))))


(use-package! centaur-tabs
  :demand
  :config
   (setq centaur-tabs-style "bar"
	  centaur-tabs-height 30
          centaur-tabs-set-icons t
	  centaur-tabs-set-modified-marker t
	  centaur-tabs-show-navigation-buttons t
	  centaur-tabs-set-bar 'under
	  x-underline-at-descent-line t
          centaur-tabs-label-fixed-length 15)
   (centaur-tabs-headline-match)
(centaur-tabs-change-fonts "P22 Underground Book" 160)
   ;; (setq centaur-tabs-gray-out-icons 'buffer)
   ;; (centaur-tabs-enable-buffer-reordering)
   ;; (setq centaur-tabs-adjust-buffer-order t)
   (centaur-tabs-mode t)
   (setq uniquify-separator "/")
   (setq uniquify-buffer-name-style 'forward)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
   (:map evil-normal-state-map
	  ("g t" . centaur-tabs-forward)
	  ("g T" . centaur-tabs-backward)))

;; (add-to-list 'load-path
;;               "~/.emacs.d/plugins/yasnippet")
;; (require 'yasnippet)
;; (yas-global-mode 1)


(after! yasnippet
  ;; set path to file templates
  (setq +file-templates-dir "~/doom.d/snippets/"))
;;(add-hook 'yas-minor-mode-hook (lambda() (yas-active-extra-mode 'fundamental-mode)))

 (use-package doom-snippets
   :load-path "~/doom.d/snippets/"
   :after yasnippet)

(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US")
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "de_DE")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))

;; bind save word to dict to key
(defun my-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(map! :localleader
      :map (org-mode-map)
      (:prefix ("D" . "Dict")
          :desc "add word" "a" 'my-save-word
          ))

;; thesaurus synomnyms
(setq synosaurus-backend 'synosaurus-backend-openthesaurus)

;; google translate
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "de")

(use-package google-translate
  :ensure t
  :custom
  (google-translate-backend-method 'curl)
  :config
   (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 0)) t))
