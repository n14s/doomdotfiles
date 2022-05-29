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
   org_notes (concat (getenv "HOME") "/Documents/notes/")
   org_doc (concat (getenv "HOME") "/Documents/")
   zot_bib (concat (getenv "HOME") "/drive/org/master.bib")
   org-directory org_notes
   deft-directory org_notes
   org-roam-directory "~/Documents/notes/roam"
   org-my-anki-file "~/drive/org/anki/anki.org"
   )

(defun efs/org-font-setup ()
;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Latin Modern Sans" :weight 'regular :height (cdr face))))

(use-package! org
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package! org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


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


;;############### jethro ###############
;;
(require 'org-habit)

(map! :leader
        :prefix "n"
        "c" #'org-capture)
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading)

  (defun jethro/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (require 'find-lisp)
  (setq jethro/org-agenda-directory (file-truename "~/drive/org/gtd/"))
  (setq org-agenda-files
        (find-lisp-find-files jethro/org-agenda-directory "\.org$"))

  )


;; Compiler
;; (setq org-latex-pdf-process (list "latexmk -shell-escape -f -xelatex %f"))
(setq org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -bibtex -f -output-directory=%o %f"))


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
         org-ref-bibliography-notes "/home/mrpeanutbutter/Documents/notes/roam/refs/bibnotes.org"
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "/home/mrpeanutbutter/Documents/notes/roam/refs"
         org-ref-notes-function 'orb-edit-notes
    ))

;; org-bibtex
;;
(setq org-latex-prefer-user-labels t)
(after! org-ref
  (setq
   bibtex-completion-notes-path "/home/mrpeanutbutter/Documents/notes/roam/refs"
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
    ":NOTER_DOCUMENT: ${file}"
;; %(orb-process-file-field \"${=key=}\")\n"
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


;;;;;;;;; old org-roam v1
;; (use-package! org-roam
;;   :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
;;   :hook
;;   (after-init . org-roam-mode)
;;   :init
;;   (map! :leader
;;         :prefix "n"
;;         :desc "org-roam" "l" #'org-roam
;;         :desc "org-roam-insert" "i" #'org-roam-insert
;;         :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
;;         :desc "org-roam-find-file" "f" #'org-roam-find-file
;;         :desc "org-roam-show-graph" "g" #'org-roam-show-graph
;;         :desc "org-roam-insert" "i" #'org-roam-insert
;;         :desc "org-roam-capture" "c" #'org-roam-capture
;;         :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
;;   (setq org-roam-directory (file-truename "~/Documents/notes/roam")
;;         org-roam-db-gc-threshold most-positive-fixnum
;;         org-roam-graph-exclude-matcher "private"
;;         org-roam-tag-sources '(prop last-directory)
;;         org-id-link-to-org-use-id t)
;;   :config
;;   (setq org-roam-capture-templates
;;         '(("d" "default" plain (function org-roam--capture-get-point)
;;            "%?"
;;            :file-name "${slug}"
;;            :head "#+title: ${title}\n"
;;            :immediate-finish t
;;            :unnarrowed t)
;;           ("p" "private" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "private/${slug}"
;;            :head "#+title: ${title}\n"
;;            :immediate-finish t
;;            :unnarrowed t)))
;;   (setq org-roam-capture-ref-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "${slug}"
;;            :head "#+roam_key: ${ref}
;; #+roam_tags: website
;; #+title: ${title}
;; - source :: ${ref}"
;;            :unnarrowed t)))
;;   (add-to-list 'org-capture-templates `("c" "org-protocol-capture" entry (file+olp ,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory) "The List")
;;                                          "* TO-READ [[%:link][%:description]] %^g"
;;                                          :immediate-finish t))
;;   (add-to-list 'org-agenda-custom-commands `("r" "Reading"
;;                                              ((todo "WRITING"
;;                                                     ((org-agenda-overriding-header "Writing")
;;                                                      (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory)))))
;;                                               (todo "READING"
;;                                                     ((org-agenda-overriding-header "Reading")
;;                                                      (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory)))))
;;                                               (todo "TO-READ"
;;                                                     ((org-agenda-overriding-header "To Read")
;;                                                      (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory))))))))
;;   (setq org-roam-dailies-directory "daily/")
;;   (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry
;;          #'org-roam-capture--get-point
;;          "* %?"
;;          :file-name "daily/%<%Y-%m-%d>"
;;          :head "#+title: %<%Y-%m-%d>\n\n")))
;;   (set-company-backend! 'org-mode '(company-capf)))


;; (general-def :states '(normal motion emacs) "SPC" nil)
;; org-roamv2
;;
;;
(setq org-roam-dailies-directory "journal/")
(setq nm/daily-note-filename "%<%Y-%m-%d>.org"
      nm/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")

(use-package! org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
(setq org-roam-dailies-directory "journal/")
  :custom
  (org-roam-dailies-capture-templates
`(("d" "default" entry
        "* %?"
        :if-new (file+head ,nm/daily-note-filename
                           ,nm/daily-note-header)
 :unnarrowed t)
("c" "check" plain (file "~/Documents/notes/roam/templates/check.org")
 :if-new (file+head ,nm/daily-note-filename
                    ,nm/daily-note-header)
 :unnarrowed t)
("s" "sleep" plain (file "~/Documents/notes/roam/templates/sleep.org")
 :if-new (file+head ,nm/daily-note-filename
                    ,nm/daily-note-header)
 :unnarrowed t)
("j" "journal" entry
        "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
        :if-new (file+head+olp ,nm/daily-note-filename
                               ,nm/daily-note-header
                               ("Log"))
 :unnarrowed t)
("m" "meeting" entry
      "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n%?\n"
      :if-new (file+head+olp ,nm/daily-note-filename
                             ,nm/daily-note-header
                             ("Meeting"))
 :unnarrowed t)
;;("t" "tasks" plain (file "~/Documents/notes/roam/templates/tasks.org")
;; :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
;; :unnarrowed t)
("t" "task" entry
 "* TODO %?\n  %U\n  %a\n  %i"
 :if-new (file+head+olp ,nm/daily-note-filename
                        ,nm/daily-note-header
                        ("Tasks"))
 :unnarrowed t
 :empty-lines 1)
      ))

  (org-roam-directory "~/Documents/notes/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point)
        :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-setup))

(defun n14/org-roam-goto-month ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: Project\n")
                                   :unnarrowed t))))

(defun n14/org-roam-goto-year ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: Project\n")
                                   :unnarrowed t))))
; hydra for roam
(defhydra n14/org-roam-jump-menu (:hint nil)
  "
^Dailies^        ^Capture^       ^Jump^
^^^^^^^^-------------------------------------------------
_t_: today       _T_: today       _m_: current month
_r_: tomorrow    _R_: tomorrow    _e_: current year
_y_: yesterday   _Y_: yesterday   ^ ^
_d_: date        ^ ^              ^ ^
"
  ("t" org-roam-dailies-goto-today :color blue)
  ("r" org-roam-dailies-goto-tomorrow :color blue)
  ("y" org-roam-dailies-goto-yesterday :color blue)
  ("d" org-roam-dailies-goto-date :color blue)
  ("T" org-roam-dailies-capture-today :color blue)
  ("R" org-roam-dailies-capture-tomorrow :color blue)
  ("Y" org-roam-dailies-capture-yesterday :color blue)
  ("m" n14/org-roam-goto-month :color blue)
  ("e" n14/org-roam-goto-year :color blue)
  ("c" nil "cancel"))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(after! org-ref
  (setq org-ref-default-bibliography `,(list (concat zot_bib))))

(use-package! org-roam-protocol
  :after org-protocol)

(defvar orb-title-format "${author-or-editor-abbrev} (${date}).  ${title}."
  "Format of the title to use for `orb-templates'.")

(use-package! org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref) ; optional: if Org Ref is not loaded anywhere else, load it here
  :requires bibtex-completion
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :load-path "~/.emacs.d/.local/straight/build-27.1/org-roam-bibtex"
  :bind (:map org-roam-bibtex-mode-map
         (("C-c m f" . orb-find-non-ref-file))
         :map org-mode-map
         (("C-c m t" . orb-insert-non-ref)
          ("C-c m a" . orb-note-actions)))
  :init
  :custom
  (orb-autokey-format "%a%y")
  (orb-templates
   `(("r" "ref" plain
      (function org-roam-capture--get-point)
      ""
      :file-name "refs/${citekey}"
      :head ,(s-join "\n"
                     (list
                      (concat "#+title: "
                              orb-title-format)
                      "#+roam_key: ${ref}"
                      "#+created: %U"
                      "#+last_modified: %U\n\n"))
      :unnarrowed t)
     ("p" "ref + physical" plain
      (function org-roam-capture--get-point)
      ""
      :file-name "refs/${citekey}"
      :head ,(s-join "\n"
                     (list
                      (concat "#+title: "
                              orb-title-format)
                      "#+roam_key: ${ref}"
                      ""
                      "* Notes :physical:")))
     ("n" "ref + noter" plain
      (function org-roam-capture--get-point)
      ""
      :file-name "refs/${citekey}"
      :head ,(s-join "\n"
                     (list
                      (concat "#+title: "
                              orb-title-format)
                      "#+roam_key: ${ref}"
                      ""
                      "* Notes :noter:"
                      ":PROPERTIES:"
                      ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")"
                      ":NOTER_PAGE:"
                      ":END:"))))))


;; previous orb
;;(use-package! org-roam-bibtex
;;:after (org-roam)
;;:hook (org-roam-mode . org-roam-bibtex-mode)
;;:config
;;(setq org-roam-bibtex-preformat-keywords
;;'("=key=" "title" "url" "file" "author-or-editor" "keywords"))
;;(setq orb-templates
;;'(("r" "ref" plain (function org-roam-capture--get-point)
;;""
;;:file-name "${slug}"
;;:head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS:
;;
;;- keywords :: ${keywords}
;;
;;\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
;;
;;:unnarrowed t))))


;; org noter
(after! org-noter
  org-noter-doc-split-fraction '(0.37 0.37))


(use-package! pdf-tools
  :config
(add-hook! 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode))

;; pdftools

;;(use-package! org-pdftools
;;  :hook (org-mode . org-pdftools-setup-link))
;;
;;(use-package! org-noter-pdftools
;;  :config
;;  (after! pdf-annot
;;    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
;;
;;(use-package! org-noter
;;  :commands (org-noter)
;;  :config
;;  (add-hook! org-noter-notes-mode (require 'org-noter-pdftools)))
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

;;;;;;;;;;;;;;;;;;
;;;;;;;; org noter config copied from https://github.com/zaeph/.emacs.d/blob/master/init.el
(use-package! org-noter
  :bind (:map org-mode-map
         (("C-c N" . zp/org-noter-dwim))
         :map org-noter-doc-mode-map
         (("M-i" . zp/org-noter-insert-precise-note-dwim)))
  :config
  (setq org-noter-hide-other t
        org-noter-auto-save-last-location t
        org-noter-doc-split-fraction '(0.57 0.43))

  (defun zp/org-noter-visual-line-mode ()
    "Enable visual-line-mode in ‘org-noter’ notes.
Workaround to counter race conditions with the margins."
    (let ((parent (current-buffer))
          (refresh (lambda (parent)
                     (with-current-buffer parent
                       (visual-line-mode 'toggle)
                       (visual-line-mode 'toggle)))))
      (run-at-time "1 sec" nil refresh parent)
      (run-at-time "5 sec" nil refresh parent)))

  (add-hook 'org-noter-notes-mode-hook #'zp/org-noter-visual-line-mode)

  ;; Fix for hiding truncation
  (defun org-noter--set-notes-scroll (_window &rest _ignored)
    nil)

  ;; Fix for visual-line-mode with PDF files
  (defun org-noter--note-after-tipping-point (_point _note-property _view)
    nil)

  (defun zp/org-noter-indirect (arg)
    "Ensure that org-noter starts in an indirect buffer.
Without this wrapper, org-noter creates a direct buffer
restricted to the notes, but this causes problems with the refile
system.  Namely, the notes buffer gets identified as an
agenda-files buffer.
This wrapper addresses it by having org-noter act on an indirect
buffer, thereby propagating the indirectness."
    (interactive "P")
    (if (org-entry-get nil org-noter-property-doc-file)
        (with-selected-window (zp/org-tree-to-indirect-buffer-folded nil t)
          (org-noter arg)
          (kill-buffer))
      (org-noter arg)))

  (defun zp/org-noter-dwim (arg)
    "Run org-noter on the current tree, even if we’re in the agenda."
    (interactive "P")
    (let ((in-agenda (derived-mode-p 'org-agenda-mode))
          (marker))
      (cond (in-agenda
             (setq marker (get-text-property (point) 'org-marker))
             (with-current-buffer (marker-buffer marker)
               (goto-char marker)
               (unless (org-entry-get nil org-noter-property-doc-file)
                 (user-error "No org-noter info on this tree"))
               (zp/org-noter-indirect arg)))
            (t
             (zp/org-noter-indirect arg)
             (setq marker (point-marker))))
      (org-with-point-at marker
        (let ((tags (org-get-tags)))
          (when (and (org-entry-get nil org-noter-property-doc-file)
                     (not (member "noter" tags)))
            (org-set-tags (push "noter" tags)))))
      (unless in-agenda
        (set-marker marker nil))))

  (defun zp/org-noter-insert-precise-note-dwim (force-mouse)
    "Insert note associated with a specific location.
If in nov-mode, use point rather than the mouse to target the
position."
    (interactive "P")
    (if (and (derived-mode-p 'nov-mode)
             (not force-mouse))
        (let ((pos (if (region-active-p)
                       (min (region-beginning) (point))
                     (point))))
          (org-noter-insert-note pos))
      (org-noter-insert-precise-note)))

  (define-key org-noter-doc-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key org-noter-doc-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page))
;;;;;;;; end of org noter

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
;;(after! org-capture
;;  ;; Firefox
;;
;;  (add-to-list 'org-capture-templates
;;               '("P" "Protocol" entry
;;                 (file+headline +org-capture-notes-file "Inbox")
;;                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
;;                 :prepend t
;;                 :kill-buffer t))
;;  (add-to-list 'org-capture-templates
;;               '("L" "Protocol Link" entry
;;                 (file+headline +org-capture-notes-file "Inbox")
;;                 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
;;                 :prepend t
;;                 :kill-buffer t))
;;  ;; Misc
;;  (add-to-list 'org-capture-templates
;;         '("R"               ; key
;;           "Article"         ; name
;;           entry             ; type
;;           (file+headline +org-capture-notes-file "Article")  ; target
;;           "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?"  ; template
;;           :prepend t        ; properties
;;           :empty-lines 1    ; properties
;;           :created t        ; properties
;;           ))
;;
;;)

;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

;; #################################################################
;; org agenda ################################################


; ############## jethro setup ###############


(setq org-capture-templates
        `(("i" "Inbox" entry (file ,(expand-file-name "inbox.org" jethro/org-agenda-directory))
           ,(concat "* TODO %?\n"
                    "/Entered on/ %u"))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-todo-keyword-faces
'(("inbox::" . "grey"))
      )

(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(setq org-tag-alist '(("@errand" . ?e)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      (:newline)
                      ("CANCELLED" . ?c)))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-fast-tag-selection-single-key nil)
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:level . 1))))

(defvar jethro/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun jethro/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (jethro/bulk-process-entries))

(defvar jethro/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun jethro/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " jethro/org-current-effort) nil nil jethro/org-current-effort)))
  (setq jethro/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun jethro/org-agenda-process-inbox-item ()
(interactive)
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   ;;(org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'jethro/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun jethro/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun jethro/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

(map! :map org-agenda-mode-map
      "i" #'org-agenda-clock-in
      "I" #'jethro/clock-in-and-advance
      "r" #'jethro/org-process-inbox
      "R" #'org-agenda-refile
      "c" #'jethro/org-inbox-capture)

(defun jethro/advance-todo ()
  (org-todo 'right)
  (remove-hook 'org-clock-in-hook #'jethro/advance-todo))

(defun jethro/clock-in-and-advance ()
  (interactive)
  (add-hook 'org-clock-in-hook 'jethro/advance-todo)
  (org-agenda-clock-in))

(use-package! org-clock-convenience
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("o" . org-clock-convenience-fill-gap)
              ("e" . org-clock-convenience-fill-gap-both)))

(use-package! org-agenda
  :init
  (map! "<f1>" #'jethro/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  (defun jethro/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  :config
  (defun jethro/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

  (defun jethro/skip-projects ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((jethro/is-project-p)
        next-headline)
       (t
        nil)))))

  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands `((" " "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'day)
                                                (org-agenda-ndays 1)
	                                        (org-agenda-start-on-weekday nil)
	                                        (org-agenda-start-day "+0d")
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files '(,(expand-file-name "next.org" jethro/org-agenda-directory)))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files '(,(expand-file-name "projects.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Emails")
                                              (org-agenda-files '(,(expand-file-name "emails.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Inbox")
                                              (org-agenda-files '(,(expand-file-name "inbox.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Buero")
                                              (org-agenda-files '(,(expand-file-name "buero.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Work")
                                              (org-agenda-files '(,(expand-file-name "work.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Active Projects")
                                              (org-agenda-skip-function #'jethro/skip-projects)
                                              (org-agenda-files '(,(expand-file-name "projects.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Computer")
                                              (org-agenda-files '(,(expand-file-name "computer.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Friends")
                                              (org-agenda-files '(,(expand-file-name "friends.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Events")
                                              (org-agenda-files '(,(expand-file-name "events.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Family")
                                              (org-agenda-files '(,(expand-file-name "family.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Read")
                                              (org-agenda-files '(,(expand-file-name "reading.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Watch")
                                              (org-agenda-files '(,(expand-file-name "watching.org" jethro/org-agenda-directory)))))
                                        (todo "TODO"
                                             ((org-agenda-overriding-header "Someday")
                                              (org-agenda-files '(,(expand-file-name "someday.org" jethro/org-agenda-directory)))))
                                        (agenda "" ((org-agenda-overriding-header "Weekly Log")
                                                    (org-agenda-start-day "+0d")
                                                    ))
                                        ))
                          ;          (
                          ;     ("P" "Weekly Log"
                          ;           (agenda "" ((org-agenda-overriding-header "Weekly Log"))))
                          ;           )
                                     )))




;; ############## super agenda ###########
;;(setq
;;
;;org-agenda-time-grid
;;
;;(quote
;;
;;((weekly today remove-match)
;;
;;(900 1100 1300 1500 1700)
;;
;;"......" "----------------"))
;;
;;org-agenda-skip-deadline-if-done t
;;
;;org-agenda-skip-scheduled-if-done t
;;
;;org-super-agenda-mode t
;;
;;org-agenda-include-diary nil
;;
;;org-agenda-skip-scheduled-if-deadline-is-shown t
;;
;;org-agenda-block-separator nil
;;
;;org-agenda-compact-blocks t
;;
;;org-agenda-dim-blocked-tasks t
;;
;;org-agenda-start-with-log-mode t
;;
;;org-agenda-todo-list-sublevels t
;;
;;org-agenda-include-deadlines t)
;;
;;(use-package! org-super-agenda
;;  :config
;;
;;(setq org-agenda-custom-commands
;;      '(("z" "Super zaen view"
;;         ((agenda "" ((org-agenda-span 'day)
;;                      (org-super-agenda-groups
;;                       '((:name "Today"
;;                                :time-grid t
;;                                :date today
;;                                :todo "TODAY"
;;                                :scheduled today
;;                                :order 1)))))
;;          (alltodo "" ((org-agenda-overriding-header "")
;;                       (org-super-agenda-groups
;;                        '((:name "Next to do"
;;                                 :todo "NEXT"
;;                                 :order 1)
;;                          (:name "Important"
;;                                 :tag "Important"
;;                                 :priority "A"
;;                                 :order 6)
;;                          (:name "Due Today"
;;                                 :deadline today
;;                                 :order 2)
;;                          (:name "Due Soon"
;;                                 :deadline future
;;                                 :order 8)
;;                          (:name "Overdue"
;;                                 :deadline past
;;                                 :order 7)
;;                          (:name "Assignments"
;;                                 :tag "Assignment"
;;                                 :order 10)
;;                          (:name "Issues"
;;                                 :tag "Issue"
;;                                 :order 12)
;;                          (:name "Projects"
;;                                 :tag "Project"
;;                                 :order 14)
;;                          (:name "Emacs"
;;                                 :tag "Emacs"
;;                                 :order 13)
;;                          (:name "Research"
;;                                 :tag "Research"
;;                                 :order 15)
;;                          (:name "To read"
;;                                 :tag "Read"
;;                                 :order 30)
;;                          (:name "Waiting"
;;                                 :todo "WAITING"
;;                                 :order 20)
;;                          (:name "trivial"
;;                                 :priority<= "C"
;;                                 :tag ("Trivial" "Unimportant")
;;                                 :todo ("SOMEDAY" )
;;                                 :order 90)
;;                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
;; )




;; -#############################################################

;; 3224903248934980342908234908324902398042390832490324;;

;;;:;;;;;;; ja moin kurzer test
;; configure org noter stuff (from github readme)
(use-package! org-noter
 ; :config
 ; ;; Your org-noter config ........
 ; ""  (require 'org-noter-pdftools)
  )


;; 2021-10 trying with only orb
(use-package! org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (abrehaSelfAdaptiveMonitoringFog2020org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
;;;;;;;;;;; ja moin kurzer test




;; ;; changed 25.1.21
;;(use-package! org-pdftools
;;  :hook (org-mode . org-pdftools-setup-link))
;;
;;(use-package! org-noter-pdftools
;;  :config
;;  (after! pdf-annot
;;    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; ;; changed 25.1.21
;;(use-package! org-noter
;;  :commands (org-noter)
;;  :config
;;  (add-hook! org-noter-notes-mode (require 'org-noter-pdftools)))


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
  ;;
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
         '("R"               ; key
           "Article"         ; name
           entry             ; type
           (file+headline +org-capture-notes-file "Article")  ; target
           "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?"  ; template
           :prepend t        ; properties
           :empty-lines 1    ; properties
           :created t        ; properties
           ))
(add-to-list 'org-capture-templates
             '("a" "Anki basic"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n"
               :prepend t        ; properties
               :empty-lines 1    ; properties
               :created t
               ))
(add-to-list 'org-capture-templates
             '("A" "Anki cloze"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n"))

)


(use-package! centaur-tabs
  :demand
  :config
   (setq centaur-tabs-style "bar"
	  centaur-tabs-height 30
          centaur-tabs-set-icons t
	  centaur-tabs-set-modified-marker t
	  centaur-tabs-show-navigation-buttons t
          centaur-tabs-gray-out-icons 'buffer
	  centaur-tabs-set-bar 'over
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



(use-package! eshell-toggle
:custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :bind
  ("C-S-e" . eshell-toggle))




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

;; thesaurus synomnyms
;; (setq synosaurus-backend 'synosaurus-backend-openthesaurus)
(setq synosaurus-backend 'synosaurus-backend-wordnet)

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


(use-package anki-editor
  :after org-noter
  :config
  ; I like making decks
  (setq anki-editor-create-decks 't))




;;(use-package! lsp-pyright
;;  :ensure t
;;  :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))  ; or lsp-deferred
;; ############################################################################################################################################################
;; ################################  K E Y S  #################################################################################################################
;; ############################################################################################################################################################
;; ############################################################################################################################################################
;;
(use-package! ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-down)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-S-l" . ivy-immediate-done)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


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

(general-define-key
 :keymaps 'pdf-view-mode-map
 :prefix "g"
 "p" 'pdf-view-goto-page
 )

(after! pdf-tools
  (map! :map pdf-view-mode-map "C-s" #'pdf-occur)
  (map! :map pdf-view-mode-map "C-n" #'pdf-occur-next-error)
  (map! :map pdf-view-mode-map "C-," #'pdf-occur-next-error)
  )



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
        :desc "Subheading" "h" 'org-insert-subheading

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
;;(after! pdf-view
;;  ;; open pdfs scaled to fit page
;;  (setq-default pdf-view-display-size 'fit-width)
;;  ;; automatically annotate highlights
;;  (setq pdf-annot-activate-created-annotations t
;;        pdf-view-resize-factor 1.1)
;;   ;; faster motion
;; (map!
;;   :map pdf-view-mode-map
;;   :n "g g"          #'pdf-view-first-page
;;   :n "G"            #'pdf-view-last-page
;;   :n "J"            #'pdf-view-next-page-command
;;   :n "K"            #'pdf-view-previous-page-command
;;   :n "k"            #'evil-collection-pdf-view-previous-line-or-previous-page
;;   :n "j"            #'evil-collection-pdf-view-next-line-or-next-page
;;   :n "C-<"        #'pdf-annot-list-display-annotation-from-id
;;   :localleader
;;   (:prefix "o"
;;    (:prefix "n"
;;     :desc "Insert" "i" 'org-noter-insert-note
;;     ))
;;   (:prefix "m"
;;    (:desc "move left" "h"   'move-alltheway-left)
;;    (:desc "move right" "l"   'move-alltheway-right))
;;   )
;;; (add-hook 'pdf-annot-list-mode-hook (define-key 'evil-normal-state-local-map "<C-tab>" 'pdf-annot-list-display-annotation-from-id))
;; )
;;(map!
;; :g "C-<" #'pdf-annot-list-display-annotation-from-id)

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
;
(define-key! evil-motion-state-map "C-f" 'evil-scroll-page-down)
(define-key! magit-mode-map "C-f" 'evil-scroll-page-down)
(define-key! :keymaps +default-minibuffer-maps "C-f" 'evil-scroll-page-down)
(map!
 :g "M-f" #'avy-goto-char
 (:map minibuffer-local-map
   "M-f" #'avy-goto-char))
(define-key minibuffer-local-map (kbd "M-f") #'avy-goto-char)
(global-set-key (kbd "C-q") 'avy-goto-char)
(global-set-key (kbd "C-z") 'evil-avy-goto-char-2)

(global-set-key (kbd "M-q") 'centaur-tabs-backward)
(global-set-key (kbd "M-w") 'centaur-tabs-forward)


(global-set-key (kbd "C-S-a") 'eshell)
;;example for keybinding
;;(map! :leader
;;     :desc "Jump to char" "/ c" #'avy-goto-char)
;;popups

;; capture
(global-set-key (kbd "C-c c") 'org-capture)







;; org noter
;;(map! :localleader
;;      :map (org-mode-map pdf-view-mode-map)
;;      (:prefix ("o" . "Org")
;;        (:prefix ("n" . "Noter")
;;          :desc "Noter" "n" 'org-noter
;;          :desc "window location" "w" 'org-noter-set-notes-window-location
;;          )))

(map! :leader
      :map (org-mode-map pdf-view-mode-map)
      (:prefix ("o" . "Org")
        (:prefix ("n" . "Noter")
          :desc "Noter" "n" 'org-noter
          :desc "window location" "w" 'org-noter-set-notes-window-location
          )))

;; org links
(map! :leader
      :map (org-mode-map)
      (:prefix ("l" . "link")
          :desc "store" "s" 'org-store-link
          :desc "insert" "i" 'org-insert-link
          ))

(map! :leader
      (:prefix ("l" . "link")
          :desc "follow" "f" 'link-hint-open-link-at-point
          ))



;; open link
;;(map! :leader
;;      (:prefix ("o" . "open")
;;          :desc "link" "l" 'link-hint-open-link-at-point
;;          ))


;;(map!
;;      :map (pdf-view-mode-map)
;;      (:prefix ("g" . "goto")
;;          :desc "page" "p" 'pdf-view-goto-page
;;          ))



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



(map! :localleader
      :map (org-mode-map)
      (:prefix ("D" . "Dict")
          :desc "add word" "a" 'my-save-word
          ))




(map! :localleader
      :map (org-mode-map)
      (:prefix ("w" . "words")
          :desc "transl e-g" "t" 'google-translate-at-point
          :desc "transl g-e" "r" 'google-translate-at-point-reverse
          :desc "synonym" "s" 'synosaurus-lookup
          ))

(map! :localleader
      :map (org-mode-map)
      (:prefix ("i" . "words")
          :desc "transl e-g" "t" 'google-translate-at-point
          :desc "transl g-e" "r" 'google-translate-at-point-reverse
          :desc "synonym" "s" 'synosaurus-lookup
          ))


(map! :localleader
      :map (org-mode-map)
      (:prefix ("y" . "notnotes")
       :desc "backlinks" "l" 'org-roam-buffer-toggle
       :desc "find" "f" 'org-roam-node-find
          :desc "insert" "i" 'org-roam-node-insert
          ))

;; Make ESC quit stuff like C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;: configure helpful
(use-package! helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-commad] . helpful-command) ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(setq dired-listing-switches "-aXlh --group-directories-first")


;; window movenment
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)


(use-package! switch-window
  :ensure t
  :bind
  ;; default C-x o is other-window
  ;; default C-x C-o is delete-blank-lines
  (("C-x o" . switch-window)
   ("C-x C-o" . switch-window))
  :config
  (setq switch-window-multiple-frames t)
  (setq switch-window-shortcut-style 'qwerty)
  ;; when Emacs is run as client, the first shortcut does not appear
  ;; "x" acts as a dummy; remove first entry if not running server
  (setq switch-window-qwerty-shortcuts '("x" "a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "r" "u" "i" "o" "q" "t" "y" "p"))
  (setq switch-window-increase 3))

;; org find file in notes

(global-set-key (kbd "C-c f") '+default/find-in-notes)

;; org-roam
;;

(use-package! exec-path-from-shell
:init
(exec-path-from-shell-initialize)
 )


;; org-export
(add-to-list 'org-latex-classes
             '("bjmarticle"
               "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage{natbib}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{geometry}
\\geometry{a4paper,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file")
      orb-process-file-keyword t
      orb-file-field-extensions '("pdf"))

(setq org-roam-capture-templates
      '(("r" "bibliography reference" plain
         (file "/home/mrpeanutbutter/Documents/notes/roam/templates/roam-ref.org")
         :target
         (file+head "refs/${citekey}.org" "#+title: ${title}\n")
         :unnarrowed t)
      ("d" "default" plain
  (file "/home/mrpeanutbutter/Documents/notes/roam/templates/roam-def.org")
  :target
  (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
  :unnarrowed t)
      )
      )

;; (setq org-noter-notes-search-path "/home/mrpeanutbutter/Documents/notes/roam/refs")

;; make highlight in treemacs more visible
(defface custom-line-highlight '((t (:background "darkred" :extend t))) "")
(add-hook
 'treemacs-mode-hook
 (defun channge-hl-line-mode ()
   (setq-local hl-line-face 'custom-line-highlight)
   (overlay-put hl-line-overlay 'face hl-line-face)
   (treemacs--setup-icon-background-colors)))

;; helm-bibtex and org-ref insert link
;(map! :leader
;          :desc "helm-bitex" "r" 'helm-bibtex)
;(map! :localleader
;      :map (org-mode-map)
;          :desc "helm-bitex" "r" 'helm-bibtex)

;(map! :leader
;      :map (org-mode-map)
;      (:prefix ("r" . "bib/ref")
;       :desc "helm-bibtex" "r" 'helm-bibtex
;          :desc "insert" "i" 'org-ref-insert-link
;          ))

(map! :leader
      "r" nil
      (:prefix ("r" . "bib/ref")
       :desc "helm-bibtex" "r" 'helm-bibtex
          :desc "insert" "i" 'org-ref-insert-link
          ))

(map! :leader
      (:prefix ("g" . "git")
       :desc "visualize" "v" 'magit-log-all-branches
          ))



(map! :leader
      :map (python-mode-map)
      (:prefix ("c" . "code")
       :desc "pyenv activate" "p" 'pyvenv-activate
       :desc "restart lsp" "r" 'lsp-workspace-restart
          ))



(global-set-key (kbd "Q") 'evil-execute-macro)



;; pyvenv
(use-package! pyvenv
  :diminish
  :config
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-[") 'insert-pair)

; faces
; add magit status file font bg color to distinguish better
(custom-set-faces!
  '(magit-diff-file-heading :background "#3b0024")
  '(magit-branch-current :background "#73092b"))


; find faces underneath point
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(global-set-key (kbd "M-#") 'what-face)


;;
;; go for emacs
;;
;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)


(format-all-mode -1)

(setq auth-sources '("~/.authinfo"))

(defun clone-window-to-new-frame ()
  (+evil-window-vsplit-a)
  (centaur-tabs-extract-window-to-new-frame)
  )

(map! :leader
        (:prefix ("w" . "window")
          :desc "clone" "c" 'clone-window-to-new-frame
         ))

(map! :leader
        (:prefix ("n" . "notes")
          :desc "node insert" "i" 'org-roam-node-insert
          :desc "find note" "f" '+default/find-in-notes
          :desc "node find" "F" 'org-roam-node-find
          :desc "node immediate" "I" 'org-roam-node-insert-immediate
          :desc "dailies" "d" 'n14/org-roam-jump-menu/body
         ))

; hydra for modes
(defhydra n14/hydra-modes (:hint nil)
;  "
;^Buffer^        ^Package^       ^Window^
;^^^^^^^^-------------------------------------------------
;_t_: truncate    _o_: orb-mode     ^ ^
;^ ^              ^ ^              ^ ^
;^ ^              ^ ^              ^ ^
;"

  ("t" toggle-truncate-lines "truncate" :color blue)
  ("o" org-roam-bibtex-mode "orb" :color blue)
  ("l" linum-mode "linum" :color blue)
  ("i" org-toggle-inline-images "images" :color blue)
  ("c" nil "cancel"))

(map! :leader
        (:prefix ("m" . "misc")
          :desc "hydra modes" "m" 'n14/hydra-modes/body
         ))

; hydra for error navigation
(defhydra n14/hydra-error (:hint nil)
;  "
;^Error navigation^
;^^^^^^^^-------------------------------------------------
;_f_: first       _j_: next        _k_: prev        _v_: recenter
;"
("h" first-error "first")
("j" next-error "next")
("k" previous-error "prev")
("v" recenter-top-bottom "recenter")
("q" nil "quit"))

(map! :leader
        (:prefix ("m" . "misc")
          :desc "hydra error" "e" 'n14/hydra-error/body
         ))

; hydra for noter
(defhydra n14/hydra-noter (:hint nil)
("n" org-noter "init" :color blue)
("w" org-noter-set-notes-window-location "window")
("c" org-noter-sync-current-note "current")
("p" org-noter-sync-current-page-or-chapter "page")
("j" org-noter-sync-next-note "next")
("k" org-noter-sync-prev-note "prev")
("q" nil "quit"))

(map! :leader
        (:prefix ("m" . "misc")
          :desc "hydra noter" "n" 'n14/hydra-noter/body
         ))

; hydra for lang
(defhydra n14/hydra-lang (:hint nil)
("e" google-translate-at-point-reverse "de-eng" :color blue)
("d" google-translate-at-point "eng-de" :color blue)
("s" synosaurus-lookup "synonym" :color blue)
("S" +lookup/synonyms "synonym2" :color blue)
("c" ispell-region "spellcheck" :color blue)
("q" nil "quit"))

(map! :leader
        (:prefix ("m" . "misc")
          :desc "hydra lang" "l" 'n14/hydra-lang/body
         ))


