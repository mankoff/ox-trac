;;; ox-trac.el --- Trac Flavored Markdown Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Lars Tveito (ox-gmf.el)
;; Copyright (C) 2024 Ken Mankoff

;; Author: Ken Mankoff
;; Package-Version: 20241016
;; Package-Revision: tbd
;; Keywords: org, wp, markdown, trac

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Markdown back-end (Trac flavor) for Org
;; exporter, based on the `md' back-end.

;;; Code:

(require 'ox-md)
(require 'ox-publish)

;;; User-Configurable Variables

(defgroup org-export-trac nil
  "Options specific to Markdown export back-end."
  :tag "Org Trac Flavored Markdown"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

;;; Define Back-End

(org-export-define-derived-backend 'trac 'md
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?g "Export to Trac Flavored Markdown"
       ((?T "To temporary buffer"
            (lambda (a s v b) (org-trac-export-as-markdown a s v)))
        (?t "To file" (lambda (a s v b) (org-trac-export-to-markdown a s v)))
        (?7 "To file and open"
            (lambda (a s v b)
              (if a (org-trac-export-to-markdown t s v)
                (org-open-file (org-trac-export-to-markdown nil s v)))))))
  :translate-alist '((inner-template . org-trac-inner-template)
                     (paragraph . org-trac-paragraph)
                     (strike-through . org-trac-strike-through)
                     (subscript . org-trac-subscript)
                     (example-block . org-trac-example-block)
                     (src-block . org-trac-src-block)
                     (headline . org-trac-headline)
                     (table-cell . org-trac-table-cell)
                     (table-row . org-trac-table-row)
                     (table . org-trac-table)))

;;; Transcode Functions

;;;; Headline

(defun org-trac-headline (headline contents info)
  "Transcode HEADLINE element into Trac syntax.
CONTENTS holds the contents of the headline.
INFO is a plist used as a communication channel."
  (let* ((level (org-export-get-relative-level headline info))
         (title (org-export-data (org-element-property :title headline) info))
         (trac-heading (make-string (+ 1 level) ?=)))  ;; Create trac-style heading with '='
    (concat trac-heading " " title "\n" contents)))

;; (defun org-trac-headline (headline contents info)
;;   "Transcode HEADLINE object into Trac format.
;; CONTENTS is the headline contents. INFO is a plist used as a
;; communication channel."
;;   (message headline)
;;   (let* ((level (+ (org-export-get-relative-level headline info))))
;;     (concat (make-string level ?X) " a "  " b " contents)))
;;          ;; (format "== %s" level)))
;;     ;; (concat (org-md--headline-title style level heading anchor tags)
;;     ;;          contents)))))))
;; ;;# contents)))

;;;; Paragraph

(defun org-trac-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Trac Flavoured Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as a
communication channel."
  (unless (plist-get info :preserve-breaks)
    (setq contents (concat (mapconcat 'identity (split-string contents) " ") "\n")))
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-match "\\`#" first-object))
        (replace-regexp-in-string "\\`#" "\\#" contents nil t)
      contents)))

;;;; Src Block

(defun org-trac-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Trac Flavored Markdown format.
_CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((lang (org-element-property :language src-block))
         (lang (if (string= lang "f90") "fortran" lang))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "{{{#!" lang "\n"))
         (suffix "}}}"))
    (concat prefix code suffix)))

;;;; Example Block

;(defalias 'org-trac-example-block #'org-trac-src-block)
(defun org-trac-example-block (src-block _contents info)
  "Transcode EXAMPLE-BLOCK element into Trac Flavored Markdown format.
_CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((lang (org-element-property :language src-block))
         (lang (if (string= lang "f90") "fortran" lang))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "{{{" lang "\n"))
         (suffix "}}}"))
    (concat prefix code suffix)))

;;;; Strike-Through

(defun org-trac-strike-through (_strike-through contents _info)
  "Transcode _STRIKE-THROUGH from Org to Markdown (TRAC).
CONTENTS is the text with strike-through markup.  _INFO is a plist
holding contextual information."
  (format "~~%s~~" contents))

;;;; Subscript

(defun org-trac-subscript (subscript contents info)
  "Transcode SUBSCRIPT from Org to Markdown (TRAC).
CONTENTS is the text with subscript markup. INFO is a plist
holding contextual information."
  (format "_%s" contents))

;;;; Table-Common

(defvar width-cookies nil)
(defvar width-cookies-table nil)

(defconst trac-table-left-border "||")
(defconst trac-table-right-border " ||")
(defconst trac-table-separator " ||")

(defun org-trac-table-col-width (table column info)
  "Return width of TABLE at given COLUMN.
INFO is a plist used as communication channel.  Width of a column
is determined either by inquerying `width-cookies' in the column,
or by the maximum cell with in the column."
  (let ((cookie (when (hash-table-p width-cookies)
                  (gethash column width-cookies))))
    (if (and (eq table width-cookies-table)
             (not (eq nil cookie)))
        cookie
      (progn
        (unless (and (eq table width-cookies-table)
                     (hash-table-p width-cookies))
          (setq width-cookies (make-hash-table))
          (setq width-cookies-table table))
        (let ((max-width 0)
              (specialp (org-export-table-has-special-column-p table)))
          (org-element-map
              table
              'table-row
            (lambda (row)
              (setq max-width
                    (max (length
                          (org-export-data
                           (org-element-contents
                            (elt (if specialp (car (org-element-contents row))
                                   (org-element-contents row))
                                 column))
                           info))
                         max-width)))
            info)
          (puthash column max-width width-cookies))))))

(defun org-trac-make-hline-builder (table info char)
  "Return a function to build horizontal line in TABLE with given CHAR.
INFO is a plist used as a communication channel."
  (lambda (col)
    (let ((max-width (max 3 (org-trac-table-col-width table col info))))
      (when (< max-width 1)
        (setq max-width 1))
      (make-string max-width char))))

;;;; Table-Cell

(defun org-trac-table-cell (table-cell contents info)
  "Transcode TABLE-CELL element from Org into TRAC.
CONTENTS is content of the cell.  INFO is a plist used as a
communication channel."
  (let* ((table (org-export-get-parent-table table-cell))
         (column (cdr (org-export-table-cell-address table-cell info)))
         (width (org-trac-table-col-width table column info))
         (left-border (if (org-export-table-cell-starts-colgroup-p table-cell info) "|| " " "))
         (right-border " ||")
         (data (or contents "")))
    (setq contents
          (concat data
                  (make-string (max 0 (- width (string-width data)))
                               ?\s)))
    (concat left-border contents right-border)))

;;;; Table-Row

(defun org-trac-table-row (table-row contents info)
  "Transcode TABLE-ROW element from Org into TRAC.
CONTENTS is cell contents of TABLE-ROW.  INFO is a plist used as a
communication channel."
  (let ((table (org-export-get-parent-table table-row)))
    (when (and (eq 'rule (org-element-property :type table-row))
               ;; In TRAC, rule is valid only at second row.
               (eq 1 (cl-position
                      table-row
                      (org-element-map table 'table-row 'identity info))))
      (let* ((table (org-export-get-parent-table table-row))
             (build-rule (org-trac-make-hline-builder table info ?-))
             (cols (cdr (org-export-table-dimensions table info))))
        (setq contents
              (concat trac-table-left-border
                      (mapconcat (lambda (col) (funcall build-rule col))
                                 (number-sequence 0 (- cols 1))
                                 trac-table-separator)
                      trac-table-right-border))))
    contents))

;;;; Table

(defun org-trac-table (table contents info)
  "Transcode TABLE element into Trac Flavored Markdown table.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let* ((rows (org-element-map table 'table-row 'identity info))
         (no-header (or (<= (length rows) 1)))
         (cols (cdr (org-export-table-dimensions table info)))
         (build-dummy-header
          (lambda ()
            (let ((build-empty-cell (org-trac-make-hline-builder table info ?\s))
                  (build-rule (org-trac-make-hline-builder table info ?-))
                  (columns (number-sequence 0 (- cols 1))))
              (concat trac-table-left-border
                      (mapconcat (lambda (col) (funcall build-empty-cell col))
                                 columns
                                 trac-table-separator)
                      trac-table-right-border "\n" trac-table-left-border
                      (mapconcat (lambda (col) (funcall build-rule col))
                                 columns
                                 trac-table-separator)
                      trac-table-right-border "\n")))))
    (concat (and no-header (funcall build-dummy-header))
            (replace-regexp-in-string "\n\n" "\n" contents))))

;;;; Table of contents

(defun org-trac-format-toc (headline info)
  "Return an appropriate table of contents entry for HEADLINE."
  (let* ((title (org-export-data
                 (org-export-get-alt-title headline info) info))
         (level (1- (org-element-property :level headline)))
         (indent (concat (make-string (* level 2) ? )))
         (anchor (or (org-element-property :CUSTOM_ID headline)
                     (org-export-get-reference headline info))))
    (concat indent "- [" title "]" "(#" anchor ")")))

;;;; Footnote section

(defun org-trac-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (and-let* ((fn-alist (org-export-collect-footnote-definitions info)))
    (format
     "## Footnotes\n\n%s\n"
     (mapconcat (pcase-lambda (`(,n ,_type ,def))
                  (format
                   "%s %s\n"
                   (format (plist-get info :html-footnote-format)
                           (org-html--anchor
                            (format "fn.%d" n)
                            n
                            (format " class=\"footnum\" href=\"#fnr.%d\"" n)
                            info))
                   (org-trim (org-export-data def info))))
                fn-alist "\n"))))

;;;; Template

(defun org-trac-inner-template (contents info)
  "Return body of document after converting it to Markdown syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* (;(depth (plist-get info :with-toc))
         ;(headlines (and depth (org-export-collect-headlines info depth)))
         ;; (toc-string (or (mapconcat (lambda (headline)
         ;;                              (org-trac-format-toc headline info))
         ;;                            headlines "\n")
         ;;                 ""))
         ;; (toc-tail (if headlines "\n\n" ""))
         )
    ;(org-trim (concat toc-string toc-tail contents "\n" (org-trac-footnote-section info)))))
    (org-trim (concat contents "\n" (org-trac-footnote-section info)))))

;;; Interactive function

;;;###autoload
(defun org-trac-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Trac Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org TRAC Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'trac "*Org TRAC Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-trac-convert-region-to-md ()
  "Convert the region to Trac Flavored Markdown.
This can be used in any buffer, this function assume that the
current region has org-mode syntax.  For example, you can write
an itemized list in org-mode syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'trac))

;;;###autoload
(defun org-trac-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Trac Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'trac outfile async subtreep visible-only)))

;;;###autoload
(defun org-trac-publish-to-trac (plist filename pub-dir)
  "Publish an org file to Markdown.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'trac filename ".md" plist pub-dir))

(provide 'ox-trac)

;;; ox-trac.el ends here
