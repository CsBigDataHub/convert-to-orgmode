;;; convert-to-org.el --- Paste and convert clipboard HTML/Markdown to Org -*- lexical-binding: t; -*-

;; Author: CK
;; Version: 1.1.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, markup, org

;;; Commentary:
;; Single function to paste clipboard content into an Org buffer.
;; Detects content type: HTML, Markdown, or plain text.
;; Converts HTML or Markdown to Org-mode using CLI tools.
;; Plain text is inserted unmodified.

;;; Code:
(require 'subr-x)

(defgroup convert-to-org nil
  "Convert clipboard HTML or Markdown to Org-mode when pasting."
  :group 'tools
  :prefix "convert-to-org-")

(defcustom convert-to-org-converter 'auto
  "Preferred converter: mldoc, kramdown, pandoc, or auto-detect."
  :type '(choice (const :tag "mldoc" mldoc)
                 (const :tag "kramdown" kramdown)
                 (const :tag "pandoc" pandoc)
                 (const :tag "auto" auto))
  :group 'convert-to-org)

(defcustom convert-to-org-mldoc-cmd "mldoc"
  "Command name for mldoc converter."
  :type 'string
  :group 'convert-to-org)

(defcustom convert-to-org-kramdown-cmd "kramdown"
  "Command name for kramdown converter."
  :type 'string
  :group 'convert-to-org)

(defcustom convert-to-org-pandoc-cmd "pandoc"
  "Command name for pandoc converter."
  :type 'string
  :group 'convert-to-org)

(defcustom convert-to-org-fallback-regex t
  "Use basic regex conversion if no CLI converter is available."
  :type 'boolean
  :group 'convert-to-org)

;;; Utility functions

(defun convert-to-org--get-clipboard-text ()
  "Retrieve text from system clipboard or kill-ring."
  (or (and (fboundp 'gui-get-selection)
           (gui-get-selection 'CLIPBOARD))
      (condition-case nil
          (current-kill 0)
        (error ""))))

(defun convert-to-org--classify (text)
  "Classify TEXT as html, markdown, or plaintext.
Heuristic: TEXT starting with '<' is html,
containing markdown markers is markdown, else plaintext."
  (cond
   ((string-match-p "^\s-*<" text) 'html)
   ((string-match-p "^\s-*#\|\*\s-+" text) 'markdown)
   (t 'plaintext)))

(defun convert-to-org--shell (cmd args input)
  "Run CMD with ARGS on INPUT string; return output string."
  (with-temp-buffer
    (insert input)
    (let ((exit (apply #'call-process-region
                       (point-min) (point-max)
                       cmd t t nil args)))
      (if (= exit 0)
          (buffer-string)
        (error "Conversion failed: %s exited %d" cmd exit)))))

(defun convert-to-org--convert-markdown (text)
  "Convert Markdown TEXT to Org-mode."
  (let ((conv (or (and (executable-find convert-to-org-mldoc-cmd) 'mldoc)
                  (and (executable-find convert-to-org-kramdown-cmd) 'kramdown)
                  (and (executable-find convert-to-org-pandoc-cmd) 'pandoc)
                  (when convert-to-org-fallback-regex 'regex))))
    (pcase conv
      ('mldoc (convert-to-org--shell convert-to-org-mldoc-cmd
                                     '("convert" "--format" "org") text))
      ('kramdown (convert-to-org--shell convert-to-org-kramdown-cmd
                                        '("-i" "GFM" "-o" "org") text))
      ('pandoc (convert-to-org--shell convert-to-org-pandoc-cmd
                                      '("--from=gfm" "--to=org" "--wrap=preserve")
                                      text))
      ('regex
       ;; Basic fallback: convert links
       (replace-regexp-in-string
        "\[\([^]]+\)\](\([^)]+\))" "[[\2][\1]]" text))
      (_ text))))

(defun convert-to-org--convert-html (text)
  "Convert HTML TEXT to Org-mode using pandoc."
  (if (executable-find convert-to-org-pandoc-cmd)
      (convert-to-org--shell convert-to-org-pandoc-cmd
                             '("--from=html" "--to=org" "--wrap=preserve")
                             text)
    (error "Pandoc not found for HTML conversion")))

;;;###autoload
(defun convert-to-org-paste ()
  "Paste from clipboard, converting HTML/Markdown to Org-mode."
  (interactive)
  (let* ((raw (convert-to-org--get-clipboard-text))
         (type (convert-to-org--classify raw))
         (out (pcase type
                ('html (convert-to-org--convert-html raw))
                ('markdown (convert-to-org--convert-markdown raw))
                ('plaintext raw))))
    (insert out)))

;;;###autoload
(defun convert-to-org-setup-keybinding ()
  "Bind `convert-to-org-paste` to a single key."
  (global-set-key (kbd "C-c p o") #'convert-to-org-paste))

(provide 'convert-to-org)
;;; convert-to-org.el ends here
