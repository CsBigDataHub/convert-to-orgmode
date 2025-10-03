;;; convert-to-org.el --- Paste and convert clipboard HTML/Markdown/Jupyter -*- lexical-binding: t; -*-

;; Author: CK
;; Version: 1.3.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, markup, org, jupyter

;;; Commentary:
;; One command to paste clipboard content into Org:
;; - Detects HTML vs Markdown vs Jupyter vs plain
;; - Handles Jupyter notebook content properly
;; - Preserves code blocks and prevents code comments from being treated as headers
;; - Removes Jupyter %md magic commands
;; - Uses Pandoc via temp files
;; - Falls back to simple regex if Pandoc not found
;; - Integrates jupytext for direct Org conversion
;; Key bindings:
;;   C-c p o   convert intelligently based on content type
;;   C-c p j   force Jupyter conversion via jupytext

;;; Code:
(require 'subr-x)

(defgroup convert-to-org nil
  "Convert clipboard HTML, Markdown, or Jupyter to Org-mode when pasting."
  :group 'tools
  :prefix "convert-to-org-")

(defcustom convert-to-org-pandoc-cmd "pandoc"
  "Command name for the pandoc converter."
  :type 'string
  :group 'convert-to-org)

(defcustom convert-to-org-fallback-regex t
  "Use basic regex conversion if Pandoc is unavailable or errors."
  :type 'boolean
  :group 'convert-to-org)

(defun convert-to-org--get-clipboard-text ()
  "Retrieve text from system clipboard or kill-ring."
  (or (and (fboundp 'gui-get-selection)
           (gui-get-selection 'CLIPBOARD))
      (condition-case nil
          (current-kill 0)
        (error ""))))

(defun convert-to-org--has-markdown-headers-outside-code (text)
  "Check if TEXT has markdown headers that are not inside code blocks."
  (let ((lines (split-string text "\n"))
        (in-code-block nil)
        (has-header nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^\\s-*```
        (setq in-code-block (not in-code-block)))
       ((and (not in-code-block)
             (string-match-p "^\\s-*#\\s-+\\w" line))
        (setq has-header t))))
    has-header))

(defun convert-to-org--detect-content-type (text)
  "Detect whether TEXT is HTML, Jupyter, Markdown, or plain text."
  (cond
   ;; HTML detection
   ((string-match-p "^\\s-*<" text) 'html)
   ;; Jupyter detection: code fences, %md magic, or In/Out prompts
   ((or (string-match-p "^\\s-*```[a-zA-Z]" text)
        (string-match-p "^\\s-*%md" text)
        (string-match-p "^\\s-*In\\s-*\\[" text)
        (string-match-p "^\\s-*Out\\s-*\\[" text))
    'jupyter)
   ;; Markdown detection
   ((convert-to-org--has-markdown-headers-outside-code text) 'markdown)
   ;; Default to plain text
   (t 'plain)))

(defun convert-to-org--preprocess-jupyter (text)
  "Preprocess Jupyter notebook content to prepare for conversion."
  (let ((processed text))
    ;; Remove %md magic commands
    (setq processed (replace-regexp-in-string "^\\s-*%md\\s-*\\n?" "" processed))
    ;; Remove In/Out markers
    (setq processed (replace-regexp-in-string "^\\s-*In\\s-*\\[[0-9]*\\]:\\s-*\\n?" "" processed))
    (setq processed (replace-regexp-in-string "^\\s-*Out\\s-*\\[[0-9]*\\]:\\s-*\\n?" "" processed))
    ;; Collapse multiple blank lines
    (setq processed (replace-regexp-in-string "\n\n\n+" "\n\n" processed))
    processed))

(defun convert-to-org--jupytext (text)
  "Convert Jupyter-flavored TEXT to Org using jupytext."
  (with-temp-buffer
    (insert text)
    (let ((exit-code
           (call-process-region (point-min) (point-max)
                                "jupytext" t t nil
                                "--to" "org" "--pipe")))
      (if (zerop exit-code)
          (buffer-string)
        (error "jupytext conversion failed: exit %d" exit-code)))))

(defun convert-to-org--pandoc (from to text)
  "Run Pandoc FROM format TO format on TEXT via a temp file."
  (let ((cmd convert-to-org-pandoc-cmd)
        (args (list (format "--from=%s" from)
                    (format "--to=%s" to)
                    "--wrap=preserve"))
        (tmp (make-temp-file "cto" nil (concat "." from))))
    (unwind-protect
        (condition-case _err
            (progn
              (with-temp-file tmp (insert text))
              (with-temp-buffer
                (apply #'call-process cmd tmp t nil args)
                (buffer-string)))
          (error
           (if convert-to-org-fallback-regex
               (convert-to-org--regex-fallback text)
             (error "Pandoc conversion failed"))))
      (delete-file tmp))))

(defun convert-to-org--regex-fallback (text)
  "Basic regex fallback converting Markdown-like TEXT to Org, preserving code blocks."
  (let ((lines (split-string text "\n"))
        (result '())
        (in-code-block nil)
        (lang nil))
    (dolist (line lines)
      (cond
       ;; Start of code block
       ((string-match "^\\s-*```
        (setq in-code-block t)
        (setq lang (match-string 1 line))
        (push (format "#+BEGIN_SRC %s" (or lang "")) result))
       ;; End of code block
       ((and in-code-block (string-match-p "^\\s-*```" line))
        (setq in-code-block nil)
        (setq lang nil)
        (push "#+END_SRC" result))
       ;; Inside code block: preserve
       (in-code-block
        (push line result))
       ;; Outside code block: apply conversions
       (t
        (let ((s line))
          ;; Headers
          (setq s (replace-regexp-in-string "^# \\(.*\\)" "* \\1" s))
          (setq s (replace-regexp-in-string "^## \\(.*\\)" "** \\1" s))
          (setq s (replace-regexp-in-string "^### \\(.*\\)" "*** \\1" s))
          ;; Lists
          (setq s (replace-regexp-in-string "^[-*] " "- " s))
          (setq s (replace-regexp-in-string "^[0-9]+\\. " "1. " s))
          ;; Links
          (setq s (replace-regexp-in-string
                   "\\[\\([^]]+\\)\\](\\([^)]+\\))" "[[\\2][\\1]]" s))
          (push s result)))))
    (string-join (reverse result) "\n")))

;;;###autoload
(defun convert-to-org-paste ()
  "Paste from clipboard, converting HTML/Markdown/Jupyter to Org or plain text."
  (interactive)
  (let* ((raw (convert-to-org--get-clipboard-text))
         (type (convert-to-org--detect-content-type raw))
         (clean (if (eq type 'jupyter)
                    (convert-to-org--preprocess-jupyter raw)
                  raw))
         (out (pcase type
                ('html     (convert-to-org--pandoc "html" "org" clean))
                ('jupyter  (convert-to-org--jupytext clean))
                ('markdown (convert-to-org--pandoc "gfm" "org" clean))
                (_         clean))))
    (insert out)))

;;;###autoload
(defun convert-to-org-paste-as-jupyter ()
  "Force paste clipboard content as Jupyter notebook format via jupytext."
  (interactive)
  (let* ((raw (convert-to-org--get-clipboard-text))
         (clean (convert-to-org--preprocess-jupyter raw))
         (out (if (and (executable-find "jupytext")
                       (not (string-empty-p clean)))
                  (convert-to-org--jupytext clean)
                (convert-to-org--regex-fallback clean))))
    (insert out)))

;;;###autoload
(defun convert-to-org-setup-keybinding ()
  "Bind conversion functions to convenient keys."
  (global-set-key (kbd "C-c p o") #'convert-to-org-paste)
  (global-set-key (kbd "C-c p j") #'convert-to-org-paste-as-jupyter))

(provide 'convert-to-org)
;;; convert-to-org.el ends here
