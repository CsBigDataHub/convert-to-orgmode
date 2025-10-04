;;; convert-to-org.el --- Paste and convert clipboard HTML/Markdown/Jupyter -*- lexical-binding: t; -*-

;; Author: CK
;; Version: 1.4.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, markup, org, jupyter

;;; Commentary:
;; One command to paste clipboard content into Org:
;; - Detects HTML vs Markdown vs Jupyter vs plain
;; - Cross-platform clipboard access (macOS, Linux, Windows)
;; - Handles Jupyter notebook content properly
;; - Preserves code blocks and prevents code comments from being treated as headers
;; - Removes Jupyter %md magic commands
;; - Uses Pandoc via temp files for robust HTML/Markdown conversion
;; - Falls back to simple regex if Pandoc not found
;; - Integrates jupytext for direct Org conversion
;; - Enhanced HTML parsing with multiple fallback methods
;; Key bindings:
;;   C-c p o   convert intelligently based on content type
;;   C-c p j   force Jupyter conversion via jupytext
;;   C-c p h   force HTML conversion

;;; Code:
(require 'subr-x)
(require 'eww)

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

(defcustom convert-to-org-html-fallback-method 'simple
  "Method to use for HTML conversion when pandoc is unavailable.
Options: simple, dom, eww."
  :type '(choice (const :tag "Simple Regex" simple)
                 (const :tag "DOM Parser" dom)
                 (const :tag "EWW Readable" eww))
  :group 'convert-to-org)

(defun convert-to-org--get-clipboard-text ()
  "Retrieve text from system clipboard in a cross-platform manner."
  (or
   ;; Try GUI selection first
   (and (fboundp 'gui-get-selection)
        (gui-get-selection 'CLIPBOARD))

   ;; macOS: use pbpaste
   (and (eq system-type 'darwin)
        (executable-find "pbpaste")
        (with-temp-buffer
          (when (zerop (call-process "pbpaste" nil t nil))
            (buffer-string))))

   ;; Linux/Unix: try xclip or xsel
   (and (memq system-type '(gnu/linux berkeley-unix))
        (or (and (executable-find "xclip")
                 (with-temp-buffer
                   (when (zerop (call-process "xclip" nil t nil "-selection" "clipboard" "-o"))
                     (buffer-string))))
            (and (executable-find "xsel")
                 (with-temp-buffer
                   (when (zerop (call-process "xsel" nil t nil "--clipboard" "--output"))
                     (buffer-string))))))

   ;; Windows: try powershell
   (and (eq system-type 'windows-nt)
        (executable-find "powershell.exe")
        (with-temp-buffer
          (when (zerop (call-process "powershell.exe" nil t nil "-command" "Get-Clipboard"))
            (buffer-string))))

   ;; Fallback to kill-ring
   (condition-case nil
       (current-kill 0)
     (error ""))))

(defun convert-to-org--has-markdown-headers-outside-code (text)
  "Check if TEXT has markdown headers not inside code blocks."
  (let ((lines (split-string text "\n"))
        (in-code-block nil)
        (has-header nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^[ \t]*```" line)
        (setq in-code-block (not in-code-block)))
       ((and (not in-code-block)
             (string-match-p "^[ \t]*#[ \t]+[a-zA-Z]" line))
        (setq has-header t))))
    has-header))

(defun convert-to-org--detect-content-type (text)
  "Detect whether TEXT is HTML, Jupyter, Markdown, or plain text."
  (cond
   ;; HTML detection - look for HTML tags
   ((or (string-match-p "^[ \t]*<!DOCTYPE html" text)
        (string-match-p "^[ \t]*<html" text)
        (string-match-p "<[a-zA-Z][^>]*>" text)) 'html)

   ;; Jupyter detection: code fences, %md, In/Out
   ((or (string-match-p "^[ \t]*```[a-zA-Z]" text)
        (string-match-p "^[ \t]*%md" text)
        (string-match-p "^[ \t]*In[ \t]*\\[" text)
        (string-match-p "^[ \t]*Out[ \t]*\\[" text)) 'jupyter)

   ;; Markdown detection
   ((convert-to-org--has-markdown-headers-outside-code text) 'markdown)

   ;; Default to plain text
   (t 'plain)))

(defun convert-to-org--preprocess-jupyter (text)
  "Preprocess Jupyter notebook content to prepare for conversion."
  (let ((processed text))
    (setq processed (replace-regexp-in-string "^[ \t]*%md[ \t]*\\(\\n\\)?" "" processed))
    (setq processed (replace-regexp-in-string "^[ \t]*In[ \t]*\\[[0-9]*\\]:[ \t]*\\(\\n\\)?" "" processed))
    (setq processed (replace-regexp-in-string "^[ \t]*Out[ \t]*\\[[0-9]*\\]:[ \t]*\\(\\n\\)?" "" processed))
    (setq processed (replace-regexp-in-string "\\(\\n\\)\\{3,\\}" "\n\n" processed))
    processed))

(defun convert-to-org--html-to-org-pandoc (html-text)
  "Convert HTML-TEXT to Org format using pandoc."
  (let ((tmp (make-temp-file "cto-html" nil ".html")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert html-text))
          (with-temp-buffer
            (if (zerop (call-process convert-to-org-pandoc-cmd nil t nil
                                     "--from=html" "--to=org" tmp))
                (buffer-string)
              (error "Pandoc HTML conversion failed"))))
      (delete-file tmp))))

(defun convert-to-org--simple-html-to-org (html-text)
  "Simple regex-based HTML to Org conversion."
  (let ((s html-text))
    ;; Headers
    (setq s (replace-regexp-in-string "<h1[^>]*>\\(.*?\\)</h1>" "* \\1" s))
    (setq s (replace-regexp-in-string "<h2[^>]*>\\(.*?\\)</h2>" "** \\1" s))
    (setq s (replace-regexp-in-string "<h3[^>]*>\\(.*?\\)</h3>" "*** \\1" s))
    (setq s (replace-regexp-in-string "<h4[^>]*>\\(.*?\\)</h4>" "**** \\1" s))
    (setq s (replace-regexp-in-string "<h5[^>]*>\\(.*?\\)</h5>" "***** \\1" s))
    (setq s (replace-regexp-in-string "<h6[^>]*>\\(.*?\\)</h6>" "****** \\1" s))
    ;; Paragraphs
    (setq s (replace-regexp-in-string "<p[^>]*>" "" s))
    (setq s (replace-regexp-in-string "</p>" "\n\n" s))
    ;; Bold and italic
    (setq s (replace-regexp-in-string "<\\(strong\\|b\\)[^>]*>\\(.*?\\)</\\(strong\\|b\\)>" "*\\2*" s))
    (setq s (replace-regexp-in-string "<\\(em\\|i\\)[^>]*>\\(.*?\\)</\\(em\\|i\\)>" "/\\2/" s))
    ;; Code
    (setq s (replace-regexp-in-string "<code[^>]*>\\(.*?\\)</code>" "~\\1~" s))
    (setq s (replace-regexp-in-string "<pre[^>]*>\\(.*?\\)</pre>" "#+BEGIN_EXAMPLE\n\\1\n#+END_EXAMPLE" s))
    ;; Links
    (setq s (replace-regexp-in-string "<a[^>]*href=\"\\([^\"]+\\)\"[^>]*>\\(.*?\\)</a>" "[[\\1][\\2]]" s))
    ;; Line breaks
    (setq s (replace-regexp-in-string "<br[^>]*>" "\n" s))
    ;; Lists
    (setq s (replace-regexp-in-string "<li[^>]*>\\(.*?\\)</li>" "- \\1\n" s))
    ;; Remove remaining tags
    (setq s (replace-regexp-in-string "<[^>]+>" "" s))
    ;; Clean up whitespace
    (setq s (replace-regexp-in-string "[ \t]+" " " s))
    (setq s (replace-regexp-in-string "\n\n\n+" "\n\n" s))
    (string-trim s)))

(defun convert-to-org--html-to-org-dom (html-text)
  "Convert HTML-TEXT to Org using DOM parsing."
  (if (not (fboundp 'libxml-parse-html-region))
      (convert-to-org--simple-html-to-org html-text)
    (with-temp-buffer
      (insert html-text)
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (convert-to-org--dom-to-org dom)))))

(defun convert-to-org--dom-to-org (dom)
  "Convert DOM tree to Org format."
  (cond
   ((stringp dom) dom)
   ((not (listp dom)) "")
   (t
    (let ((tag (car dom))
          (attrs (when (listp (cadr dom)) (cadr dom)))
          (children (if (listp (cadr dom)) (cddr dom) (cdr dom))))
      (pcase tag
        ('h1 (concat "* " (mapconcat #'convert-to-org--dom-to-org children "")))
        ('h2 (concat "** " (mapconcat #'convert-to-org--dom-to-org children "")))
        ('h3 (concat "*** " (mapconcat #'convert-to-org--dom-to-org children "")))
        ('h4 (concat "**** " (mapconcat #'convert-to-org--dom-to-org children "")))
        ('h5 (concat "***** " (mapconcat #'convert-to-org--dom-to-org children "")))
        ('h6 (concat "****** " (mapconcat #'convert-to-org--dom-to-org children "")))
        ('p (concat (mapconcat #'convert-to-org--dom-to-org children "") "\n\n"))
        ('br "\n")
        ('strong (concat "*" (mapconcat #'convert-to-org--dom-to-org children "") "*"))
        ('b (concat "*" (mapconcat #'convert-to-org--dom-to-org children "") "*"))
        ('em (concat "/" (mapconcat #'convert-to-org--dom-to-org children "") "/"))
        ('i (concat "/" (mapconcat #'convert-to-org--dom-to-org children "") "/"))
        ('code (concat "~" (mapconcat #'convert-to-org--dom-to-org children "") "~"))
        ('pre (concat "#+BEGIN_EXAMPLE\n"
                      (mapconcat #'convert-to-org--dom-to-org children "")
                      "\n#+END_EXAMPLE\n"))
        ('a (let ((href (alist-get 'href attrs)))
              (if href
                  (format "[[%s][%s]]" href (mapconcat #'convert-to-org--dom-to-org children ""))
                (mapconcat #'convert-to-org--dom-to-org children ""))))
        ('li (concat "- " (mapconcat #'convert-to-org--dom-to-org children "") "\n"))
        (_ (mapconcat #'convert-to-org--dom-to-org children "")))))))

(defun convert-to-org--html-to-org-eww (html-text)
  "Convert HTML-TEXT to Org using eww-readable."
  (if (not (fboundp 'eww-readable))
      (convert-to-org--simple-html-to-org html-text)
    (with-temp-buffer
      (insert html-text)
      (let ((dom (when (fboundp 'libxml-parse-html-region)
                   (libxml-parse-html-region (point-min) (point-max)))))
        (when dom
          (erase-buffer)
          (eww-readable dom)
          (convert-to-org--simple-html-to-org (buffer-string)))))))

(defun convert-to-org--html-to-org (html-text)
  "Convert HTML-TEXT to Org format using best available method."
  (condition-case err
      (if (executable-find convert-to-org-pandoc-cmd)
          (convert-to-org--html-to-org-pandoc html-text)
        (pcase convert-to-org-html-fallback-method
          ('eww (convert-to-org--html-to-org-eww html-text))
          ('dom (convert-to-org--html-to-org-dom html-text))
          ('simple (convert-to-org--simple-html-to-org html-text))
          (_ (convert-to-org--simple-html-to-org html-text))))
    (error
     (message "HTML conversion failed: %s. Using simple fallback." (error-message-string err))
     (convert-to-org--simple-html-to-org html-text))))

(defun convert-to-org--jupytext (text)
  "Convert Jupyter-flavored TEXT to Org using jupytext."
  (if (not (executable-find "jupytext"))
      (error "jupytext not found")
    (with-temp-buffer
      (insert text)
      (let ((exit-code
             (call-process-region (point-min) (point-max)
                                  "jupytext" t t nil
                                  "--to" "org" "--pipe")))
        (if (zerop exit-code)
            (buffer-string)
          (error "jupytext conversion failed: exit %d" exit-code))))))

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
  "Basic regex fallback converting Markdown-like TEXT to Org."
  (let ((lines (split-string text "\n"))
        (result '())
        (in-code-block nil)
        (lang ""))
    (dolist (line lines)
      (cond
       ;; Start of code block
       ((string-match "^[ \t]*```\\([a-zA-Z]*\\)" line)
        (setq in-code-block t)
        (setq lang (match-string 1 line))
        (push (concat "#+BEGIN_SRC " lang) result))
       ;; End of code block
       ((and in-code-block (string-match-p "^[ \t]*```" line))
        (setq in-code-block nil)
        (setq lang "")
        (push "#+END_SRC" result))
       ;; Inside code block: preserve
       (in-code-block
        (push line result))
       ;; Outside code block
       (t
        (let ((s line))
          (setq s (replace-regexp-in-string "^# \\(.*\\)" "* \\1" s))
          (setq s (replace-regexp-in-string "^## \\(.*\\)" "** \\1" s))
          (setq s (replace-regexp-in-string "^### \\(.*\\)" "*** \\1" s))
          (setq s (replace-regexp-in-string "^[-*] " "- " s))
          (setq s (replace-regexp-in-string "^[0-9]+\\. " "1. " s))
          (setq s (replace-regexp-in-string
                   "\\[\\([^]]+\\)\\](\\([^)]+\\))" "[[\\2][\\1]]" s))
          (push s result)))))
    (string-join (nreverse result) "\n")))

;;;###autoload
(defun convert-to-org-paste ()
  "Paste from clipboard, converting HTML/Markdown/Jupyter to Org or plain text."
  (interactive)
  (let* ((raw (convert-to-org--get-clipboard-text))
         (type (convert-to-org--detect-content-type raw))
         (clean (if (eq type 'jupyter)
                    (convert-to-org--preprocess-jupyter raw)
                  raw))
         (out (condition-case err
                  (pcase type
                    ('html     (convert-to-org--html-to-org clean))
                    ('jupyter  (convert-to-org--jupytext clean))
                    ('markdown (convert-to-org--pandoc "gfm" "org" clean))
                    (_         clean))
                (error
                 (message "Conversion failed: %s" (error-message-string err))
                 clean))))
    (insert out)))

;;;###autoload
(defun convert-to-org-paste-as-jupyter ()
  "Force paste clipboard content as Jupyter notebook format via jupytext."
  (interactive)
  (let* ((raw (convert-to-org--get-clipboard-text))
         (clean (convert-to-org--preprocess-jupyter raw))
         (out (condition-case err
                  (if (executable-find "jupytext")
                      (convert-to-org--jupytext clean)
                    (convert-to-org--regex-fallback clean))
                (error
                 (message "Jupyter conversion failed: %s" (error-message-string err))
                 (convert-to-org--regex-fallback clean)))))
    (insert out)))

;;;###autoload
(defun convert-to-org-paste-as-html ()
  "Force paste clipboard content as HTML and convert to Org."
  (interactive)
  (let* ((raw (convert-to-org--get-clipboard-text))
         (out (condition-case err
                  (convert-to-org--html-to-org raw)
                (error
                 (message "HTML conversion failed: %s" (error-message-string err))
                 raw))))
    (insert out)))

;;;###autoload
(defun convert-to-org-setup-keybinding ()
  "Bind conversion functions to convenient keys."
  (global-set-key (kbd "C-c p o") #'convert-to-org-paste)
  (global-set-key (kbd "C-c p j") #'convert-to-org-paste-as-jupyter)
  (global-set-key (kbd "C-c p h") #'convert-to-org-paste-as-html))

(provide 'convert-to-org)
;;; convert-to-org.el ends here
