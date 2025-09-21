;;; convert-to-org.el --- Paste and convert clipboard HTML/Markdown -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.2.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, markup, org

;;; Commentary:
;; One command to paste clipboard content into Org:
;; - Detects HTML vs Markdown vs plain
;; - Uses Pandoc via temp files
;; - Falls back to simple regex if Pandoc not found
;; Key binding: C-c p o

;;; Code:
(require 'subr-x)

(defgroup convert-to-org nil
  "Convert clipboard HTML or Markdown to Org-mode when pasting."
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
  (let ((s text))
    (setq s (replace-regexp-in-string "^# \\(.*\\)" "* \\1" s))
    (setq s (replace-regexp-in-string "^## \\(.*\\)" "** \\1" s))
    (setq s (replace-regexp-in-string "^### \\(.*\\)" "*** \\1" s))
    (setq s (replace-regexp-in-string "^[-*] " "- " s))
    (setq s (replace-regexp-in-string "^[0-9]+\\. " "1. " s))
    (replace-regexp-in-string
     "\\[\\([^]]+\\)\\](\\([^)]+\\))" "[[\\2][\\1]]" s)))

;;;###autoload
(defun convert-to-org-paste ()
  "Paste from clipboard, converting HTML/Markdown to Org or plain text."
  (interactive)
  (let* ((raw  (convert-to-org--get-clipboard-text))
         (type (cond
                ((string-match-p "^\\s-*<" raw) 'html)
                ((string-match-p "^\\s-*#\\|\\*\\s-+" raw) 'markdown)
                (t 'plain)))
         (out  (pcase type
                 ('html     (convert-to-org--pandoc "html" "org"     raw))
                 ('markdown (convert-to-org--pandoc "gfm"  "org"     raw))
                 (_          raw))))
    (insert out)))

;;;###autoload
(defun convert-to-org-setup-keybinding ()
  "Bind `convert-to-org-paste` to C-c p o."
  (global-set-key (kbd "C-c p o") #'convert-to-org-paste))

(provide 'convert-to-org)
;;; convert-to-org.el ends here
