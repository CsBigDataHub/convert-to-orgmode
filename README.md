# convert-to-org

Single-command Emacs utility to paste clipboard content into Org-mode.

Detects content type and converts:
- HTML → Org via Pandoc
- GitHub Markdown → Org via Pandoc
- Plain text → inserted unchanged
- Regex fallback if Pandoc unavailable

## Features

- **Automatic detection**: HTML vs Markdown vs Plain
- **Pandoc-based**: reliable conversion of complex content
- **Regex fallback**: basic headings, lists, links
- **One key binding**: `C-c p o`

## Installation

1. Place `convert-to-org.el` in your Emacs `load-path`.
2. Add to your init:
   ```elisp
   (require 'convert-to-org)
   (convert-to-org-setup-keybinding)
   ```

   Or, using `use-package` (lazy-load on Org buffers):

   ```elisp
   (use-package convert-to-org
     :load-path "/path/to/convert-to-org"
     :after org
     :hook (org-mode . convert-to-org-setup-keybinding)
     :init
     ;; Optional customizations:
     ;; (setq convert-to-org-pandoc-cmd "/usr/local/bin/pandoc")
     ;; (setq convert-to-org-fallback-regex t)
   )
   ```

3. Install Pandoc:
   - macOS: `brew install pandoc`
   - Debian/Ubuntu: `sudo apt install pandoc`
   - Windows: `choco install pandoc`

## Usage

1. Copy HTML, Markdown, or plain text.
2. In any Org buffer, press `C-c p o`.
3. Content is pasted converted to Org-mode or raw if plain.

## Customization

```elisp
;; Pandoc command if non-standard path
(setq convert-to-org-pandoc-cmd "/usr/local/bin/pandoc")

;; Enable/disable regex fallback
(setq convert-to-org-fallback-regex t)
```

## FAQ

**Q: I see errors about Pandoc not found.**
A: Ensure Pandoc is installed and on your PATH.

**Q: I want to support other Markdown processors.**
A: This version uses only Pandoc for reliability. Regex fallback covers basics.
