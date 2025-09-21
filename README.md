# convert-to-org

Single-function paste converter for Emacs: HTML, Markdown, or plain text → Org-mode.

## Features

- Detects content type: HTML, Markdown, or plain text
- Converts HTML via Pandoc
- Converts Markdown via mldoc, kramdown, pandoc, or regex fallback
- Plain text: direct paste
- One key binding: `C-c p o`

## Installation

1. Place `convert-to-org.el` in your Emacs `load-path`.
2. In your init:
    ```elisp
    (require 'convert-to-org)
    (convert-to-org-setup-keybinding)
    ```
3. Install converters:
    - mldoc: `npm install -g mldoc`
    - kramdown: `gem install kramdown kramdown-parser-gfm`
    - pandoc: via package manager

## Usage

- Copy HTML or Markdown or plain text.
- In an Org buffer, press `C-c p o`.
- Content is pasted converted to Org-mode or raw if plain text.

## Customization

```elisp
;; Preferred converter: mldoc, kramdown, pandoc, or auto
(setq convert-to-org-converter 'auto)
```

