[![CI](https://github.com/pasunboneleve/markdown-mermaid/actions/workflows/test.yml/badge.svg)](https://github.com/pasunboneleve/markdown-mermaid/actions/workflows/test.yml)
# markdown-mermaid.el

A lightweight Emacs package that allows you to preview [Mermaid](https://mermaid-js.github.io/mermaid/#/) diagrams directly from Markdown buffers.

It leverages the official Mermaid CLI (`mmdc`) to compile the code blocks into images and displays them in a separate Emacs buffer. Crucially, **it automatically themes the diagram** (backgrounds, lines, and text) to match your current Emacs theme.

## Prerequisites

This package requires the Mermaid Command Line Interface. You must have Node.js installed.

```bash
npm install -g @mermaid-js/mermaid-cli
```

Verify the installation by running `mmdc --version` in your terminal.

## Installation

### Manual Installation

1.  Clone this repository:
    ```bash
    git clone [https://github.com/pasunboneleve/markdown-mermaid.git](https://github.com/pasunboneleve/markdown-mermaid.git) ~/.emacs.d/site-lisp/markdown-mermaid
    ```
2.  Add the following to your `init.el`:
    ```elisp
    (add-to-list 'load-path "~/.emacs.d/site-lisp/markdown-mermaid")
    (require 'markdown-mermaid)
    ```

### Using `straight.el` (Recommended)

```elisp
(use-package markdown-mermaid
  :straight (markdown-mermaid :type git :host github :repo "pasunboneleve/markdown-mermaid")
  :bind (:map markdown-mode-map
              ("C-c C-x m" . markdown-mermaid-preview)))
```

### Using Emacs 29+ (`package-vc`)

If you are on Emacs 29 or newer and prefer the built-in package manager:

```elisp
(use-package markdown-mermaid
  :vc (:url "https://github.com/pasunboneleve/markdown-mermaid" :rev :newest)
  :bind (:map markdown-mode-map
              ("C-c C-x m" . markdown-mermaid-preview)))
```

## Usage

1.  Open a Markdown file (`.md`).

2.  Create a Mermaid code block. For example:

    ```mermaid
    graph TD;
        A[Start] --> B{Is it working?};
        B -- Yes --> C[Great!];
        B -- No --> D[Check Logs];
    ```

3.  Place your cursor anywhere **inside** the code block (between the triple backticks).

4.  Run the command:

      * **M-x** `markdown-mermaid-preview`
      * Or use the keybinding if you configured one (e.g., `C-c C-x m`).

A new buffer will open displaying the rendered diagram.

## Configuration

You can customize the package behavior using `M-x customize-group RET markdown-mermaid`.

### Variables

| Variable | Default | Description |
| :--- | :--- | :--- |
| `markdown-mermaid-mmdc-path` | `(executable-find "mmdc")` | The absolute path to the mermaid-cli executable. If Emacs cannot find `mmdc`, set this manually. |
| `markdown-mermaid-temp-files` | `nil` | (Internal) Tracks temporary files created during the session for cleanup. |

### Theming

There are no manual variables for theming. The package automatically pulls the following faces from your current active theme to color the diagram:

  * **Background**: `default` background color.
  * **Text**: `default` foreground color.
  * **Borders**: `font-lock-type-face` foreground.
  * **Lines/Arrows**: `font-lock-constant-face` foreground.

## Troubleshooting

**"Mermaid CLI not found"**

  * Ensure you ran `npm install -g @mermaid-js/mermaid-cli`.
  * If you installed it via `nvm`, Emacs might not inherit the PATH correctly. Set the variable explicitly in your config:
    ```elisp
    (setq markdown-mermaid-mmdc-path "/home/user/.nvm/versions/node/v14/bin/mmdc")
    ```

**"Compilation failed"**

  * Check the `*mermaid-error*` buffer. It usually contains syntax errors reported by the Mermaid CLI.

## Development

To run the unit tests locally:

```bash
make test
```
