# RTN
RTN (Rich Text Notes) is a robust toy project that allows you to add persistent, searchable, and visual annotation markers to your code, documents, and notes. You can also use it as comments, bookmarks, or notes. Inspired by [rich-text](https://github.com/Kinneyzhang/rich-text).

## Features
- **Emoji Icon Markers**: Quick annotation using 📝 (default), 🐛 (Bug), ✅ (Todo), and other icons that dynamically follow text positions
- **Visual Table Management**: Display all annotations in tabulated-list format with support for search, filter, delete, and modify annotations
- **Reference Feature**: Support for creating references to annotation positions. References can be placed anywhere pointing to original annotation positions. Support for cloning annotations and re-marking them at other locations

## Configuration and Dependencies
```elisp
(use-package emacsql
  :straight (:host github :repo "magit/emacsql")
  :defer nil)
(use-package emacsql-sqlite-builtin
  :straight (:host github :repo "magit/emacsql"
			 :files ("sqlite-builtin/*.el"))
  :after emacsql)
(use-package selected
  :straight (:host github :repo "Kungsgeten/selected.el"))
(use-package ov
  :straight (:host github :repo "emacsorphanage/ov"))
```

```elisp
(add-to-list 'load-path "xxx/rtn")
(require 'rtn-db)
(require 'rtn)
;; (setq rtn-show-note-on-hover nil)
(require 'rtn-extra)
(require 'rtn-list-tabulated)
(setq rtn-tabulated-max-content-lines 5)
;; (setq rtn-tabulated-max-content-lines nil)
(require 'rtn-reference)
(require 'rtn-migrate)
(add-hook 'prog-mode-hook #'rtn-mode)
(add-hook 'text-mode-hook #'rtn-mode)
(rtn-mode 1)
```

## Basic Operations

### Add/Delete Annotations
- `rtn-add-edit`
- `rtn-add-edit-with-icon`
- `rtn-clear-at-point`
- `rtn-clear-buffer`
- `rtn-clear-file`

### Enter List View
- `rnt-list-tabulate`
- `rnt-search-tabulate`

### Reference and Clone
- `rtn-copy-position`
- `rtn-paste-reference`
- `rtn-ref-jump-to-target`
---
- `rtn-copy-anno-text`
- `rtn-paste-anno-as-marker`
---
- `rtn-move-annotation`
- `rtn-paste-moved-annotation`
---
- `rtn-copy-reference-with-target`
- `rtn-paste-reference-with-target`

### Migrate
- `rtn-migrate-file`
- `rtn-migrate-directory`

### Rescue
- `rtn-rescue-annotations`

| Command                   | Key | Function              |
|---------------------------|-----|-----------------------|
| rtn-rescue-save-to-file   | RET | Rescue save to file   |
| rtn-rescue-delete-entry   | d   | Delete annotation     |
| rtn-rescue-edit-file-path | e   | Rescue edit file path |
| rtn-rescue-refresh        | g   | Refresh list          |
| quit-window               | q   | Close list            |


## List Table Keybindings

| Command                    | Key | Function                 |
|----------------------------|-----|--------------------------|
| rtn-tabulated-jump         | RET | Jump to annotation       |
| rtn-list-edit-annotation   | e   | Edit annotation          |
| rtn-tabulated-delete       | d   | Delete annotation        |
| rtn-tabulated-other-window | o   | Preview annotation       |
| rtn-tabulated-filter       | /   | Search and filter        |
| rtn-list-tabulated         | L   | Switch back to full list |
| quit-window                | q   | Close list               |
