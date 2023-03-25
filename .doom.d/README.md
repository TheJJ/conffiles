# jj's Doom Emacs configuration

Requires [GNU Emacs](https://www.gnu.org/software/emacs/) and [Doom Emacs](https://github.com/doomemacs/doomemacs).

This config adjusts and improves the default Doom configuration without sacrificing the its portability.

The config has grown since 2011 and went from vanilla Emacs, Spacemacs to Doom Emacs.

## dependencies

basically determined through doom.
- C++: `clangd`


## useful customization variables

device and user-specific config should be put in `device.el` in `~/.doom.d/`.
that way, these config files remain user-independent.

### org
- `org-agenda-files`: directories and files to auto-load.
  can be added with `org-agenda-file-to-front` C-c [
  and removed with `org-remove-file` C-c ]

### org-roam
- `org-roam-directory`: path where the org-roam notes are

### bibtex
- `bibtex-completion-bibliography`  (where are bib files)
- `bibtex-completion-library-path`  (where are pdfs)
- to edit notes of bibtex entries, use `M-x helm-bibtex <search> C-z F8`


## dir-locals

`dir-locals`: for project-specific configs, create a `.dir-locals.el` file:

example `.dir-locals.el` file for a linux repo:

```lisp
(
 (nil . ((indent-tabs-mode . t)
         (tab-width . 8)))
 (c-mode . ((c-file-style . "linux-kernel")))
 (c++-mode . ((c-file-style . "linux-kernel")))
 )
```


## License

```
jj's Emacs configuration
Copyright (c) 2011-2023 Jonas Jelten <jj@sft.lol>
Licensed GPLv3 or later
```
