## [2021-06-08 Tue 14:32]
- Added ChangeLog and License.
- `util/hidden-buffers` now has regexp optional.
- Bug fix for `util/try-copy-help-buffer-link`.
- `util/rgrep-default-search` now ignores `node_modules` directory for JS files
  and `env` dir for python files.
- `util/org-collect-headings` now has only one predicate. It used to have two.
- Fixed a bug in `util/org-multi-collect-headings` where buffer timestamps weren't
  being checked correctly.
- Fixed consequent bugs in `util/org-insert-link-to-heading`.
- Added a `citation` option to `util/org-insert-link-to-heading` and consequent
  text collection code.
- Added a helper routine `util/org-get-text-links` to get links from org text body.
- Added a function `util/run-python-file-from-str` but it isn't useful yet.
- Version bump to 0.2.0.

## [2021-06-08 Tue 14:32]
- Separated some functionality of `util/org-insert-link-to-heading` into
  subroutines.
- Added suggestions from `references` in `util/org-insert-link-to-heading`
- Added macros `util/with-check-mode`, `util/with-org-mode` etc.
- Added time stamp functions `util/insert-or-update-time-stamp-at-point`,
  `util/insert-or-update-today`
- Added org utility functions `util/org-apply-to-buffer-headings`,
  `util/org-apply-to-subtree-headings`, `util/org-heading-and-body-bounds`,
  `util/org-get-subtree-with-body-for-heading-matching-re`
- Modified `util/org-multi-collect-headings` to optionally return only headings
  for `bufname`. Slight modifications to few other org functions.
- `util/org-collect-duplicate-headings` is now much faster.
- Added `util/org-helm-*` helm actions.
- Added ffip like grep commands `util/ffip-search-paths`, `util/ffip-search`,
  `util/ffip-grep-default`, `util/ffip-grep-git-files` and associated functions.
- Version bump to `0.3.0`.
