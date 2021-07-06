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
