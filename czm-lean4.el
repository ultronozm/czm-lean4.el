;;; czm-lean4.el --- Embellishments for lean4-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-lean4.el
;; Package-Requires: ((emacs "29.1") (pos-tip) (consult "1.1"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Embellishments for lean4-mode.  See README.org.

;;; Code:

(require 'pos-tip)
(require 'project)
(require 'consult)

;; Could also just use forward-sentence/backward-sentence for the next
;; two functions

;;;###autoload
(defun czm-lean4-cheap-beginning-of-defun ()
  "Move to last non-blank line after any blank lines."
  (interactive)
  (unless (bobp)
    (backward-char)
    (goto-char (line-beginning-position))
    (while (and (not (bobp))
                (looking-at-p "^\\s-*$"))
      (forward-line -1))
    (while (and (not (bobp))
                (save-excursion
                  (forward-line -1)
                  (not (looking-at-p "^\\s-*$"))))
      (forward-line -1))))

;;;###autoload
(defun czm-lean4-cheap-end-of-defun ()
  "Move to first blank line after some non-blank lines."
  (interactive)
  (while (and (not (eobp)) (looking-at-p "^\\s-*$"))
    (forward-line 1))
  (while (and (not (eobp)) (not (looking-at-p "^\\s-*$")))
    (forward-line 1)))

;;;###autoload
(defun czm-lean4-show-variables (&optional prefix)
  "Display current namespace/section and active variables.
By default, show the result in a pop-up, via `pos-tip-show'.
With a PREFIX argument, use a separate buffer."
  (interactive "P")
  (let ((my-stack '())
        (indent-level 0)
        (case-fold-search nil)
        (pos (point)))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while
            (and
             (re-search-forward "^\\(section\\|namespace\\|end\\|variable\\).*" nil t)
             (< (point)
                pos))
          (let ((matched (match-string 0)))
            (cond
             ((string-match "^end" matched)
              (progn
                (while (not (string-match "^\\s-*\\(section\\|namespace\\)" (car my-stack)))
                  (pop my-stack))
                (pop my-stack)
                (setq indent-level (max 0 (1- indent-level)))))
             ((string-match "^\\(section\\|namespace\\)" matched)
              (progn
                (push (concat (make-string indent-level ? )
                              matched)
                      my-stack)
                (setq indent-level (1+ indent-level))))
             (t
              (push (concat (make-string indent-level ? )
                            matched)
                    my-stack)))))))
    (let ((output (mapconcat 'identity (nreverse my-stack)
                             "\n")))
      (if (<= (prefix-numeric-value prefix)
              1)
          (pos-tip-show output nil nil nil 60)
        (with-output-to-temp-buffer "*Variable context for lean4*"
          (princ output))))))

;;;###autoload
(defun czm-lean4-mode-hook ()
  "Hook to be used with lean4-mode."
  (setq-local beginning-of-defun-function #'czm-lean4-cheap-beginning-of-defun)
  (setq-local end-of-defun-function #'czm-lean4-cheap-end-of-defun)
  (setq-local outline-regexp "\\(namespace\\|section\\)\\>")
  (setq-local outline-level 'czm-lean4-outline-level))

(defun czm-lean4-outline-level ()
  "Find outline level of current line in a lean4 document."
  (let ((count 0))
(save-restriction
      (widen)
      (save-excursion
        (beginning-of-line)
        (while (> (point)
                  (point-min))
          (cond ((looking-at outline-regexp)
                 (setq count (1+ count)))
                ((looking-at "end ")
                 (setq count (1- count))))
          (forward-line -1))))
    count))

; the following couple defuns were taken from https://github.com/leanprover/lean4-mode/issues/22, due to felipeochoa

(defvar czm-lean4-pause-info nil "If non-nil, pause info buffer updates.")

;;;###autoload
(defun czm-lean4-info-buffer-redisplay (old-fun &rest args)
  "Suppress call to OLD-FUN if `czm-lean4-pause-info' is non-nil.
Otherwise, call with ARGS.

Credit: felipeochoa, https://github.com/leanprover/lean4-mode/issues/22."
  (unless czm-lean4-pause-info
    (apply old-fun args)))

;;;###autoload
(defun czm-lean4-toggle-info-pause ()
  "Toggle pausing of automatic info refresh.

Credit: felipeochoa, https://github.com/leanprover/lean4-mode/issues/22."
  (interactive)
  (setq czm-lean4-pause-info (not czm-lean4-pause-info)))

(defun czm-lean4-mathlib-path ()
  "Get the path to the Mathlib folder."
  (let* ((project-root (expand-file-name (project-root (project-current))))
         (root-folder-name (file-name-nondirectory (directory-file-name project-root))))
    (cond ((string= root-folder-name "mathlib")
           ;; Get parent of 'project-root' and append "Mathlib"
           (expand-file-name  "Mathlib" project-root))
          ((file-exists-p (concat project-root ".lake/packages/mathlib/Mathlib"))
           ;; Append ".lake/packages/mathlib/Mathlib" to project-root
           (expand-file-name  ".lake/packages/mathlib/Mathlib" project-root) )
          (t
           nil))))

(defcustom czm-lean4-search-function #'consult-ripgrep
  "Function to use for searching in lean4-mode."
  :type 'function
  :group 'czm-lean4)

;;;###autoload
(defun czm-lean4-search-mathlib (&optional initial)
  "Search the Mathlib folder with given INITIAL input."
  (interactive)
  (let ((mathlib-path (czm-lean4-mathlib-path)))
    (if mathlib-path
        (funcall czm-lean4-search-function mathlib-path initial)
      (message "Mathlib path not found."))))

(defcustom czm-lean4-headings
  '("def" "theorem" "inductive" "structure" "class" "instance" "axiom" "opaque")
  "List of headings to search for in Mathlib."
  :type '(repeat string)
  :group 'czm-lean4)

;;;###autoload
(defun czm-lean4-search-mathlib-headings ()
  "Search the Mathlib folder for theorems."
  (interactive)
  (let ((re (concat "^" (regexp-opt czm-lean4-headings 'words))))
    (czm-lean4-search-mathlib (concat re " -- -g !Deprecated # "))))

(provide 'czm-lean4)
;;; czm-lean4.el ends here
