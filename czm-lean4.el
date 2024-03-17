;;; czm-lean4.el --- Embellishments for lean4-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-lean4.el
;; Package-Requires: ((emacs "29.1") (pos-tip) (consult "1.1") (lsp-mode "8.0.1") (lean4-mode)  (mmm-mode "0.5.9") (auctex) (czm-preview))
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
(require 'lsp-mode)
(require 'lean4-mode)
(require 'mmm-auto)
(require 'mmm-region)
(require 'preview)
(require 'czm-preview)

;; Could also just use forward-sentence/backward-sentence for the next
;; two functions

(defun czm-lean4--blank-or-comment-line-p ()
  "Return non-nil when the current line is blank or commented."
  (save-excursion
    (beginning-of-line)
    (or
     (looking-at-p
      (rx
       (or
        (seq bol (zero-or-more (syntax whitespace))
             eol)
        (seq bol (or "--" "/-")
             (zero-or-more nonl)
             eol))))
     (lean4-in-comment-p))))

;;;###autoload
(defun czm-lean4-cheap-beginning-of-defun ()
  "Move to last non-blank line after any blank lines."
  (interactive)
  (unless (bobp)
    (backward-char)
    (goto-char (line-beginning-position))
    (while (and (not (bobp))
                (czm-lean4--blank-or-comment-line-p))
      (forward-line -1))
    (while (and (not (bobp))
                (save-excursion
                  (forward-line -1)
                  (not (czm-lean4--blank-or-comment-line-p))))
      (forward-line -1))))

;;;###autoload
(defun czm-lean4-cheap-end-of-defun ()
  "Move to first blank line after some non-blank lines."
  (interactive)
  (while (and (not (eobp)) (czm-lean4--blank-or-comment-line-p))
    (forward-line 1))
  (while (and (not (eobp)) (not (czm-lean4--blank-or-comment-line-p)))
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
             (re-search-forward "^\\(noncomputable section\\|section\\|namespace\\|end\\|variable\\|open\\).*" nil t)
             (< (point)
                pos))
          (unless (lean4-in-comment-p)
            (let ((matched (match-string 0)))
              (cond
               ((string-match "^end" matched)
                (progn
                  (while (not (string-match "^\\s-*\\(noncomputable section\\|section\\|namespace\\)" (car my-stack)))
                    (pop my-stack))
                  (pop my-stack)
                  (setq indent-level (max 0 (1- indent-level)))))
               ((string-match "^\\(noncomputable section\\|section\\|namespace\\)" matched)
                (progn
                  (push (concat (make-string indent-level ? )
                                matched)
                        my-stack)
                  (setq indent-level (1+ indent-level))))
               (t                       ; variable or open
                (setq matched (buffer-substring-no-properties
                               (line-beginning-position)
                               (progn (forward-paragraph)
                                      (backward-char)
                                      (point))))
                (setq matched
                      (mapconcat (lambda (line)
                                   (concat (make-string indent-level ? )
                                           line))
                                 (split-string matched "\n")
                                 "\n"))
                (push matched
                      my-stack))))))))
    (let ((output (mapconcat 'identity (nreverse my-stack)
                             "\n")))
      (if (<= (prefix-numeric-value prefix)
              1)
          (pos-tip-show output nil nil nil 60)
        (with-output-to-temp-buffer "*Variable context for lean4*"
          (princ output))))))

;;;###autoload
(defun czm-lean4-magit-section-mode-hook ()
  "Hook to be used with *Lean Goal* window."
  (when (equal (buffer-name) "*Lean Goal*")
    (setq truncate-lines nil)
    ;; (visual-line-mode)
    ))

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
  '("def" "irreducible_def" "theorem" "inductive" "structure" "class" "instance" "axiom" "opaque")
  "List of headings to search for in Mathlib."
  :type '(repeat string)
  :group 'czm-lean4)

(defcustom czm-lean4-heading-prefixes
  '("private" "protected" "noncomputable")
  "List of prefixes to search for in Mathlib."
  :type '(repeat string)
  :group 'czm-lean4)

;;;###autoload
(defun czm-lean4-search-mathlib-headings ()
  "Search the Mathlib folder for theorems."
  (interactive)
  (let ((re (concat "^"
                    (regexp-opt (append czm-lean4-headings czm-lean4-heading-prefixes))
                    ;; the following would be more natural, but doesn't
                    ;; seem to work correctly with rg
                    ;;
                    ;; (regexp-opt
                    ;;  (mapcan (lambda (x)
                    ;;            (mapcar (lambda (y)
                    ;;                      (concat x y))
                    ;;                    czm-lean4-headings))
                    ;;          `(""
                    ;;            ,@(mapcar (lambda (x)
                    ;;                        (concat x " "))
                    ;;                      czm-lean4-heading-prefixes))))
                    )))
    (czm-lean4-search-mathlib (concat re " -- -g !Deprecated # "))))


(defun czm-lean4--insertion-helper (header footer region)
  "Helper function for inserting section namespace/comment blocks.
If REGION is nil, then insert HEADER and FOOTER, separated by
newlines, with point in between.  Otherwise, insert HEADER before
the region and FOOTER after the region."
  (if region
      (progn
        (goto-char (cdr region))
        (insert footer "\n")
        (goto-char (car region))
        (beginning-of-line)
        (insert header "\n")
        (forward-line))
    (insert header "\n")
    (save-excursion
      (insert "\n" footer "\n")))
  ;; this last bit updates the font coloring
  (lsp-on-change 0 (buffer-size)
                 (buffer-size)))

;;;###autoload
(defun czm-lean4-insert-section-or-namespace (&optional arg)
  "Insert a new section or namespace block.
With a prefix ARG, insert a namespace block.  Otherwise, insert a
section block."
  (interactive "P")
  (let* ((is-namespace (consp arg))
         (text (read-from-minibuffer "Enter name: "))
         (region-active (region-active-p))
         (header
          (concat
           (if is-namespace "namespace" "section")
           (unless (string-empty-p text)
             (concat " " text))))
         (footer
          (concat
           "end"
           (unless (string-empty-p text)
             (concat " " text)))))
    (czm-lean4--insertion-helper header footer (when region-active (cons (region-beginning)
                                                                         (region-end))))))


;;;###autoload
(defun czm-lean4-insert-comment-block ()
  "Insert a comment block."
  (interactive)
  (let* ((region-active (region-active-p))
         (header "/-\n")
         (footer "\n-/"))
    (czm-lean4--insertion-helper header footer (when region-active (cons (region-beginning)
                                                                         (region-end))))))

(defun czm-lean4--toggle-info-custom-display (action)
  "Toggle display of info buffer with ACTION."
  (let ((display-buffer-base-action action))
    (lean4-toggle-info-buffer lean4-info-buffer-name)
    (lean4-info-buffer-refresh)))

(defcustom czm-lean4-info-window-height-fraction 0.4
  "Fraction of window height to use for info buffer."
  :type 'number
  :group 'czm-lean4)

(defcustom czm-lean4-info-window-width-fraction 0.4
  "Fraction of window width to use for info buffer."
  :type 'number
  :group 'czm-lean4)

;;;###autoload
(defun czm-lean4-toggle-info-split-below ()
  "Show infos at the current point, split below."
  (interactive)
  (czm-lean4--toggle-info-custom-display
   `((display-buffer-below-selected display-buffer-reuse-window)
     (window-height . ,czm-lean4-info-window-height-fraction))))

;;;###autoload
(defun czm-lean4-toggle-info-split-right ()
  "Show infos at the current point, split right."
  (interactive)
  (czm-lean4--toggle-info-custom-display
   `((display-buffer-in-side-window)
     (side . right)
     (window-width . ,czm-lean4-info-window-width-fraction))))

(defconst czm-lean4-delimiter-list '("(" "{" "[")
  "List of delimiter characters for cycling.")

(defun czm-lean4--cycle-delimiter-helper (ch forward)
  "Helper function to cycle delimiters.
CH is one of the delimiter characters.
FORWARD is non-nil if we are cycling forward, nil otherwise.
This function assumes that point is on a delimiter character."
  (let* ((steps (if forward 1 -1))
         (delimiters-length (length czm-lean4-delimiter-list))
         (index (mod (+ (cl-position ch czm-lean4-delimiter-list :test #'string=) steps)
                     delimiters-length))
         (new-delimiter (elt czm-lean4-delimiter-list index))
         (end-of-sexp (save-excursion
                        (forward-sexp)
                        (backward-char)
                        (point))))
    (save-excursion
      (delete-char 1)
      (insert new-delimiter)
      (goto-char end-of-sexp)
      (delete-char 1)
      (insert
       (pcase new-delimiter
         ("(" ")")
         ("{" "}")
         ("[" "]"))))))

;;;###autoload
(defun czm-lean4-cycle-delimiter-forward ()
  "Cycle forward the delimiter at point: ( -> { -> [ -> ( -> ..."
  (interactive)
  (let ((ch (string (char-after (point)))))
    (if (member ch czm-lean4-delimiter-list)
        (czm-lean4--cycle-delimiter-helper ch t)
      (message "Point is not on a delimiter character."))))

;;;###autoload
(defun czm-lean4-cycle-delimiter-backward ()
  "Cycle backward the delimiter at point: ( -> [ -> { -> ( -> ..."
  (interactive)
  (let ((ch (string (char-after (point)))))
    (if (member ch czm-lean4-delimiter-list)
        (czm-lean4--cycle-delimiter-helper ch nil)
      (message "Point is not on a delimiter character."))))

;;;###autoload
(defun czm-lean4-format-function ()
  "Format function at point according to the library guidelines.
This function assumes that the function or theorem at point has
its proof indented correctly, in particular, by two spaces, and
attempts to format the hypotheses in the manner described at URL
`https://leanprover-community.github.io/contribute/style.html'."
  (interactive)
  (let* ((beg (point))
         (end (save-excursion (end-of-defun)
                              (point)))
         (colon-equals (save-excursion
                         (search-forward ":=" end t)
                         (while
                             (or
                              (> (car (syntax-ppss)) 0)
                              (save-excursion
                                     (beginning-of-line)
                                     (search-forward "let" (line-end-position) t)))
                           (search-forward ":=" end t))
                         (point)))
         (colon-equals-line-number (line-number-at-pos colon-equals)))
    ;; for lines below the current line up through the colon-equals
    ;; line, we make sure that they begin with four spaces.
    (save-excursion
      (goto-char beg)
      (forward-line)
      (while (and (< (point)
                     end)
                  (<= (line-number-at-pos)
                      colon-equals-line-number))
        (beginning-of-line)
        (when (looking-at " ")
          (delete-horizontal-space)
          (insert "    "))
        (forward-line)))))

;;;###autoload
(defun czm-lean4-format-buffer ()
  "Format function at point according to the library guidelines.
Applies `czm-lean4-format-function' to each function in the
buffer."
  (interactive)
  (goto-char (point-max))
  (while (> (point) (point-min))
    (beginning-of-defun)
    (when (looking-at
           (regexp-opt
            (append czm-lean4-headings czm-lean4-heading-prefixes '("open" "@["))))
      (czm-lean4-format-function))))

(defvar czm-lean4-tex-mode-map (make-sparse-keymap)
  "Keymap for `czm-lean4-tex-mode'.")

(define-minor-mode czm-lean4-tex-mode
  "Minor mode for latex blocks."
  :init-value nil
  :lighter nil
  :keymap czm-lean4-tex-mode-map)

(defun czm-lean4-tex--initialize ()
  "Initialize `czm-lean4-tex-mode'."
  (mmm-add-classes
   '((czm-lean4-tex
      :submode LaTeX-mode
      :face mmm-default-submode-face
      :front "/-%%"
      :back "%%-/"
      :save-matches 1
      :insert ((?s lean-latex nil @ "/-%%" @ "\n" _ "\n" @ "%%-/" @))
      :submode-hook (lambda () (czm-lean4-tex-mode 1)))))
  (mmm-add-mode-ext-class 'lean4-mode nil 'czm-lean4-tex))


(defun czm-lean4-tex--enable ()
  "Enable `czm-lean4-tex-mode' in the current buffer."
  (czm-lean4-tex-mode 1))

(defun czm-lean4-tex--disable ()
  "Disable `czm-lean4-tex-mode' in the current buffer."
  (czm-lean4-tex-mode 0))

(defun czm-lean4-tex-setup ()
  "Set up LaTeX preview for lean4-mode."
  (czm-lean4-tex--initialize)
  (add-hook 'mmm-latex-mode-enter-hook #'czm-lean4-tex--enable)
  (add-hook 'mmm-latex-mode-exit-hook #'czm-lean4-tex--disable))

(defcustom czm-lean4-TeX-master nil
  "TeX-master value to be used for AUCTeX preview."
  :type 'file
  :group 'czm-lean4)

;;;###autoload
(defun czm-lean4-mode-hook ()
  "Hook to be used with lean4-mode."
  (setq-local beginning-of-defun-function #'czm-lean4-cheap-beginning-of-defun)
  (setq-local end-of-defun-function #'czm-lean4-cheap-end-of-defun)
  (setq-local outline-regexp "\\(namespace\\|section\\|noncomputable section\\)\\>")
  (setq-local outline-level 'czm-lean4-outline-level)
  (czm-lean4-tex-setup))

(defun czm-lean4--current-mmm-LaTeX-region ()
  "Return (beg . end) for mmm-mode LaTeX region at point, or nil."
  (let ((overlays (overlays-at (point)))
        result)
    (while (and overlays (not result))
      (let* ((overlay (car overlays))
             (properties (overlay-properties overlay))
             (mmm-mode (plist-get properties 'mmm-mode)))
        (when (eq mmm-mode 'LaTeX-mode)
          (setq result (cons (overlay-start overlay) (overlay-end overlay))))
        (setq overlays (cdr overlays))))
    result))

;;;###autoload
(defun czm-lean4-preview-fold-block ()
  "Fold the current block and preview it.
Block delimited by /-%% and %%-/."
  (interactive)
  (save-excursion
    (let
        ;; ((beg (re-search-backward "/-%%" nil t))
        ;;  (end (re-search-forward "%%-/" nil t)))
        ((region (czm-lean4--current-mmm-LaTeX-region))
         (beg (car region))
         (end (cdr region)))
      (when (and beg end)
        (czm-preview-fold-region-anywhere beg end)))))

(provide 'czm-lean4)
;;; czm-lean4.el ends here
