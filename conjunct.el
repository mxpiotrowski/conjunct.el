;;; conjunct.el --- transpose conjuncts around conjunction
;;;
;;; Author: Cerstin Mahlow <cerstin@mahlow.ch>
;;	Michael Piotrowski <mxp@dynalabs.de>
;;; Keywords: editing, transpose
;;; Created: 2008-05-28
;;; Version: 1.7

;; Copyright (C) 2013 Cerstin Mahlow, Michael Piotrowski

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Note: This version of `conjunct-mode' uses overlays and works with
;; GNU Emacs and XEmacs.

;; Sometimes during writing you recognize that it would be nice to change
;; word order within a coordination: e.g., instead of "editing and
;; revising" you may prefer "revising and editing".  Placing the point on
;; the conjunction and calling `conjunct-mode' will help you doing this
;; "nice 'n easy."

;; Put this in your init.el to bind conjunct-mode to M-C-t in
;; text-mode, TeX-mode, and message-mode:

;;  (require 'conjunct)
;;  (define-key text-mode-map [(meta control t)] 'conjunct-mode)
;;  (add-hook 'TeX-mode-hook
;;	    (lambda ()
;;	      (define-key LaTeX-mode-map [(meta control t)] 'conjunct-mode)))
;;  (define-key message-mode-map [(meta control t)] 'conjunct-mode)

;; When entering conjunct-mode, the word on which the point is placed
;; (the conjunction) will be marked, as well as the two words
;; immediately to the right and the left of the conjunction.  The
;; right word is called "right conjunct", the left word is called
;; "left conjunct."

;; If one or both of the conjuncts comprise more than one word, you
;; can expand them.  You can also reduce both conjuncts. Of course,
;; each conjunct has to consist of at least one word.

;; If you like to transpose heads of phrases or words not directly
;; adjacent to the conjunction, you can reduce and expand the
;; conjuncts to mark the desired word(s).

;; You can expand, reduce and transpose conjuncts as often as you
;; want.  You are free to choose the right order of commands.

;; Leaving conjunct-mode returns you to the previous mode.  The marks
;; for the conjunction and the conjuncts disappear.  You can exit
;; conjunct-mode at any time, it is not necessary to actually
;; transpose the conjuncts!

;;; Code:

;; variables
(defvar conjunct-mode-hook nil
  "If non-nil, its value is called on entry to conjunct-mode.
conjunct-mode is invoked by the command \\[conjunct-mode].")

(defvar conjunct-mode-old-local-map
  "Needed for restoring the previous local map")
(defvar conjunct-mode-old-mode-name
  "Needed for restoring the previous mode name")
(defvar conjunct-mode-old-major-mode
  "Needed for restoring the previous major mode")

(defvar conjunct-mode-old-undo-list nil
  "Holds the state of `buffer-undo-list' before entering `conjunct-mode'.")

;; overlays
(defvar conjunct-overlay-conjunct
  "The conjunction")
(defvar conjunct-overlay-right
  "The right conjunct")
(defvar conjunct-overlay-left
  "The left conjunct")

;; faces used for marking the conjunction and the conjuncts
(copy-face 'default 'conjunct-conjunction-face)
(set-face-background 'conjunct-conjunction-face "hotpink")
(copy-face 'default 'conjunct-left-face)
(set-face-background 'conjunct-left-face "lightskyblue")
(copy-face 'default 'conjunct-right-face)
(set-face-background 'conjunct-right-face "limegreen")

;; utility function
(defun overlay-string (overlay)
  "Return the string delimited by the bounds of OVERLAY.
This function corresponds to `extent-string' in XEmacs."
  (unless (overlayp overlay)
    (error "Argument to `overlay-string' must be an overlay"))
  (buffer-substring (overlay-start overlay) (overlay-end overlay)))

;; conjunct keymap, entry and exit points.
(defconst conjunct-mode-map nil)

(if (not conjunct-mode-map)
    (progn
      (setq conjunct-mode-map (make-keymap 'conjunct-mode-map))
      ;; disable standard key-bindings
      (suppress-keymap conjunct-mode-map)
      ;; keys to be used
      (define-key conjunct-mode-map "\C-c\C-c" 'conjunct-mode-exit)
      (define-key conjunct-mode-map "t" 'conjunct-transpose-conjuncts)
      (define-key conjunct-mode-map "l" 'conjunct-expand-left-start)
      (define-key conjunct-mode-map [(left)] 'conjunct-expand-left-start)
      (define-key conjunct-mode-map "\M-l" 'conjunct-expand-left-end)
      (define-key conjunct-mode-map "r" 'conjunct-expand-right-end)
      (define-key conjunct-mode-map [(right)] 'conjunct-expand-right-end)
      (define-key conjunct-mode-map "\M-r" 'conjunct-expand-right-start)
      (define-key conjunct-mode-map "L" 'conjunct-reduce-left-start)
      (define-key conjunct-mode-map [(down)] 'conjunct-reduce-left-start)
      (define-key conjunct-mode-map "\C-L" 'conjunct-reduce-left-end)
      (define-key conjunct-mode-map "R" 'conjunct-reduce-right-end)
      (define-key conjunct-mode-map [(up)] 'conjunct-reduce-right-end)
      (define-key conjunct-mode-map"\C-R" 'conjunct-reduce-right-start)))

(defun conjunct-mark-conjunction ()
  "Marks current conjunction"
  ;; remember current point position used in every function to start
  ;; expanding, reducing and transposing always from the
  ;; conjunction-point
  (save-excursion 
    (forward-word 1) ; got to the end of the conjunction
    (let ((word-end (point)))
      (backward-word 1) ; go to the beginning of the conjunction
      (setq conjunct-overlay-conjunct (make-overlay (point) word-end))
      (overlay-put conjunct-overlay-conjunct 'face 'conjunct-conjunction-face))))

(defun conjunct-mark-left ()
  "Marks left conjunct"
  (save-excursion
    (forward-word 1)
    (backward-word 2) ; go to the beginning of the left conjunct
    (let ((word-start (point)))
      (forward-word 1) ; got to the end of the left conjunct
      (setq conjunct-overlay-left (make-overlay word-start (point)))
      (overlay-put conjunct-overlay-left 'face 'conjunct-left-face))))

(defun conjunct-mark-right ()
  "Marks right conjunct"
  (save-excursion
    (forward-word 2) ; got to the end of the right conjunct
    (let ((word-end (point)))
      (backward-word 1) ; go to the beginning of the right conjunct 
      (setq conjunct-overlay-right (make-overlay (point) word-end))
      (overlay-put conjunct-overlay-right 'face 'conjunct-right-face))))

(defun conjunct-expand-right-end ()
  "Expands the right conjunct to the right"
  (interactive)
  (save-excursion
    ;; start at end of right conjunct
    (let ((start (overlay-end conjunct-overlay-right)))
      (goto-char start)
      ;; new end point one word to the right
      (forward-word 1)
      (move-overlay conjunct-overlay-right
		    (overlay-start conjunct-overlay-right) (point)))))

(defun conjunct-expand-right-start ()
  "Expand the right conjunct to the left."
  (interactive)
  (save-excursion
    ;; start at start of right conjunct
    (let ((start (overlay-start conjunct-overlay-right)))
      (goto-char start)
      ;; right conjunct must not be extended beyond the conjunction
      (save-excursion
	(backward-word 1)
	(if (< (overlay-end conjunct-overlay-conjunct)
	       (point))
	    (progn
	      (move-overlay conjunct-overlay-right
			    (point)
			    (overlay-end conjunct-overlay-right)))
	  (ding))))))

(defun conjunct-expand-left-start ()
  "Expand the left conjunct to the left."
  (interactive)
  (save-excursion
    ;; start at start of left conjunct
    (let ((start (overlay-start conjunct-overlay-left)))
      (goto-char start)
      ;; new start point one wort to the left
      (backward-word 1)
      (move-overlay conjunct-overlay-left
		    (point) (overlay-end conjunct-overlay-left)))))

(defun conjunct-expand-left-end ()
  "Expands the left conjunct to the right"
  (interactive)
  (save-excursion
    ;; start at end of left conjunct
    (let ((start (overlay-end conjunct-overlay-left)))
      (goto-char start)
      ;; left conjunct must not be extended beyond the conjunction
      (save-excursion
	(forward-word 1)
	(if (< (point) 
	       (overlay-start conjunct-overlay-conjunct))
	    (progn
	      ;; new end point one wort to the right
	      (move-overlay conjunct-overlay-left 
			    (overlay-start conjunct-overlay-left)
			    (point)))
	  (ding))))))


(defun conjunct-reduce-right-end ()
  "Reduce the right conjunct by one word from the right.  The right conjunct
has to consist of at least one word."
  (interactive)
  (save-excursion
    ;; start at end point of right conjunct
    (let ((start (overlay-end conjunct-overlay-right)))
      (goto-char start)
      ;; leave min. 1 word
      (if (> (count-words-region
	      (overlay-start conjunct-overlay-right) (point)) 1)
	  (progn
	    ;; new end point one word to the left
	    (backward-word 1)
	    (move-overlay conjunct-overlay-right 
			  (overlay-start conjunct-overlay-right) 
			  (- (point) 1)))
	(ding)))))

(defun conjunct-reduce-right-start ()
  "Reduce the right conjunct by one word from the left.  The right conjunct
has to consist of at least one word."
  (interactive)
  (save-excursion
    ;; start at start point of right conjunct
    (let ((start (overlay-start conjunct-overlay-right)))
      (goto-char start)
      ;; leave min. 1 word
      (if (> (count-words-region (point) (overlay-end conjunct-overlay-right)) 1)
	  (progn
	    ;; new start point one word to the left
	    (forward-word 1)
	    (move-overlay conjunct-overlay-right
			  (+ (point) 1)
			  (overlay-end conjunct-overlay-right)))
	(ding)))))

(defun conjunct-reduce-left-start ()
  "Reduce the left conjunct by one word from the left.  The left conjunct
has to consist of at least one word."
  (interactive)
  (save-excursion
    ;; start at start point of left conjunct
    (let ((start (overlay-start conjunct-overlay-left)))
      (goto-char start)
      ;; leave min. 1 word
      (if (> (count-words-region (point) (overlay-end conjunct-overlay-left)) 1)
	  (progn
	    ;; new start at one word to the right
	    (forward-word 1)
	    (move-overlay conjunct-overlay-left 
			  (+ (point) 1)
			  (overlay-end conjunct-overlay-left)))
	(ding)))))

(defun conjunct-reduce-left-end ()
  "Reduce the left conjunct by one word from the right.  The left conjunct
has to consist of at least one word."
  (interactive)
  (save-excursion
    ;; start at end point of left conjunct
    (let ((start (overlay-end conjunct-overlay-left)))
      (goto-char start)
      ;; leave min 1 word
      (if (> (count-words-region (overlay-start conjunct-overlay-left) (point)) 1)
	  (progn
	    ;; new end at one word to the right
	    (backward-word 1)
	    (move-overlay conjunct-overlay-left 
			  (overlay-start conjunct-overlay-left)
			  (- (point) 1)))
	(ding)))))

(defun conjunct-transpose-conjuncts ()
  "Transpose marked conjuncts"
  (interactive)
  (save-excursion
    (let ((l (overlay-string conjunct-overlay-left))
	  (r (overlay-string conjunct-overlay-right)))
      ;; delete the left conjunct and replace it with the right conjunct
      ;; string is marked as left conjunct for further processing
      (goto-char (overlay-start conjunct-overlay-left))
      (delete-char (- (overlay-end conjunct-overlay-left) 
		      (overlay-start conjunct-overlay-left)))
      (insert r)
      (move-overlay conjunct-overlay-left (- (point) (length r)) (point))
      (goto-char (overlay-start conjunct-overlay-right))
      (delete-char (- (overlay-end conjunct-overlay-right) 
		      (overlay-start conjunct-overlay-right)))
      (insert l)
      (move-overlay conjunct-overlay-right (- (point) (length l)) (point)))))

(defun conjunct-mode ()
  "Special major mode for transposing words (the conjuncts)
around a pivot word (the conjunction).

When entering conjunct-mode, the word on which the point is
placed (the conjunction) will be marked, as well as the two words
immediately to the right and the left of the conjunction.  The
right word is called \"right conjunct\", the left word is called
\"left conjunct.\"

If one or both of the conjuncts comprise more than one word, you
can expand them:

  \\[conjunct-expand-right-end]:	expand the right conjunct by one word to the right

  \\[conjunct-expand-left-start]:	expand the left conjunct by one word to the left

You can also reduce both conjuncts.  Of course, each conjunct has
to consist of at least one word:

  \\[conjunct-reduce-right-end]:	reduce the right conjunct by one word from the right

  \\[conjunct-reduce-left-start]:	reduce the left conjunct by one word from the left

If you like to transpose heads of phrases or words not directly
adjacent to the conjunction, you can reduce and expand the
conjuncts to mark the desired word(s).

  \\[conjunct-reduce-right-start]:	reduce the right conjunct by one word from the left

  \\[conjunct-reduce-left-end]:	reduce the left conjunct by one word from the right

  \\[conjunct-expand-right-start]:	expand the right conjunct by one word to the left

  \\[conjunct-expand-left-start]:	expand the left conjunct by one word to the right

To transpose the conjuncts press:

  \\[conjunct-transpose-conjuncts]:	transpose the marked conjuncts

You can expand, reduce and transpose conjuncts as often as you
want.  You are free to choose the right order of commands.

Leave conjunct-mode by typing \\[conjunct-mode-exit], which returns you to the previous mode.
The marks for the conjunction and the conjuncts disappear.  You can exit
conjunct-mode at any time, it is not necessary to actually transpose the
conjuncts!

Keymap overview:

\\{conjunct-mode-map}
"
  (interactive)
  (when (eq major-mode 'conjunct-mode)
    (error "You are already editing a conjunction."))

  ;; store keymap and name of the previous mode
  (make-local-variable 'conjunct-mode-old-local-map)
  (setq conjunct-mode-old-local-map (current-local-map))
  (use-local-map conjunct-mode-map)
  (make-local-variable 'conjunct-mode-old-mode-name)
  (setq conjunct-mode-old-mode-name mode-name)
  (make-local-variable 'conjunct-mode-old-major-mode)
  (setq conjunct-mode-old-major-mode major-mode)

  ;; insert boundary marker into undo list
  (undo-boundary)

  ;; start a new undo list
  (make-local-variable 'conjunct-mode-old-undo-list)
  (setq conjunct-mode-old-undo-list buffer-undo-list)
  (setq buffer-undo-list nil)

  ;; set current mode
  (setq major-mode 'conjunct-mode)
  (setq mode-name "Conjunct")
  (run-hooks 'conjunct-mode-hook)

  ;; local variables
  (make-local-variable 'conjunct-extent-conjunct)
  (make-local-variable 'conjunct-extent-right)
  (make-local-variable 'conjunct-extent-left)

  ;; mark conjunction
  (conjunct-mark-conjunction)

  ;; mark conjuncts
  (conjunct-mark-left)
  (conjunct-mark-right)

  ;; message at entering conjunct-mode
  (message
   (substitute-command-keys
    "Type \\[conjunct-mode-exit] in this buffer to return it to %s mode.")
   conjunct-mode-old-mode-name))

(defun conjunct-mode-exit ()
  "Quit conjunct-mode and return to previous major mode"
  (interactive)
  (unless (eq major-mode 'conjunct-mode)
    (error "Not in conjunct-mode"))

  ;; restore previous major mode, mode name and key bindings
  (setq mode-name conjunct-mode-old-mode-name)
  (use-local-map conjunct-mode-old-local-map)
  (setq major-mode conjunct-mode-old-major-mode)
  (if (boundp 'redraw-modeline)
      (redraw-modeline) ; XEmacs
    (force-mode-line-update))    

  ;; delete overlays
  (delete-overlay conjunct-overlay-conjunct)
  (delete-overlay conjunct-overlay-right)
  (delete-overlay conjunct-overlay-left)

  ;; remove all boundary elements from the undo list, so that
  ;; everything done during conjunct mode will be treated as a
  ;; *single* change group
  (delete nil buffer-undo-list)

  ;; append the old undo list to the current undo list
  (nconc buffer-undo-list conjunct-mode-old-undo-list)

  ;; insert a final undo boundary
  (undo-boundary)

  ;; clean up
  (kill-local-variable 'conjunct-mode-old-undo-list))


(provide 'conjunct)

;;; conjunct.el ends here
