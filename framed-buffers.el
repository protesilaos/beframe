;;; framed-buffers.el --- WORK-IN-PROGRESS -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou General Issues <~protesilaos/general-issues@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/framed-buffers
;; Mailing-List: https://lists.sr.ht/~protesilaos/general-issues
;; Version: 0.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; WORK-IN-PROGRESS

;;; Code:

(defgroup framed-buffers ()
  "Isolate buffers per frame WORK-IN-PROGRESS."
  :group 'frames)

(defcustom framed-buffers-global-buffers '("*scratch*" "*Messages*" "*Backtrace*")
  "List of buffer names to include in all frames.
These buffers are shown in the buffer selection prompts even if
they have not been used by---and thus associated with---the
current frame.

Otherwise framed buffers are limited to the frame that uses them."
  :type '(repeat string))

(defun framed-buffers--frame-buffers (&optional frame)
  "Produce list of buffers for either specified or current FRAME."
  (seq-filter
   (lambda (buf)
     (and (bufferp buf)
          (not (string-prefix-p " " (buffer-name buf)))))
   (frame-parameter frame 'buffer-list)))

(defun framed-buffers--global-buffers ()
  "Return list of `framed-buffers-global-buffers' buffer objects."
  (mapcar
   (lambda (name)
     (get-buffer name))
   framed-buffers-global-buffers))

(defun framed-buffers--buffer-list (&optional frame)
  "Return list of buffers that are used by the current frame.
With optional FRAME as an object that satisfies `framep', return
the list of buffers that are used by FRAME.

Include `framed-buffers-global-buffers' in the list."
  (delete-dups
   (append (framed-buffers--frame-buffers frame)
           (framed-buffers--global-buffers))))

(defun framed-buffers--buffer-names (&optional frame)
  "Return list of names of `framed-buffers--buffer-list' as strings.
With optional FRAME, do it for the given frame name."
  (mapcar
   (lambda (buf)
     (buffer-name buf))
   (framed-buffers--buffer-list frame)))

(defun framed-buffers--read-buffer-p (buf &optional frame)
  "Return non-nil if BUF belongs to the current FRAME.
BUF is a string or a cons cell, per `framed-buffers-read-buffer'.
If optional FRAME is nil, then default to the current one.  Else
it must satisfy `framep'."
  (let ((b buf))
    (when (consp buf)
      (setq b (car buf)))
    (unless (string-prefix-p " " b)
      (seq-contains-p (framed-buffers--buffer-names frame) b #'string-match-p))))

(defvar framed-buffers-history nil
  "Minibuffer history of frame specific buffers.")

;;;###autoload
(defun framed-buffers-read-buffer (prompt &optional def require-match _predicate)
  "The `read-buffer-function' that limits buffers to frames.
PROMPT, DEF, REQUIRE-MATCH, and PREDICATE are the same as
`read-buffer'.  The PREDICATE is ignored, however, to apply the
per-frame filter."
  (completing-read prompt
                   (framed-buffers--buffer-names)
                   #'framed-buffers--read-buffer-p
                   require-match
                   nil
                   'framed-buffers-history
                   def))

(defun framed-buffers--buffer-prompt (&optional frame)
  "Prompt for buffer among `framed-buffers--buffer-names'.

Use the previous buffer as the default value, such that
subsequent invocations of this command flip between the current
and previous buffers.

With optional FRAME, use list of buffers specific to the given
frame name."
  (read-buffer
   "Switch to frame buffer: "
   (other-buffer (current-buffer))
   (confirm-nonexistent-file-or-buffer)
   ;; NOTE: This predicate is not needed if `framed-buffers-mode' is
   ;; non-nil because it sets the `read-buffer-function'.
   (lambda (buf)
     (framed-buffers--read-buffer-p buf frame))))

(defun framed-buffers--buffers-with-current ()
  "Return frame list with current one renamed appropriately."
  (let ((frames (make-frame-names-alist)))
    (mapcar
     (lambda (frame)
       (let ((name (car frame))
             (obj (cdr frame)))
         (cond
          ((and (eq (selected-frame) obj)
                (string-prefix-p " " name))
           (setcar frame "Current frame")
           frame)
          ((eq (selected-frame) obj)
           (setcar frame (format "%s (Current frame)" name))
           frame)
          (t
           frame))))
     frames)))

(defun framed-buffers--frame-prompt ()
  "Prompt to select a frame among the list of frames."
  (let ((frames (framed-buffers--buffers-with-current)))
    (completing-read "Select Frame: " frames nil t nil 'frame-name-history (caar frames))))

(defun framed-buffers--frame-object (frame)
  "Retun frame object of named FRAME.
FRAME is the human-readable representation of a frame."
  (let* ((frames (framed-buffers--buffers-with-current))
         (names (mapcar #'car frames)))
    (when (seq-contains-p names frame #'string-match-p)
      (alist-get frame frames nil nil #'string-match-p))))

;;;###autoload
(defun framed-buffers-switch-buffer (buffer)
  "Switch to BUFFER in the current frame using completion.

Either bind this command to a key as an alternative to
`switch-to-buffer', or enable the minor mode
`framed-buffers-mode' which makes all buffer prompts limit the
candidates to those that belong to the selected frame.

Also see `framed-buffers-switch-buffer-in-frame'."
  (interactive (list (framed-buffers--buffer-prompt)))
  (switch-to-buffer buffer))

;;;###autoload
(defun framed-buffers-switch-buffer-in-frame (frame buffer)
  "Switch to BUFFER that belongs to FRAME.

BUFFER is selected with completion among a list of buffers that
belong to FRAME.

Either bind this command to a key as an alternative to
`switch-to-buffer', or enable the minor mode
`framed-buffers-mode' which makes all buffer prompts limit the
candidates to those that belong to the selected frame.

Also see `framed-buffers-switch-buffer'.

Raising and then selecting FRAME does not depend solely on Emacs.
The window manager must permit such an operation.  See bug#61319:
<https://debbugs.gnu.org/cgi/bugreport.cgi?bug=61319>."
  (interactive
   (let ((obj (framed-buffers--frame-object (framed-buffers--frame-prompt))))
     (list obj (framed-buffers--buffer-prompt obj))))
  (select-frame-set-input-focus frame)
  (switch-to-buffer buffer))

;;; Minor mode setup

(defun framed-buffers--rename-frame (frame)
  "Rename FRAME.
Add this to `after-make-frame-functions'."
  (select-frame frame)
  (set-frame-name
   (cond
    ((and (not (minibufferp)) (buffer-file-name))
     (format "%s  %s" (buffer-name) default-directory))
    ((not (minibufferp))
     (buffer-name))
    (t
     default-directory))))

(defun framed-buffers--frame-parameter-p (buf)
  "Return non-nil if BUF belongs to the current frame.
BUF is a buffer object among `framed-buffers--buffer-list'."
  (memq buf (framed-buffers--buffer-list)))

(defun framed-buffers--frame-predicate (&optional frame)
  "Set FRAME `buffer-predicate' parameter.
If FRAME is nil, use the current frame."
  (set-frame-parameter frame 'buffer-predicate #'framed-buffers--frame-parameter-p))

(defvar framed-buffers--read-buffer-function nil
  "Last value of `read-buffer-function'.")

;;;###autoload
(define-minor-mode framed-buffers-mode
  "Make all buffer prompts limit candidates per frame."
  :global t
  (if framed-buffers-mode
      (progn
        (setq framed-buffers--read-buffer-function read-buffer-function
              read-buffer-function #'framed-buffers-read-buffer)
        (add-hook 'after-make-frame-functions #'framed-buffers--frame-predicate)
        (add-hook 'after-make-frame-functions #'framed-buffers--rename-frame))
    (setq read-buffer-function framed-buffers--read-buffer-function
          framed-buffers--read-buffer-function nil)
    (remove-hook 'after-make-frame-functions #'framed-buffers--frame-predicate)
    (remove-hook 'after-make-frame-functions #'framed-buffers--rename-frame)))

;;;; Integration with `consult'

(defvar consult-buffer-sources)
(declare-function consult--buffer-state "consult")

(with-eval-after-load 'consult
  (defface framed-buffers-buffer
    '((t :inherit font-lock-string-face))
    "Face for `consult' framed buffers.")

  (defvar framed-buffers--consult-source
    `( :name     "Frame-specific buffers (current frame)"
       :narrow   ?F
       :category buffer
       :face     framed-buffers-buffer
       :history  framed-buffers-history
       :items    ,#'framed-buffers--buffer-names
       :action   ,#'switch-to-buffer
       :state    ,#'consult--buffer-state))

  (add-to-list 'consult-buffer-sources 'framed-buffers--consult-source))

(provide 'framed-buffers)
;;; framed-buffers.el ends here
