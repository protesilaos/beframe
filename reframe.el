;;; reframe.el --- WORK-IN-PROGRESS -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou General Issues <~protesilaos/general-issues@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/reframe
;; Mailing-List: https://lists.sr.ht/~protesilaos/general-issues
;; Version: 0.0.0
;; Package-Requires: ((emacs "27.1") (compat "29.1.3.2"))

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

(require 'compat)

(defgroup reframe ()
  "Isolate buffers per frame WORK-IN-PROGRESS."
  :group 'frames)

(defcustom reframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*")
  "List of buffer names to include in all frames.
These buffers are shown in the buffer selection prompts even if
they have not been used by---and thus associated with---the
current frame.

Otherwise framed buffers are limited to the frame that uses them."
  :group 'reframe
  :package-version '(reframe . "0.1.0")
  :type '(repeat string))

(defcustom reframe-project-always-in-frame nil
  "When non-nil, open projects in new frames with `reframe-mode'.

When the `reframe-mode' is enabled, install advice around
`project-prompt-project-dir' so that every prompt for a new
project is prefixed with `other-frame-prefix'."
  :group 'reframe
  :package-version '(reframe . "0.1.0")
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (reframe-mode 1))
  :type 'boolean)

(defun reframe--frame-buffers (&optional frame)
  "Produce list of buffers for either specified or current FRAME."
  (seq-filter
   (lambda (buf)
     (and (bufferp buf)
          (not (string-prefix-p " " (buffer-name buf)))))
   (frame-parameter frame 'buffer-list)))

(defun reframe--global-buffers ()
  "Return list of `reframe-global-buffers' buffer objects."
  (mapcar
   (lambda (name)
     (get-buffer name))
   reframe-global-buffers))

(defun reframe--buffer-list (&optional frame)
  "Return list of buffers that are used by the current frame.
With optional FRAME as an object that satisfies `framep', return
the list of buffers that are used by FRAME.

Include `reframe-global-buffers' in the list."
  (delete-dups
   (append (reframe--frame-buffers frame)
           (reframe--global-buffers))))

(defun reframe--buffer-names (&optional frame)
  "Return list of names of `reframe--buffer-list' as strings.
With optional FRAME, do it for the given frame name."
  (mapcar
   (lambda (buf)
     (buffer-name buf))
   (reframe--buffer-list frame)))

(defun reframe--read-buffer-p (buf &optional frame)
  "Return non-nil if BUF belongs to the current FRAME.
BUF is a string or a cons cell, per `reframe-read-buffer'.
If optional FRAME is nil, then default to the current one.  Else
it must satisfy `framep'."
  (let ((b buf))
    (when (consp buf)
      (setq b (car buf)))
    (unless (string-prefix-p " " b)
      (seq-contains-p (reframe--buffer-names frame) b #'string-match-p))))

(defvar reframe-history nil
  "Minibuffer history of frame specific buffers.")

;;;###autoload
(defun reframe-read-buffer (prompt &optional def require-match _predicate)
  "The `read-buffer-function' that limits buffers to frames.
PROMPT, DEF, REQUIRE-MATCH, and PREDICATE are the same as
`read-buffer'.  The PREDICATE is ignored, however, to apply the
per-frame filter."
  (completing-read prompt
                   (reframe--buffer-names)
                   #'reframe--read-buffer-p
                   require-match
                   nil
                   'reframe-history
                   def))

(defun reframe--buffer-prompt (&optional frame)
  "Prompt for buffer among `reframe--buffer-names'.

Use the previous buffer as the default value, such that
subsequent invocations of this command flip between the current
and previous buffers.

With optional FRAME, use list of buffers specific to the given
frame name."
  (read-buffer
   "Switch to frame buffer: "
   (other-buffer (current-buffer))
   (confirm-nonexistent-file-or-buffer)
   ;; NOTE: This predicate is not needed if `reframe-mode' is
   ;; non-nil because it sets the `read-buffer-function'.
   (lambda (buf)
     (reframe--read-buffer-p buf frame))))

(defun reframe--buffers-with-current ()
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

(defun reframe--frame-prompt ()
  "Prompt to select a frame among the list of frames."
  (let ((frames (reframe--buffers-with-current)))
    (completing-read "Select Frame: " frames nil t nil 'frame-name-history (caar frames))))

(defun reframe--frame-object (frame)
  "Retun frame object of named FRAME.
FRAME is the human-readable representation of a frame."
  (let* ((frames (reframe--buffers-with-current))
         (names (mapcar #'car frames)))
    (when (seq-contains-p names frame #'string-match-p)
      (alist-get frame frames nil nil #'string-match-p))))

;;;###autoload
(defun reframe-switch-buffer (buffer)
  "Switch to BUFFER in the current frame using completion.

Either bind this command to a key as an alternative to
`switch-to-buffer', or enable the minor mode
`reframe-mode' which makes all buffer prompts limit the
candidates to those that belong to the selected frame.

Also see `reframe-switch-buffer-in-frame'."
  (interactive (list (reframe--buffer-prompt)))
  (switch-to-buffer buffer))

;;;###autoload
(defun reframe-switch-buffer-in-frame (frame buffer)
  "Switch to BUFFER that belongs to FRAME.

BUFFER is selected with completion among a list of buffers that
belong to FRAME.

Either bind this command to a key as an alternative to
`switch-to-buffer', or enable the minor mode
`reframe-mode' which makes all buffer prompts limit the
candidates to those that belong to the selected frame.

Also see `reframe-switch-buffer'.

Raising and then selecting FRAME does not depend solely on Emacs.
The window manager must permit such an operation.  See bug#61319:
<https://debbugs.gnu.org/cgi/bugreport.cgi?bug=61319>."
  (interactive
   (let ((obj (reframe--frame-object (reframe--frame-prompt))))
     (list obj (reframe--buffer-prompt obj))))
  (select-frame-set-input-focus frame)
  (switch-to-buffer buffer))

;;; Minor mode setup

(defun reframe--rename-frame (frame)
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

(defun reframe--frame-parameter-p (buf)
  "Return non-nil if BUF belongs to the current frame.
BUF is a buffer object among `reframe--buffer-list'."
  (memq buf (reframe--buffer-list)))

(defun reframe--frame-predicate (&optional frame)
  "Set FRAME `buffer-predicate' parameter.
If FRAME is nil, use the current frame."
  (set-frame-parameter frame 'buffer-predicate #'reframe--frame-parameter-p))

(defvar reframe--read-buffer-function nil
  "Last value of `read-buffer-function'.")

(defun reframe--with-other-frame (&rest app)
  "Apply APP with `other-frame-prefix'.
Use this as :around advice to commands that must make a new frame."
  (funcall (compat-function other-frame-prefix))
  (apply app))

(declare-function project-prompt-project-dir "project")

;;;###autoload
(define-minor-mode reframe-mode
  "Make all buffer prompts limit candidates per frame."
  :global t
  (if reframe-mode
      (progn
        (setq reframe--read-buffer-function read-buffer-function
              read-buffer-function #'reframe-read-buffer)
        (add-hook 'after-make-frame-functions #'reframe--frame-predicate)
        (add-hook 'after-make-frame-functions #'reframe--rename-frame)
        (when reframe-project-always-in-frame
          (advice-add (compat-function project-prompt-project-dir) :around #'reframe--with-other-frame)))
    (setq read-buffer-function reframe--read-buffer-function
          reframe--read-buffer-function nil)
    (remove-hook 'after-make-frame-functions #'reframe--frame-predicate)
    (remove-hook 'after-make-frame-functions #'reframe--rename-frame)
    (advice-remove (compat-function project-prompt-project-dir) #'reframe--with-other-frame)))

;;;; Integration with `consult'

(defvar consult-buffer-sources)
(declare-function consult--buffer-state "consult")

(with-eval-after-load 'consult
  (defface reframe-buffer
    '((t :inherit font-lock-string-face))
    "Face for `consult' framed buffers.")

  (defvar reframe--consult-source
    `( :name     "Frame-specific buffers (current frame)"
       :narrow   ?F
       :category buffer
       :face     reframe-buffer
       :history  reframe-history
       :items    ,#'reframe--buffer-names
       :action   ,#'switch-to-buffer
       :state    ,#'consult--buffer-state))

  (add-to-list 'consult-buffer-sources 'reframe--consult-source))

(provide 'reframe)
;;; reframe.el ends here
