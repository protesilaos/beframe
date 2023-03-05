;;; beframe.el --- Isolate buffers per frame -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou General Issues <~protesilaos/general-issues@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/beframe
;; Mailing-List: https://lists.sr.ht/~protesilaos/general-issues
;; Version: 0.1.11
;; Package-Requires: ((emacs "28.1"))

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
;; `beframe' enables a frame-oriented Emacs workflow where each frame
;; has access to the list of buffers visited therein.  In the interest
;; of brevity, we call buffers that belong to frames "beframed".
;; Beframing is achieved in three main ways:
;;
;; 1. By calling the command `beframe-switch-buffer'.  It is like the
;;    standard `switch-to-buffer' except the list of candidates is
;;    limited to those that the current frame knows about.
;;
;; 2. By enabling the global minor mode `beframe-mode'.  It sets the
;;    `read-buffer-function' to one that filters buffers per frame.  As
;;    such, commands like `switch-to-buffer', `next-buffer', and
;;    `previous-buffer' automatically work in a beframed way.
;;
;; 3. The command `beframe-buffer-menu' produces a dedicated buffer with
;;    a list of buffers for the current frame.  This is the counterpart
;;    of `beframe-switch-buffer'.  When called with a prefix argument
;;    (`C-u' with default key bindings), it prompts for a frame whose
;;    buffers it will display.
;;
;; Producing multiple frames does not generate multiple buffer lists.
;; There still is only one global list of buffers.  Beframing them simply
;; filters the list.
;;
;; The user option `beframe-global-buffers' contains a list of strings
;; that represent buffers names.  Those buffers are never beframed and
;; are available in all frames.  The default value contains the buffers
;; `*scratch*', `*Messages*', and `*Backtrace*'.
;;
;; The user option `beframe-create-frame-scratch-buffer' allows
;; `beframe-mode' to create a frame-specific scratch buffer that runs
;; the `initial-major-mode'.  This is done upon the creation of a new
;; frame and the scratch buffer is named after the frame it belongs
;; to.  For example, if the frame is called `modus-themes', the
;; corresponding scratch buffer is `*scratch for modus-themes*'.  Set
;; this user option to `nil' to disable the creation of such scratch
;; buffers.
;;
;; The user option `beframe-kill-frame-scratch-buffer' is the
;; counterpart of `beframe-create-frame-scratch-buffer'.  It kills the
;; frame-specific scratch buffer after the frame is deleted.  Set this
;; user option to `nil' to disable the killing of such buffers.
;;
;; The command `beframe-assume-frame-buffers' (alias
;; `beframe-add-frame-buffers') prompts for a frame and then copies
;; its buffer list into the current frame.
;;
;; The command `beframe-unassume-frame-buffers' (alias
;; `beframe-remove-frame-buffers') prompts for a frame and then
;; removes its buffer list from the current frame.
;;
;; The `beframe-mode' does the following:
;;
;; - Sets the value of `read-buffer-function' to a function that
;;   beframes all commands that read that variable.  This includes the
;;   likes of `switch-to-buffer', `next-buffer', and `previous-buffer'.
;;
;; - Add a filter to newly created frames so that their
;;   `buffer-predicate' parameter beframes buffers.
;;
;; - Renames newly created frames so that they have a potentially more
;;   meaningful title.  The user option `beframe-rename-function'
;;   specifies the function that handles this process.  When its value
;;   is nil, no renaming is performed.
;;
;; - When the user option `beframe-functions-in-frames' contains a list
;;   of functions, it makes them run with `other-frame-prefix', meaning
;;   that they are called in a new frame.  The default value of that
;;   user option affects the `project.el' project-switching selection:
;;   the new project buffer appears in its own frame and, thus, becomes
;;   part of a beframed list of buffers, isolated from all other frames.
;;
;; - Handles the creation and deletion of frame-specific scratch
;;   buffers,per the user options `beframe-create-frame-scratch-buffer',
;;   `beframe-kill-frame-scratch-buffer'.
;;
;; Development note: `beframe' is in its early days.  The minor mode may
;; be revised to have more features and/or greater flexibility.
;;
;; The `consult' package by Daniel Mendler (also known as @minad)
;; provides commands that extend the functionality of the generic
;; minibuffer.  Among them is `consult-buffer'.  With that command the
;; user can search for buffers, recent files, and bookmarks, among
;; others.
;;
;; `consult-buffer' displays a global list of buffers and is never
;; beframed.  This is done by design.  Instead of forcing it to only show
;; beframed buffers, we get the best of both worlds: (i) a convenient way
;; to access the global list of buffers and (ii) a new entry to the
;; `consult-buffer-sources' that registers beframed buffers as their own
;; group.  Users can narrow directly to this group by typing `F' followed
;; by `SPC'.
;;
;; Backronym: Buffers Encapsulated in Frames Realise Advanced
;; Management of Emacs.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup beframe ()
  "Isolate buffers per frame."
  :group 'frames)

(defcustom beframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*")
  "List of buffer names to include in all frames.
These buffers are shown in the buffer selection prompts even if
they have not been used by---and thus associated with---the
current frame.

Otherwise framed buffers are limited to the frame that uses them."
  :group 'beframe
  :package-version '(beframe . "0.1.0")
  :type '(repeat string))

(defcustom beframe-create-frame-scratch-buffer t
  "Create a frame-specific scratch buffer for new frames.
Do it when `beframe-mode' is enabled.

The frame-specific scratch buffer runs `initial-major-mode'.

Also see `beframe-kill-frame-scratch-buffer'."
  :group 'beframe
  :package-version '(beframe . "0.2.0")
  :type 'boolean)

(defcustom beframe-kill-frame-scratch-buffer t
  "Kill the frame-specific scratch buffer when the frame is deleted.
Do it when `beframe-mode' is enabled.

Also see `beframe-create-frame-scratch-buffer'."
  :group 'beframe
  :package-version '(beframe . "0.2.0")
  :type 'boolean)

(defcustom beframe-functions-in-frames nil
  "Functions that use new frame when `beframe-mode' is enabled.

When `beframe-mode' is enabled, install advice around each
function so that every invocation of it is called with
`other-frame-prefix'."
  :group 'beframe
  :package-version '(beframe . "0.1.0")
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (beframe--functions-in-frames :disable)
         (set-default symbol value)
         (beframe--functions-in-frames))
  :type '(repeat symbol))

(defcustom beframe-rename-function #'beframe-rename-frame
  "Function that renames new frames when `beframe-mode' is enabled.

The function accepts one argument, the current frame, as is
called by the `after-make-frame-functions' hook.

If nil, no renaming is performed."
  :group 'beframe
  :package-version '(beframe . "0.2.0")
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (beframe--functions-in-frames :disable)
         (set-default symbol value)
         (beframe--functions-in-frames))
  :type '(repeat symbol))

(defun beframe--frame-buffers (&optional frame)
  "Produce list of buffers for either specified or current FRAME."
  (seq-filter
   (lambda (buf)
     (and (bufferp buf)
          (not (string-prefix-p " " (buffer-name buf)))))
   (frame-parameter frame 'buffer-list)))

(defun beframe--global-buffers ()
  "Return list of `beframe-global-buffers' buffer objects."
  (mapcar
   (lambda (name)
     (get-buffer name))
   beframe-global-buffers))

(cl-defun beframe--buffer-list (&optional frame &key sort)
  "Return list of buffers that are used by the current frame.
With optional FRAME as an object that satisfies `framep', return
the list of buffers that are used by FRAME.

The key SORT may be a function taking the list of buffers as an
argument, and returning a new list to be used instead.  This can,
for example, be used to prefer hidden buffers to visible ones—see
`beframe-buffer-sort-visibility'.

Include `beframe-global-buffers' in the list."
  (funcall (or sort #'identity)
           (delq nil
                 (delete-dups
                  (append (beframe--frame-buffers frame)
                          (beframe--global-buffers))))))

(cl-defun beframe--buffer-names (&optional frame &key sort)
  "Return list of names of `beframe--buffer-list' as strings.
With optional FRAME, do it for the given frame name.  With key
SORT, apply this sorting function—see `beframe--buffer-list' for
more information."
  (mapcar
   (lambda (buf)
     (buffer-name buf))
   (beframe--buffer-list frame :sort sort)))

(defun beframe--read-buffer-p (buf &optional frame)
  "Return non-nil if BUF belongs to the current FRAME.
BUF is a string or a cons cell, per `beframe-read-buffer'.
If optional FRAME is nil, then default to the current one.  Else
it must satisfy `framep'."
  (let ((b buf))
    (when (consp buf)
      (setq b (car buf)))
    (unless (string-prefix-p " " b)
      (seq-contains-p (beframe--buffer-names frame) b))))

(defvar beframe-history nil
  "Minibuffer history of frame specific buffers.")

;;;###autoload
(defun beframe-read-buffer (prompt &optional def require-match _predicate)
  "The `read-buffer-function' that limits buffers to frames.
PROMPT, DEF, REQUIRE-MATCH, and PREDICATE are the same as
`read-buffer'.  The PREDICATE is ignored, however, to apply the
per-frame filter."
  (completing-read prompt
                   (beframe--buffer-names)
                   #'beframe--read-buffer-p
                   require-match
                   nil
                   'beframe-history
                   def))

(defun beframe--buffer-prompt (&optional frame)
  "Prompt for buffer among `beframe--buffer-names'.

Use the previous buffer as the default value, such that
subsequent invocations of this command flip between the current
and previous buffers.

With optional FRAME, use list of buffers specific to the given
frame name."
  (read-buffer
   "Switch to frame buffer: "
   (other-buffer (current-buffer))
   (confirm-nonexistent-file-or-buffer)
   ;; NOTE: This predicate is not needed if `beframe-mode' is
   ;; non-nil because it sets the `read-buffer-function'.
   (lambda (buf)
     (beframe--read-buffer-p buf frame))))

(defun beframe--buffers-with-current ()
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

(defun beframe--frame-prompt ()
  "Prompt to select a frame among the list of frames."
  (let ((frames (beframe--buffers-with-current)))
    (completing-read "Select Frame: " frames nil t nil 'frame-name-history (caar frames))))

(defun beframe--frame-object (frame)
  "Retun frame object of named FRAME.
FRAME is the human-readable representation of a frame."
  (let* ((frames (beframe--buffers-with-current))
         (names (mapcar #'car frames)))
    (when (seq-contains-p names frame #'string-match-p)
      (alist-get frame frames nil nil #'string-match-p))))

;;;###autoload
(defun beframe-switch-buffer (buffer)
  "Switch to BUFFER in the current frame using completion.

Either bind this command to a key as an alternative to
`switch-to-buffer', or enable the minor mode
`beframe-mode' which makes all buffer prompts limit the
candidates to those that belong to the selected frame.

Also see `beframe-switch-buffer-in-frame'."
  (interactive (list (beframe--buffer-prompt)))
  (switch-to-buffer buffer))

;;;###autoload
(defun beframe-switch-buffer-in-frame (frame buffer)
  "Switch to BUFFER that belongs to FRAME.

BUFFER is selected with completion among a list of buffers that
belong to FRAME.

Either bind this command to a key as an alternative to
`switch-to-buffer', or enable the minor mode
`beframe-mode' which makes all buffer prompts limit the
candidates to those that belong to the selected frame.

Also see `beframe-switch-buffer'.

Raising and then selecting FRAME does not depend solely on Emacs.
The window manager must permit such an operation.  See bug#61319:
<https://debbugs.gnu.org/cgi/bugreport.cgi?bug=61319>."
  (interactive
   (let ((obj (beframe--frame-object (beframe--frame-prompt))))
     (list obj (beframe--buffer-prompt obj))))
  (select-frame-set-input-focus frame)
  (switch-to-buffer buffer))

(cl-defun beframe--list-buffers-noselect (&optional frame &key sort)
  "Produce a buffer list of buffers for optional FRAME.
When FRAME is nil, use the current one.  With key SORT, apply
this sorting function—see `beframe--buffer-list' for more
information.

This is a simplified variant of `list-buffers-noselect'."
  (let* ((frame (if (framep frame) frame (selected-frame)))
         (name (frame-parameter frame 'name))
         (old-buf (current-buffer))
         (buf (get-buffer-create (format "*Buffer List for %s*" name)))
         (buffer-list (beframe--buffer-list frame :sort sort)))
    (with-current-buffer buf
      (Buffer-menu-mode)
      (setq-local Buffer-menu-files-only nil
                  Buffer-menu-buffer-list buffer-list
                  Buffer-menu-filter-predicate nil)
      (list-buffers--refresh buffer-list old-buf)
      (tabulated-list-print))
    buf))

;;;###autoload
(cl-defun beframe-buffer-menu (&optional frame &key sort)
  "Produce a `buffer-menu' for the current FRAME.
With FRAME as a prefix argument, prompt for a frame.  When called
from Lisp, FRAME satisfies `framep'.  With key SORT, apply this
sorting function—see `beframe--buffer-list' for more information.

The bespoke buffer menu is displayed in a window using
`display-buffer'.  Configure `display-buffer-alist' to control
its placement and other parameters."
  (interactive
   (list
    (when current-prefix-arg
      (beframe--frame-object (beframe--frame-prompt)))))
  (display-buffer (beframe--list-buffers-noselect frame :sort sort)))

;;;###autoload
(defun beframe-assume-frame-buffers (frame)
  "Assume FRAME buffer list, copying it into current buffer list.
When called interactively, prompt for FRAME using completion.
Else FRAME must satisfy `framep'.

Also see `beframe-unassume-frame-buffers'."
  (interactive (list (beframe--frame-object (beframe--frame-prompt))))
  (let* ((other-buffer-list (beframe--buffer-list frame))
         (buffers (delete-dups (append other-buffer-list (beframe--buffer-list)))))
    (modify-frame-parameters
     nil
     `((buffer-list . ,buffers)))))

(defalias 'beframe-add-frame-buffers 'beframe-assume-frame-buffers
  "Alias of `beframe-assume-frame-buffers' command.")

;;;###autoload
(defun beframe-unassume-frame-buffers (frame)
  "Unassume FRAME buffer list, removing it from current buffer list.
When called interactively, prompt for FRAME using completion.
Else FRAME must satisfy `framep'.

Also see `beframe-assume-frame-buffers'."
  (interactive (list (beframe--frame-object (beframe--frame-prompt))))
  (let* ((other-buffer-list (beframe--buffer-list frame))
         (new-buffer-list
          (seq-filter
           (lambda (buf)
             (not (member buf other-buffer-list)))
           (beframe--buffer-list))))
    (modify-frame-parameters
     nil
     `((buffer-list . ,new-buffer-list)))))

(defalias 'beframe-remove-frame-buffers 'beframe-unassume-frame-buffers
  "Alias of `beframe-unassume-frame-buffers' command.")

;; TODO 2023-03-02: Define commands to `completing-read-multiple'
;; assume/unassume buffers.

;;; Minor mode setup

(defvar beframe--read-buffer-function nil
  "Last value of `read-buffer-function'.")

;;;###autoload
(define-minor-mode beframe-mode
  "Make all buffer prompts limit candidates per frame."
  :global t
  (if beframe-mode
      (progn
        (setq beframe--read-buffer-function read-buffer-function
              read-buffer-function #'beframe-read-buffer)
        (add-hook 'after-make-frame-functions #'beframe--frame-predicate)
        (when beframe-rename-function
          (add-hook 'after-make-frame-functions beframe-rename-function))
        (when beframe-create-frame-scratch-buffer
          (add-hook 'after-make-frame-functions #'beframe-create-scratch-buffer))
        (beframe--functions-in-frames))
    (setq read-buffer-function beframe--read-buffer-function
          beframe--read-buffer-function nil)
    (remove-hook 'after-make-frame-functions #'beframe--frame-predicate)
    (remove-hook 'after-make-frame-functions beframe-rename-function)
    (remove-hook 'after-make-frame-functions #'beframe-create-scratch-buffer)
    (beframe--functions-in-frames :disable)))

(defun beframe-create-scratch-buffer (frame)
  "Create scratch buffer in `initial-major-mode' for FRAME."
  (let* ((name (frame-parameter frame 'name))
         (buf (get-buffer-create (format "*scratch for %s*" name))))
    (with-current-buffer buf
      (funcall initial-major-mode)
      (when (and (zerop (buffer-size))
                 (stringp initial-scratch-message))
        (insert initial-scratch-message))
      (add-hook 'delete-frame-functions
                (lambda (frame)
                  (when (and beframe-kill-frame-scratch-buffer
                             (null frame))
                    (kill-buffer buf)))))
    (let* ((frame-bufs (beframe--buffer-list frame))
           (frame-bufs-with-buf (append frame-bufs (list buf))))
      (modify-frame-parameters
       frame
       `((buffer-list . ,frame-bufs-with-buf))))))

(defvar beframe--rename-frame-history nil
  "Minibuffer history for `beframe-rename-frame'.")

;; (defun beframe--rename-scratch-buffer (frame name)
;;   "Rename the scratch buffer associated with FRAME according to NAME."
;;   (when-let* ((buf (get-buffer (format "*scratch for %s*" frame)))
;;               ((member (format "*scratch for %s*" frame) (beframe--buffer-list))))
;;     (with-current-buffer buf
;;       (rename-buffer (format "*scratch for %s*" name)))))

(defun beframe--infer-frame-name (frame name)
  "Infer a suitable name for FRAME with given NAME.
See `beframe-rename-frame'."
  (let* ((buffer (car (frame-parameter frame 'buffer-list)))
         (file-name (when (bufferp buffer)
                      (buffer-file-name buffer)))
         (buf-name (buffer-name buffer))
         (dir (with-current-buffer buffer
                default-directory)))
    (cond
     ((and name (stringp name))
      name)
     ((and (not (minibufferp)) file-name)
      (format "%s  %s" buf-name dir))
     ((not (minibufferp))
      buf-name)
     (t
      dir))))

;;;###autoload
(defun beframe-rename-frame (frame &optional name)
  "Rename FRAME per `beframe-rename-function'.

When called interactively, prompt for FRAME.  Else accept FRAME
if it is an object that satisfies `framep'.

With optional NAME as a string, use it to name the given FRAME.
When called interactively, prompt for NAME when a prefix argument
is given.

With no NAME argument try to infer a name based on the following:

- If the current window has a buffer that visits a file, name the
  FRAME after the file's name and its `default-directory'.

- If the current window has a non-file-visiting buffer, use the
  `buffer-name' as the FRAME name.

- Else use the `default-directory'.

Remember that this function doubles as an example for
`beframe-rename-function': copy it and modify it accordingly."
  (interactive
   (let ((select-frame (beframe--frame-prompt)))
     (list
      (beframe--frame-object select-frame)
      (when current-prefix-arg
        (read-string
         (format "Rename the frame now called `%s' to: "
                 select-frame)
         nil 'beframe--rename-frame-history select-frame)))))
  ;; (when name
  ;;   (beframe--rename-scratch-buffer frame name))
  (modify-frame-parameters
   frame
   (list (cons 'name (beframe--infer-frame-name frame name)))))

(defun beframe--frame-buffer-p (buf &optional frame)
  "Return non-nil if BUF belongs to the current frame.
Use optional FRAME to test if BUF belongs to it."
  (memq buf (beframe--buffer-list frame)))

(defun beframe--frame-predicate (&optional frame)
  "Set FRAME `buffer-predicate' parameter.
If FRAME is nil, use the current frame."
  (set-frame-parameter frame 'buffer-predicate #'beframe--frame-buffer-p))

(defun beframe--with-other-frame (&rest app)
  "Apply APP with `other-frame-prefix'.
Use this as :around advice to commands that must make a new
frame.  See `beframe-functions-in-frames'."
  (funcall #'other-frame-prefix)
  (apply app))

(defun beframe--functions-in-frames (&optional disable)
  "Install advice for `beframe-functions-in-frames'.
With optional DISABLE remove the advice."
  (dolist (cmd beframe-functions-in-frames)
    (cond
     (disable
      (advice-remove cmd #'beframe--with-other-frame))
     (beframe-mode
      (advice-add cmd :around #'beframe--with-other-frame)))))

(defun beframe-buffer-sort-visibility (buffers)
  "Sort the given BUFFERS by visibility.
Concretely, this means this function will return a sequence that
first lists hidden, then visible, and then the current buffer."
  (let* ((current (current-buffer))
         (bufs (seq-group-by
                (lambda (buf)
                  (cond ((eq buf current)                 :current)
                        ((get-buffer-window buf 'visible) :visible)
                        (t                                :hidden)))
                buffers)))
    (nconc (alist-get :hidden  bufs)
           (alist-get :visible bufs)
           (alist-get :current bufs))))

(provide 'beframe)
;;; beframe.el ends here
