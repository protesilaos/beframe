;;; beframe-transient.el --- Transient user interface for Beframe -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/beframe

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
;; Transient user interface for Beframe commands.

;;; Code:

(require 'transient)

;; TODO 2025-01-23: How to process the prefix argument in a transient?

;;;###autoload (autoload 'beframe-transient-assume "beframe-transient")
(transient-define-prefix beframe-transient-assume nil
  "Beframe commands to assume buffers."
  ["Beframe"
   ["Assume buffers (absorb into this frame)"
    ("f" "All the buffers of the given FRAME" beframe-assume-frame-buffers)
    ("s" "All the SELECTED buffers using completion (, to select another)" beframe-assume-buffers-selectively-all-frames)
    ("S" "All the SELECTED buffers of the given frame using completion (, to select another)" beframe-assume-frame-buffers-selectively)
    ("a" "All the buffers from EVERYWHERE"  beframe-assume-all-buffers-no-prompts)
    ("r" "All buffer NAMES matching the REGEXP" beframe-assume-buffers-matching-regexp)]])

;;;###autoload (autoload 'beframe-transient-unassume "beframe-transient")
(transient-define-prefix beframe-transient-unassume nil
  "Beframe commands to unassume buffers."
  ["Beframe"
   ["Unassume buffers (eject from this frame)"
    ("f" "All the buffers of the given FRAME" beframe-unassume-frame-buffers)
    ("s" "All the SELECTED buffers using completion (, to select another)" beframe-unassume-current-frame-buffers-selectively)
    ("a" "ALL buffers except `beframe-global-buffers'"  beframe-unassume-all-buffers-no-prompts)
    ("r" "All buffer NAMES matching the REGEXP" beframe-unassume-buffers-matching-regexp)]])

;;;###autoload (autoload 'beframe-transient "beframe-transient")
(transient-define-prefix beframe-transient nil
  "Beframe commands."
  ["Beframe"
   ["Switch"
    ("b" "Current frame buffer" beframe-switch-buffer)
    ("B" "Buffer of given frame" beframe-switch-buffer-in-frame)
    ("l" "List buffers" beframe-buffer-menu)]
   ["Rename (`beframe-rename-function')"
    ("r" "Rename this frame" beframe-rename-current-frame)
    ("R" "Rename a given frame" beframe-rename-frame)]
   ["Modify"
    ("a" "Assume buffers..." beframe-transient-assume)
    ("u" "Unassume buffers..." beframe-transient-unassume)]])

(provide 'beframe-transient)
;;; beframe-transient.el ends here
