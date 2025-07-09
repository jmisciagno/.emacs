;;; langtool-popup.el --- Popup message extension for langtool.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 Masahiro Hayashi

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: docs
;; URL: https://github.com/mhayashi1120/Emacs-langtool
;; Emacs: GNU Emacs 25 or later
;; Version: 1.1.2
;; Package-Requires: ((emacs "25.1") (popup "0.5.9"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ## Install:
;;
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'langtool-popup)
;;
;; Or use Melpa (https://melpa.org/)

;; ## Usage:
;;
;; This idea originally come from:
;; ref: https://laclefyoshi.hatenablog.com/entry/20150912/langtool_popup

;;; Code:

(require 'popup)

(declare-function langtool-details-error-message "langtool")

(defun langtool-popup-autoshow (overlays)
  "Popup LanguageTool message (on OVERLAYS) with `popup`."

  ;; Do not interrupt current popup
  (unless (or popup-instances
              ;; suppress popup after type `C-g' .
              (memq last-command '(keyboard-quit)))
    (let ((msg (langtool-details-error-message overlays)))
      (popup-tip msg))))

;; To keep Compat
(defalias 'langtool-popup-autoshow-detail #'langtool-popup-autoshow)

(defvar langtool-autoshow-message-function)
(setq langtool-autoshow-message-function #'langtool-popup-autoshow)

(declare-function langtool-autoshow-default-message "langtool")

;; To restore default while `unload-feature'
(defun langtool-popup-unload-function ()
  "Called when `unload-feature` ."
  (when (eq langtool-autoshow-message-function 'langtool-popup-autoshow)
    ;; FIXME: or get defcustom form (Unable get by `default-value`)
    (setq langtool-autoshow-message-function
          #'langtool-autoshow-default-message)))

(provide 'langtool-popup)

;;; langtool-popup.el ends here
