;;; browse-register.el --- interactively insert items from register -*- coding: utf-8 -*-

;; Copyright (C) 2015 Lee San <skyer9@gmail.com>

;; Author: Lee San <skyer9@gmail.com>
;; Maintainer: Lee San <skyer9@gmail.com>
;; Created: 2015-03-01
;; Version: 0.1
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; This is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This library lets you list and edit registers.  M-x `browse-register'
;; displays a list of currently set registers.

;; This list is similar to that of `browse-kill-ring': you can delete
;; registers with `d'.  If you want

;; Hitting RET on a value string will jump to the register's location or
;; add the text to buffer.  Hitting RET on a register's type will
;; restrict the list to registers of this type.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'browse-register)
;;
;; Many code is from register-list.el, browse-kill-ring.el and bs.el.
;; Very thanks to those developers.
;;
;;; Todo:
;;
;;; History:
;;
;; - [2015-03-01] Released v0.1
;;
;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'derived))

(when (featurep 'xemacs)
  (require 'overlay))

(defgroup browse-register nil
  "Interactively list/edit registers."
  :tag "Register List"
  :group 'convenience)

(define-derived-mode browse-register-mode special-mode "BrowseRegister"
  "Major mode for editing a list of register keys.

Each line is of the form:

\[Delete-flag] Key Type Value

The leftmost column displays a `D' character if the register key
is flagged for further deletion.  You can add such flag by hitting
\\[register-list-delete].

The Key column displays the character used for this register.
Hitting \\[register-list-call-handler-at-point] on the key will
prompt for a replacement.

The Type column displays the type of the register, either [F]rame
\[N]umber [M]arkers [R]ectangle [S]string or [W]window.  Hitting
\\[register-list-call-handler-at-point] on this column will
restrict the register list to this type of registers.  To quickly
list a specific type, hit the type character among [FNMRSW].

The Value column displays information about the value of the
register: either a string if the register's value is a string, a
number or a rectangle, or the location of the marker or some
information about window and frame configuration.  Hitting
\\[register-list-call-handler-at-point] on this column will
copy the string to the kill ring or jump to the location.

\\[register-list-quit] -- quit the browse-register."
  (setq truncate-lines t)
  (setq buffer-read-only t)

  (define-key browse-register-mode-map "q" 'browse-register-quit)
  (define-key browse-register-mode-map (kbd "RET") 'browse-register-call-handler-at-point)
  (define-key browse-register-mode-map "+" 'browse-register-increment-key)
  (define-key browse-register-mode-map "-" 'browse-register-decrement-key)
  (define-key browse-register-mode-map "g" 'browse-register-refresh)
  (define-key browse-register-mode-map "d" 'browse-register-delete-register))

(defvar browse-register-current-fontification nil
  "Whether the value strings are currently fontified.")

(defvar browse-register-original-window-config nil
  "The window configuration to restore for `browse-register-quit'.")
  (make-variable-buffer-local 'browse-register-original-window-config)

(defconst browse-register-header-lines-length 2
  "Number of lines for headers in Browse Register Buffer.")

(defcustom browse-register-preserve-fontification nil
  "Non-nil means keep the value strings fontified."
  :type 'integer
  :group 'browse-register)

(defcustom browse-register-maximum-display-length nil
  "Whether or not to limit the length of displayed items.

If this variable is an integer, the display of `register' will be
limited to that many characters.
Setting this variable to nil means no limit."
  :type '(choice (const :tag "None" nil)
                 integer)
  :group 'browse-register)

(defcustom browse-register-hook nil
  "A list of functions to call after `browse-register'."
  :type 'hook
  :group 'browse-register)

(defcustom browse-register-resize-window nil
  "Whether to resize the `browse-register' window to fit its contents.
Value is either t, meaning yes, or a cons pair of integers,
 (MAXIMUM . MINIMUM) for the size of the window.  MAXIMUM defaults to
the window size chosen by `pop-to-buffer'; MINIMUM defaults to
`window-min-height'."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (cons (integer :tag "Maximum") (integer :tag "Minimum")))
  :group 'browse-register)

(defcustom browse-register-default-types "[FNMRSW]"
  "A regexp matching the default register types to list.

The available types are: [F]rame [N]umber [M]arkers [R]ectangle
\[S]string and [W]window.  [FW] will list markers, frame and
window configuration, [SM] will list strings and markers, etc."
  :type 'regexp
  :group 'browse-register)

(defcustom browse-register-string-width nil
  "Maximum width for the register value string."
  :type 'integer
  :group 'browse-register)

(defun browse-register-elide (str)
  (let ((typ (browse-register-get-type str)))
    (if (and browse-register-maximum-display-length
             (string= "S" typ)
             (> (length str)
                browse-register-maximum-display-length))
        (concat (substring str 0 (- browse-register-maximum-display-length 3)) "...")
      str)))

(if (fboundp 'fit-window-to-buffer)
    (defalias 'browse-register-fit-window 'fit-window-to-buffer)
  (defun browse-register-fit-window (window max-height min-height)
    (setq min-height (or min-height window-min-height))
    (setq max-height (or max-height (- (frame-height) (window-height) 1)))
    (let* ((window-min-height min-height)
           (windows (count-windows))
           (config (current-window-configuration)))
      (enlarge-window (- max-height (window-height)))
      (when (> windows (count-windows))
        (set-window-configuration config))
      (if (/= (point-min) (point-max))
          (shrink-window-if-larger-than-buffer window)
        (shrink-window (- (window-height) window-min-height))))))

(defun browse-register-resize-window ()
  (when browse-register-resize-window
    (apply #'browse-register-fit-window (selected-window)
           (if (consp browse-register-resize-window)
               (list (car browse-register-resize-window)
                     (or (cdr browse-register-resize-window)
                         window-min-height))
             (list nil window-min-height)))))

(defun browse-register-get-type (key)
  "Get the type for register's KEY."
  (if (atom key)
    (cond ((stringp key) "S")
          ((numberp key) "N")
          ((markerp key) "M")
          (t "error[1]"))
    (cond ((window-configuration-p (car key)) "W")
          ((frame-configuration-p (car key)) "F")
          ((stringp (car key)) "R")
          ;;((string= "file-query" (car key)) "B")
          ((string= "Unprintable entity" (car key)) "?")
          (t (format "error[2] : %s" (car key))))))

(defun browse-register-get-handler (register type)
  "Return a handler function for a REGISTER with TYPE."
  ;; (message (format "aaa %s"
  ;;                  (char-to-string (car register))))
  (cond ((string= "?" type)
     `(lambda() (message "No action with this type")))
    ((string= "S" type)
     `(lambda()
        (browse-register-quit)
        (insert-register ,(car register))))
    ((string= "N" type)
     `(lambda()
        (browse-register-quit)
        (insert-register ,(car register))))
    ((string= "R" type)
     `(lambda()
        (kill-new ,(mapconcat 'identity (cdr register) "\n"))
        (message "Rectangle copied to the kill ring")))
    ((string-match "[FMW]" type)
     `(lambda()
        (browse-register-quit)
        (jump-to-register ,(car register))))))

(defmacro browse-register-preserve-pos (force-line &rest body)
  "Preserve the position and execute BODY.
If FORCE-LINE is non-nil, force moving to this line."
  `(let ((line (line-number-at-pos (point)))
	 (col (current-column)))
     ,@body
     (goto-char (point-min))
     (line-move ,(or (eval force-line) '(1- line)) t)
     (line-move-to-column col)))

(defun browse-register-prepare-string (string &optional fontify)
  "Prepare STRING for the register list.
An optional argument FONTIFY takes precedence over
`browse-register-preserve-fontification' to decide whether the
string should keep its original fontification.  Also shorten the
output string to `browse-register-string-width'."
  (if (and browse-register-string-width
	    (> (length string) browse-register-string-width))
    (setq string (substring string 0 browse-register-string-width)))
  (when (or fontify browse-register-preserve-fontification)
    (remove-text-properties 0 (length string) '(face nil) string))
  string)

(defun browse-register-value-to-string (value type)
  "Convert a register VALUE into a string according to its TYPE."
  (cond ((string= "M" type)
	 (cond ((marker-position value)
		(format "[Marker at point %d in buffer %s]"
			(marker-position value)
			(buffer-name (marker-buffer value))))
	       ((marker-buffer value)
		(format "[Marker in buffer %s]"
			(buffer-name (marker-buffer value))))
	       (t (format "[Marker gone?]"))))
	((string= "N" type)
	 (format "Number: %s" (number-to-string value)))
	((string= "S" type)
	 (replace-regexp-in-string "[\n\r\t]" " " value))
	((string= "R" type)
	 (mapconcat 'identity value "\\ "))
	((string= "W" type)
	 (format "[Window configuration in frame \"%s\"]"
		  (frame-parameter
		   (window-configuration-frame (car value)) 'name)))
	((string= "F" type)
	 (format "[Frame configuration]"))
	(t "[Error: unknow type]")))

(defun browse-register-quit ()
  "Take the action specified by `browse-register-quit-action'."
  (interactive)
  ;; aaaaaaaaaaaa
  ;; (when browse-register-preview-overlay
  ;;   (delete-overlay browse-register-preview-overlay))
      (if (< emacs-major-version 24)
        (let (buf (current-buffer))
             (set-window-configuration browse-register-original-window-config)
           (kill-buffer buf))
       (quit-window)))

(defun browse-register-call-handler-at-point nil
  "Call the register handler at point.
If the point is on a register key, edit the key.  If the point is
on a register type, rebuild the list restricting to registers of
this type.  If the point is on a register value, either jump to
the register or copy its value into the kill ring."
  (interactive)
  ;;(message "kkk 111")
  (let ((register (browse-register-get-current-register)))
    (if register
      (condition-case nil
        (let* (;;(key (char-to-string (car register)))
               (val (browse-register-elide (cdr register)))
               (typ (browse-register-get-type val))
               (hdl (browse-register-get-handler register typ)))
          ;;(message "111")
          (funcall hdl))
	      (error (message "Unknown error."))))))

(defun browse-register-get-current-register ()
  "Return register on current line.
Raise an error if not on a register line."
  (beginning-of-line)
  (let ((line (+ (- browse-register-header-lines-length)
		 (count-lines 1 (point)))))
    (when (< line 0)
      (error "You are on a header row"))
    (when (>= line (length register-alist))
      (error "Register not exits."))
    (message "aaa %d %d" line (length register-alist))
    (nth line register-alist)))

(defun browse-register-increment-key nil
  "Increment the key of the register at point."
  (interactive)
  (browse-register-set-key '1+))

(defun browse-register-decrement-key nil
  "Decrement the key of the register at point."
  (interactive)
  (browse-register-set-key '1-))

(defun browse-register-set-key (function)
  "Update the regsiter key by applying FUNCTION."
  (let* ((register (browse-register-get-current-register))
         (reg register)
         (key (char-to-string (car register)))
         (val (browse-register-elide (cdr register)))
         (typ (browse-register-get-type val))
         (hdl (browse-register-get-handler register typ)))
    (if (string= "N" typ)
      (progn
        (setq register-alist (delete reg register-alist))
        (add-to-list 'register-alist (cons (car register) (funcall function val)))
        (browse-register-sort-register)
        (browse-register-refresh))
      (error "Only valid for Number."))))

(defun browse-register-delete-register nil
  "Delete register at point."
  (interactive)
  (let* ((register (browse-register-get-current-register))
         (key (char-to-string (car register)))
         (val (browse-register-elide (cdr register)))
         (typ (browse-register-get-type val))
         (hdl (browse-register-get-handler register typ)))
    (setq register-alist (delete register register-alist))
    (browse-register-refresh)))

(defun browse-register-sort-register ()
  "Sort register."
  (setq register-alist (sort
                        register-alist
                        (lambda (a b) (string< (char-to-string (car a)) (char-to-string(car b)))))))

(defun browse-register-refresh (&optional type fontify)
  "Refresh the list of registers."
  (interactive "P")
  (browse-register-preserve-pos nil
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (concat (propertize "% Key  Type  Value\n"
      'face 'font-lock-type-face
      'intangible t)
      (propertize "- ---  ----  -------------------------------------------\n"
      'intangible t
      'face 'font-lock-comment-delimiter-face)))

    (setq type (or type browse-register-default-types))
    (setq browse-register-current-fontification
          (or fontify browse-register-current-fontification))

    (browse-register-sort-register)

    (mapc
     (lambda (register)
       (let* ((key (char-to-string (car register)))
              (val (browse-register-elide (cdr register)))
              (typ (browse-register-get-type val))
              (hdl (browse-register-get-handler register typ)))
         ;;(message (format "bbb %s %s" typ type))
         (when (string-match typ type)
           (insert
            (format "  %s    %s   %s\n"
                    (propertize key
                                'face
                                'bold)
                    (propertize (concat "[" typ "]"))
                    (propertize (browse-register-prepare-string
                                 (browse-register-value-to-string val typ) fontify)))))))
     register-alist)

    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))














;;;###autoload
(defun browse-register (&optional type fontify)
  "Display items in the `register' in another buffer."
  (interactive)
  (if (eq major-mode 'browse-register-mode)
      (message "Already viewing the register")
    (let* ((orig-win (selected-window))
           (orig-buf (window-buffer orig-win))
           (buf (get-buffer-create "*Register*")))
      (pop-to-buffer buf)
      (browse-register-resize-window)
      (browse-register-refresh type fontify)
      (goto-char (point-min))
      (browse-register-mode)

      )))

(provide 'browse-register)

;;; browse-register.el ends here