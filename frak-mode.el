;;; frak-mode.el
;;;
;;; Author: Dave Footitt
;;; URL: 
;;; Version: 0.1
;;; Keywords: frak, cat, lulz, pop tart cat, build something amazing
;;; 
;;; Inspired by (and in few places copied from) sml-modeline.el,
;;; written by Lennart Borgman
;;; See: http://bazaar.launchpad.net/~nxhtml/nxhtml/main/annotate/head%3A/util/sml-modeline.el

;; LICENSE
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup frak nil
  "Customization group for `frak-mode'."
  :group 'frames)

(defun frak-refresh ()
  "Refresh after option changes if loaded."
  (when (featurep 'frak-mode)
    (when (and (boundp 'frak-mode)
               frak-mode)
      (frak-mode -1)
      (frak-mode 1))))

(defcustom frak-bar-length 32
  "Length of Frak bar in units; each unit is equal to an 8px
  image. Minimum of 3 units are required for Frak."
  :set (lambda (sym val)
         (set-default sym val)
         (frak-refresh))
  :group 'frak)

(defconst +frak-directory+ (file-name-directory (or load-file-name buffer-file-name)))
(defconst +frak-cat-size+ 3)
(defconst +frak-cat-image+ (concat +frak-directory+ "img/nyan.xpm")) ; yoyo
(defconst +frak-rainbow-image+ (concat +frak-directory+ "img/trogg.xpm"))
(defconst +frak-outerspace-image+ (concat +frak-directory+ "img/outerspace.xpm"))
(defconst +frak-yoyo-image+ (concat +frak-directory+ "img/yoyo.xpm"))

(defvar frak-cat-image (create-image +frak-cat-image+ 'xpm nil :ascent 'center))

(defun frak-get-anim-frame ()
  frak-cat-image)

(defun frak-number-of-rainbows () ; number of yoyos
  (round (/ (* (round (* 100
                         (/ (- (float (point))
                               (float (point-min)))
                            (float (point-max)))))
               (- frak-bar-length +frak-cat-size+))
          100)))

(defun frak-create ()
  (let* ((rainbows (frak-number-of-rainbows))
         (outerspaces (- frak-bar-length rainbows +frak-cat-size+))
         (rainbow-string "")
         (frakcat-string (propertize "[]*"
                                     'display (frak-get-anim-frame)))
         (outerspace-string ""))
    (dotimes (number rainbows)
      (setq rainbow-string (concat rainbow-string
                                   (propertize "|"
                                               'display (create-image +frak-rainbow-image+ 'xpm nil :ascent 'center)))))

    (dotimes (number outerspaces)
      (setq outerspace-string (concat outerspace-string
                                      (propertize "-"
                                                  'display (create-image +frak-outerspace-image+ 'xpm nil :ascent 'center)))))
    ;; Compute Frak Cat string.
    (concat rainbow-string
            frakcat-string
            outerspace-string)))

(defvar frak-old-car-mode-line-position nil)

;;;###autoload
(define-minor-mode frak-mode
  "Use Trogg's yoyo to show buffer size and position in mode-line.
You can customize this minor mode, see option `frak-mode'.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'."
  :global t
  :group 'frak
  (if frak-mode
      (progn
        (unless frak-old-car-mode-line-position
          (setq frak-old-car-mode-line-position (car mode-line-position)))
        (setcar mode-line-position '(:eval (list (frak-create)))))
    (setcar mode-line-position frak-old-car-mode-line-position)))

(provide 'frak-mode)

;;; frak-mode.el ends here
