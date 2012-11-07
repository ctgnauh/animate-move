;;;  animate-move.jl --- Smoothly slide windows across the screen.

;; Copyright (C) 2012 Tristan Huang <tristan.j.huang@gmail.com>

;; Author: Rafal Strzalinski <nablaone@gmail.com>
;; Maintainer: Tristan Huang <tristan.j.huang@gmail.com>
;; URL: https://github.com/aquallor/animate-move/
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Commentary:
;;
;; Some functions to asynchronous animated moving windows.
;;

;; INSTALLATION:
;;
;; Put this file in your /sawfish/lisp/path/ and add this:
;;
;; (require 'animate-move)
;; (bind-keys window-keymap
;;	   "W-x" 'toggle-window-shaded
;;	   "W-z" 'iconify-window
;;	   "W-c" 'animate-center-window
;;	   "W-C" 'center-window
;;	   "W-k" 'delete-window
;;	   "W-r" 'rotate-move)
;;
;; to your .sawfishrc file.
;;

;; Code:

(require 'timers)
(require 'sawfish.wm.misc)
(provide 'animate-move)

;;; some misc stuff
(defmacro dolist (spec &rest body)
  `(progn 
     (mapc #'(lambda(,(car spec)) ,@body)
	   ,(cadr spec))
     ,(caddr spec)))

(defun sgn(x)
  (cond 
   ((> x 0) 1)
   ((< x 0) -1)
   (t 0)))

(defgroup animate-move "Animate move"
  :group move)

(defcustom animate-move-mode 'exp
  "The method of animated moving windows"
  :type (set exp lin)
  :group (move animate-move))

(defcustom animate-move-delay 10
  "Animations rate in milliseconds"
  :type number
  :range (0 . nil)
  :group (move animate-move))

(defvar animate-move-timer nil)

(defun animate-move-step-exp-to (w tx ty)
  (let ((x (car (window-position w)))
	(y (cdr (window-position w))))	
    (setq x (+ x (/ (- tx x) 2)))
    (setq y (+ y (/ (- ty y) 2)))
    (move-window-to w (round x) (round y))))

(defun animate-move-step-lin-to (w tx ty)
  (let ((x (car (window-position w)))
	(y (cdr (window-position w))))	
    (setq x (+ x (sgn (- tx x))))
    (setq y (+ y (sgn (- ty y))))
    (move-window-to w (round x) (round y))))

(defun animate-move-timer-handler ()
  (let ((nanimate 0))
    (dolist (i (managed-windows))
      (when (window-get i 'animate-move)
	(setq nanimate (+ nanimate 1))
	(let ((x (car (window-position i)))
	      (y (cdr (window-position i)))
	      (nx (window-get i 'animate-move-x))
	      (ny (window-get i 'animate-move-y)))
          (if (and (eq x nx) (eq y ny))
              (window-put i 'animate-move nil)
            (cond 
	     ((eq animate-move-mode 'lin) 
	      (animate-move-step-lin-to i nx ny))
	     (t 
	      (animate-move-step-exp-to i nx ny)))))))
    (if (> nanimate 0)
	(set-timer animate-move-timer)
      (progn 
	(delete-timer animate-move-timer)
	(setq animate-move-timer nil)))))

(defun animate-move-window-to (w x y) 
  (interactive "%W")
  (window-put w 'animate-move t)
  (when (/= 0 (mod x 2))
    (setq x (+ x 1)))
  (when (/= 0 (mod y 2))
    (setq y (+ y 1)))
  (window-put w 'animate-move-x (round x))
  (window-put w 'animate-move-y (round y))
  (unless animate-move-timer 
    (setq animate-move-timer (make-timer animate-move-timer-handler
                                         (quotient animate-move-delay 1000)
                                         (mod animate-move-delay 1000)))))

(defun next-position (x) 
  (cond 
   ((equal x 'center) 'left-top)
   ((equal x 'left-top) 'right-top)
   ((equal x 'right-top) 'right-bottom)
   ((equal x 'right-bottom) 'left-bottom)
   (t 'left-top)))

(defun get-position-by-name (p w)
  (let ((wx (car (window-frame-dimensions w)))
	(wy (cdr (window-frame-dimensions w))))
    (cond 
     ((equal p 'left-top) (cons 0 0))
     ((equal p 'right-top) (cons (- (screen-width) wx)   0))
     ((equal p 'right-bottom)  (cons (- (screen-width) wx)
				     (- (screen-height) wy)))
     ((equal p 'left-bottom) (cons 0 (- (screen-height)  wy)))
     (t (cons (- (/ (screen-width) 2) (/ wx 2))
	      (- (/ (screen-height) 2) (/ wy 2)))))))

(defun rotate-move (w)
  (interactive "%W")
  (let ((prop (window-get w 'last-pos))
	(pos (cons 0 0)))
    (setq prop (next-position prop))
    (window-put w 'last-pos prop)
    (setq pos (get-position-by-name prop w))
    (animate-move-window-to w (car pos) (cdr pos))))

(defun center-window (w)
  (interactive "%W")
  (let ((nx (- (/ (screen-width) 2) (/ (car (window-dimensions w)) 2)))
	(ny (- (/ (screen-height) 2) (/ (cdr (window-dimensions w)) 2))))
    (move-window-to w nx ny)))

(defun animate-center-window (w)
  (interactive "%W")
  (let* ((pos (get-position-by-name 'center w))
	 (nx (car pos))
	 (ny (cdr pos)))
    (animate-move-window-to w nx ny)))
