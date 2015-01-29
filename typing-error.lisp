;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Frank Tamborello
;;; Copyright   : (c) 2014 Cogscent, LLC
;;; Availability: GNU LGPL, see LGPL.txt
;;; Address     : Cogscent, LLC
;;;		: http://cogscent.com/
;;;		: frank.tamborello@cogscent.com
;;;		: 1010 25th St NW, Ste 701
;;;		: Washington DC, USA 20037
;;;
;;; Disclaimer	:     This library is free software; you can redistribute it and/or
;;;		: modify it under the terms of the GNU Lesser General Public
;;;		: License as published by the Free Software Foundation; either
;;;		: version 2.1 of the License, or (at your option) any later version.
;;;		:     This library is distributed in the hope that it will be useful,
;;;		: but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;		: MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;		: Lesser General Public License for more details.
;;;		:     You should have received a copy of the GNU Lesser General Public
;;;		: License along with this library; if not, write to the Free Software
;;;		: Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;		: 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : typing-error.lisp
;;; Revision    : 5
;;; 
;;; Description : This code extends ACT-R 6 (r1674) to implement errorful typing.
;;;		
;;;
;;; Usage	: Place in ACT-R folder "User Loads." This file will load
;;;		automatically after ACT-R loads.
;;; 
;;; Bugs        : None known
;;;
;;; To do       : 
;;; 
;;;
;;;
;;; Issues	: 
;;; 1. The noise function from May et al (2008) doesn't seem quite right for
;;; typing. Like it doesn't seem right for the model to aim at "8" and instead 
;;; hit ";".
;;;
;;; 
;;; ----- History -----
;;; 2014.11.19 fpt 5
;;; 1. I added non-alphanumeric characters and capital letters to ACT-R's
;;; typing vocabulary.
;;;
;;;
;;; 2014.11.18 fpt 4
;;; I obviated virtual-experiment-window.
;;;
;;;
;;; 2014.11.18 fpt 3
;;; 1. I dialed up the noise a bit by dividing 1.5, rather than 1, by 4.133,
;;; for both noise terms.
;;;
;;; 2. The noisy typist isn't constrained to specifying move-to locations that are
;;; within ACT-R's keyboard. When it specifies an out-of-bound location, ACT-R 
;;; crashes. I need to ensure those move-to locations stay in-bounds.
;;; To do so I redefined polar-move-xy to stay inbounds. Although in retrospect
;;; that was a mistake because ACT-R uses polar-move-xy to move the mouse, too!
;;; A better way may be to instead redefine peck-recoil's queue-output-events.
;;; Yup, that seems much better.
;;;
;;; 3. There was another, related bug that occured whenever typing-error's noise 
;;; was so negative as to result in a negative radius being passed to fitts d
;;; parameter. Now typing-error ensures radius is at least 0.
;;; OTOH, how often do people miss the keyboard when typing and have to try again?
;;; It seems plausible, but very uncommon, to the point where it might not be worth
;;; considering.
;;;
;;;
;;; 2014.11.16 fpt 2
;;;  Hold-key pushes a key onto *held-keys*.
;;;  Device-handle-keypress prepends all the *held-keys* onto key.
;;;
;;;
;;; 2014.11.11 fpt 1
;;; Inception: Redefine key->cmd to add noise to the radius and theta components
;;; of the hfrt movement style commands to which it translates ACT-R's 
;;; press-key commands. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(unless
    (member ':act-r-6.0 *features*)
  (load "/Users/frank/Documents/ACT-R-current/actr6/load-act-r-6.lisp"))

(defvar *held-keys* nil "Keys currently held down by the ACT-R model.")


(defmethod populate-loc-to-non-alph-key-array ((ar array))
  "Sets all the keys in the array that need to be set"
  ;; function key row
  (setf (aref ar 0 0) 'ESC)
  (setf (aref ar 2 0) 'f1)
  (setf (aref ar 3 0) 'f2)
  (setf (aref ar 4 0) 'f3)
  (setf (aref ar 5 0) 'f4)
  (setf (aref ar 7 0) 'f5)
  (setf (aref ar 8 0) 'f6)
  (setf (aref ar 9 0) 'f7)
  (setf (aref ar 10 0) 'f8)
  (setf (aref ar 12 0) 'f9)
  (setf (aref ar 13 0) 'f10)
  (setf (aref ar 14 0) 'f11)
  (setf (aref ar 15 0) 'f12)
  (setf (aref ar 17 0) 'print-screen)
  (setf (aref ar 18 0) 'scroll-lock)
  (setf (aref ar 19 0) 'pause)
  ;; numeric key row
  (setf (aref ar 0 2) #\~)
  (setf (aref ar 1 2) #\!)
  (setf (aref ar 2 2) #\@)
  (setf (aref ar 3 2) #\#)
  (setf (aref ar 4 2) #\$)
  (setf (aref ar 5 2) #\%)
  (setf (aref ar 6 2) #\^)
  (setf (aref ar 7 2) #\&)
  (setf (aref ar 8 2) #\*)
  (setf (aref ar 9 2) #\()
  (setf (aref ar 10 2) #\))
  (setf (aref ar 11 2) #\_)
  (setf (aref ar 12 2) #\+)
  (setf (aref ar 13 2) 'Delete)
  (setf (aref ar 15 2) 'help)
  (setf (aref ar 16 2) 'home)
  (setf (aref ar 17 2) 'pageup)
  (setf (aref ar 19 2) 'ESC)
  (setf (aref ar 20 2) #\=)
  (setf (aref ar 21 2) #\/)
  (setf (aref ar 22 2) #\*)
  ;; qwerty row
  (setf (aref ar 0 3) #\Tab)
  (setf (aref ar 1 3) #\Q)
  (setf (aref ar 2 3) #\W)
  (setf (aref ar 3 3) #\E)
  (setf (aref ar 4 3) #\R)
  (setf (aref ar 5 3) #\T)
  (setf (aref ar 6 3) #\Y)
  (setf (aref ar 7 3) #\U)
  (setf (aref ar 8 3) #\I)
  (setf (aref ar 9 3) #\O)
  (setf (aref ar 10 3) #\P)
  (setf (aref ar 11 3) #\{)
  (setf (aref ar 12 3) #\})
  (setf (aref ar 13 3) #\|)
  (setf (aref ar 15 3) 'DEL)
  (setf (aref ar 16 3) 'End)
  (setf (aref ar 17 3) 'Page)
  (setf (aref ar 19 3) #\7)
  (setf (aref ar 20 3) #\8)
  (setf (aref ar 21 3) #\9)
  (setf (aref ar 22 3) #\-)
  ;; ASDF row
  (setf (aref ar 0 4) 'caps-lock)
  (setf (aref ar 1 4) #\A)
  (setf (aref ar 2 4) #\S)
  (setf (aref ar 3 4) #\D)
  (setf (aref ar 4 4) #\F)
  (setf (aref ar 5 4) #\G)
  (setf (aref ar 6 4) #\H)
  (setf (aref ar 7 4) #\J)
  (setf (aref ar 8 4) #\K)
  (setf (aref ar 9 4) #\L)
  (setf (aref ar 10 4) #\:)
  (setf (aref ar 11 4) #\")
  (setf (aref ar 12 4) #\Newline)
  (setf (aref ar 13 4) #\Newline)
  (setf (aref ar 19 4) #\4)
  (setf (aref ar 20 4) #\5)
  (setf (aref ar 21 4) #\6)
  (setf (aref ar 22 4) #\+)
  ;; Z row
  (setf (aref ar 0 5) 'shift)
  (setf (aref ar 1 5) #\Z)
  (setf (aref ar 2 5) #\X)
  (setf (aref ar 3 5) #\C)
  (setf (aref ar 4 5) #\V)
  (setf (aref ar 5 5) #\B)
  (setf (aref ar 6 5) #\N)
  (setf (aref ar 7 5) #\M)
  (setf (aref ar 8 5) #\<)
  (setf (aref ar 9 5) #\>)
  (setf (aref ar 10 5) #\?)
  (setf (aref ar 11 5) 'shift)
  (setf (aref ar 12 5) 'shift)
  (setf (aref ar 16 5) 'UpArrow)
  (setf (aref ar 19 5) #\1)
  (setf (aref ar 20 5) #\2)
  (setf (aref ar 21 5) #\3)
  (setf (aref ar 22 5) 'enter)
  ;; space bar row
  (setf (aref ar 0 6) 'control)
  (setf (aref ar 1 6) 'option)
  (setf (aref ar 2 6) 'command)
  (setf (aref ar 3 6) #\Space)
  (setf (aref ar 4 6) #\Space)
  (setf (aref ar 5 6) #\Space)
  (setf (aref ar 6 6) #\Space)
  (setf (aref ar 7 6) #\Space)
  (setf (aref ar 8 6) #\Space)
  (setf (aref ar 9 6) #\Space)
  (setf (aref ar 10 6) #\Space)
  (setf (aref ar 11 6) 'command)
  (setf (aref ar 12 6) 'option)
  (setf (aref ar 13 6) 'control)
  (setf (aref ar 15 6) 'BackArrow)
  (setf (aref ar 16 6) 'DownArrow)
  (setf (aref ar 17 6) 'ForwardArrow)
  (setf (aref ar 19 6) #\0)
  (setf (aref ar 20 6) #\0)
  (setf (aref ar 21 6) #\.)
  (setf (aref ar 22 6) 'enter)
  ar)

(defclass virtual-keyboard ()
   ((key->cmd-ht :accessor key->cmd-ht 
                  :initform (make-hash-table :test #'equal))
    (key->loc-ht :accessor key->loc-ht
                  :initform (make-hash-table :test #'equal))
    (loc->key-array :accessor loc->key-arr 
                     :initform (make-array '(23 7) :initial-element nil))
    (loc->non-alph-key-array :accessor loc->non-alph-key-arr 
                     :initform (make-array '(23 7) :initial-element nil))))

(defmethod initialize-instance :after ((vk virtual-keyboard) &key)
  (populate-key-to-command-ht (key->cmd-ht vk))
  (populate-key-to-loc-ht (key->loc-ht vk))
  (populate-loc-to-key-array (loc->key-arr vk))
  (populate-loc-to-non-alph-key-array (loc->non-alph-key-arr vk)))

(defmethod loc->key ((vk virtual-keyboard) (loc vector))
  (cond
   ((vpt= loc #(28 2)) 'mouse)
   ((or
     (member 'shift *held-keys*)
     (member 'left-shift *held-keys*)
     (member 'right-shifrt *held-keys*))
    ;; another array in the keyboard for the non-alphanumeric keys
    (aref (loc->non-alph-key-arr vk) (px loc) (py loc))) 
   (t (aref (loc->key-arr vk) (px loc) (py loc)))))

(setf (keyboard (current-device-interface))
      (make-instance 'virtual-keyboard))


(defclass exp-wind ()
  ())

(defmethod build-vis-locs-for ((device exp-wind) vismod)
  (declare (ignore vismod)))

(defmethod vis-loc-to-obj ((device exp-wind) vl)
  "Returns the vis-obj of the widget containing the vis-loc."
  (declare (ignore vl)))

(defmethod device-move-cursor-to ((device exp-wind) loc) 
  (declare (ignore loc)))

(defmethod get-mouse-coordinates ((device exp-wind))
  #(0 0))

(defmethod device-handle-click ((device exp-wind))
  nil)

(defmethod device-handle-keypress ((device exp-wind) key)
  (model-output "Model pressed ~a key with ~{~a~^, ~} modifiers." key *held-keys*)
  (release-key (get-module :motor) 'shift))

(install-device (make-instance 'exp-wind))

(define-model noisy-typer)



(defun typing-error (radius theta)
  (let ((on-noise (act-r-noise (*  (/ 1.5 4.133) (/ (sqrt 3) pi)))) 
        (off-noise (act-r-noise (* 0.75 (/ 1.5 4.133) (/ (sqrt 3) pi)))))
    (list 
     (+ radius (max 0 on-noise))
     (+ theta off-noise))))

(defmethod queue-output-events ((mtr-mod motor-module) (self peck-recoil))
  (let ((new-xy (polar-move-xy (finger-loc-m mtr-mod (hand self) (finger self))
                               (vector (r self) (theta self)))))
    ;; Finger bounds are 0:22 & 0:6
    (setf (svref new-xy 0) (max 0 (min 22 (svref new-xy 0))))
    (setf (svref new-xy 1) (max 0 (min 6 (svref new-xy 1))))
    
    (schedule-event-relative (exec-time self) 'output-key :destination :device :module :motor :output 'medium
                             :params (list new-xy))))


(defmethod key->cmd ((devin device-interface) key)
  (destructuring-bind 
      (cmd h0 h1 f0 f1 r0 r1 t0 t1)
      (key-to-command (keyboard devin) key)
    (destructuring-bind
        (rad the)
        (typing-error r1 t1)
      (list cmd h0 h1 f0 f1 r0 rad t0 the))))


;;; ACT-R needs to perform key chords like people do, e.g. hold shift while 
;;; pressing another key.
;;; Hold Key
(defgeneric hold-key (mtr-mod key)
  (:documentation "Like press-key, except ACT-R holds the key until release-key is called."))

(defmethod hold-key ((mtr-mod motor-module) key)
  (ecase key
    (shift '(peck :hand left :finger pinkie :r 1.41 :theta 2.36))
    (left-shift '(peck :hand left :finger pinkie :r 1.41 :theta 2.36))
    (left-control '(peck :hand left :finger pinkie :r 2.24 :theta 2.03))
    (left-option '(peck :hand left :finger pinkie :r 2.0 :theta 1.57))
    (left-command '(peck :hand left :finger ring :r 2.0 :theta 1.57))
    (right-shift '(peck :hand right :finger pinkie :r 1.41 :theta 0.78))
    (right-control '(peck :hand right :finger pinkie :r 3.6 :theta 0.59))
    (right-option '(peck :hand right :finger pinkie :r 2.83 :theta 0.78))
    (right-command '(peck :hand right :finger pinkie :r 2.24 :theta 1.11)))
  (push key *held-keys*))
  

;;; Release Key
(defgeneric release-key (mtr-mod key)
  (:documentation "Releases a key already held by hold-key."))

(defmethod release-key ((mtr-mod motor-module) key)
  (setf *held-keys* (delete key *held-keys*)))

(extend-manual-requests-fct '((hold-key (:include press-key))) 'hold-key)
(extend-manual-requests-fct '((release-key (:include press-key))) 'release-key)

;; Press some keys
(progn
  (clear-all)
  (define-model noisy-typer)
  (install-device (make-instance 'exp-wind))
  (dotimes (i 20)
    (hold-key (get-module :motor) 'shift)
    (press-key (get-module :motor) 'u)
    (run-until-condition #'(lambda () nil))))




