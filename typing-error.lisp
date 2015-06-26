;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Frank Tamborello
;;; Copyright   : (c)2015 Frank Tamborello, All Rights Reserved
;;; Availability: public domain
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public
;;; License: the GNU Lesser General Public License as published by the
;;; Free Software Foundation (either version 2.1 of the License, 
;;; or, at your option, any later version),
;;; and the Franz, Inc Lisp-specific preamble.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the Lisp Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;; and see Franz, Inc.'s preamble to the GNU Lesser General Public License,
;;; http://opensource.franz.com/preamble.html.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : typing-error.lisp
;;; Revision    : 12
;;; 
;;; Description : This code extends ACT-R 6.1 (r1754) to implement errorful typing.
;;;		
;;;
;;; Usage	: Place in ACT-R folder "User Loads." This file will load
;;;		automatically after ACT-R loads.
;;; 
;;; Bugs        : None known
;;;
;;; To do       : 
;;; 1. Using *held-keys* to hold a stack of held keys is problematic, especially since
;;; it doesn't get reset with the model! Each modifier key really must have its own
;;; dedicated, say, slot of some object, like, say, of the virtual-keyboard.
;;;
;;; 2. Maybe transposition errors are a result of sequence neighbors being pipelined
;;; into the motor system, and because of stochastic completion times for each stage,
;;; occassionally the latter finishes first. Do people mostly transpose across hands?
;;;
;;; 3. Sometimes people fail to punch a key not because they miss in the X or Y
;;; dimensions, but because they fail to exert sufficient force in the Z dimension.
;;; How might I implement that?
;;;
;;; 4. Sometimes people mistype because their entire hand is erroneously transposed
;;; one or two keys off from its homerow location, especially if, say, they just moved
;;; that hand back to the keyboard from the pointing device.
;;;
;;;
;;; Theoretical Issues	: 
;;; 1. Models sometimes miss the keyboard entirely! Albeit that's less so 
;;; since revision 7. That seems bizarre for normal typists, but is that really
;;; a bug or a feature? Maybe there are theoretical reasons for keeping some
;;; ability to completely miss the keyboard, like modeling typing of 
;;; neuromuscular degeneration patients or people who are extremely fatigued.
;;; For now, since the misses are only by 1 key's width or less when the intended
;;; target is one of the outside keys, we'll keep it.
;;;
;;; 2. F13-15 may actually require a move-hand, not just a peck-recoil. I dunno,
;;; I can hit those keys just by sort of rotating my right hand at the wrist and 
;;; my hands are not especially big, but others are surely smaller.
;;;
;;;
;;; Practical Issues :
;;; 1. Maybe, instead of pushing symbols like 'shift onto one list, it'd be
;;; better to have a held-key-state object with slots for each modifier key
;;; indicating whether or not that key was being held. Furthermore, methods
;;; could handle goofy things like holding either left- or right-shift means
;;; that shift is currently in effect, since having a copy of the same key 
;;; on each side of the keyboard means I need to decouple the motor movement
;;; from the key state. Maybe it's not really a problem? Maybe we should try
;;; using it more before I expend the effort?
;;;
;;;
;;; 
;;; ----- History -----
;;; 2015.03.19	fpt	12
;;; 1. Squashed bug: My destructuring bind in key->cmd doesn't work for keys on the
;;; home row because the cmd ht doesn't return a peck-recoil command, it returns a 
;;; punch command. Now key->cmd first checks to see if the command is a punch: 
;;; then it simply returns that command, else it destructure-binds the command list
;;; so that the radius and theta can have noise added.
;;; 
;;;
;;; 2015.03.18	fpt	11
;;; I added letters to populate-modded-key-to-command-ht so that when, say, shift
;;; is in *held-keys*, the model can type a capital letter.
;;;
;;;
;;; 2015.03.07 fpt 10
;;; 1. I turned the noise down, both on- and off-axis, to / 1 4.133.
;;;
;;; 2. Bug squashed: My hack to enable typing of unmodded keypresses when no mod 
;;; key is held and some modded keypress is intended seems to have fallen short: 
;;; even when a mod key is held, the unmodded character gets typed! My solution
;;; was to create another key->command hashtable for modded keypresses and to
;;; check for *held-keys* in key->cmd to dispatch to the appropriate hash table.
;;;
;;; 3. Bug squashed: 
#|
     +manual>
        cmd hold-key
        key shift
==>
> Error: The value #S(ACT-R-CHUNK-SPEC :FILLED-SLOTS 2181038080 :EMPTY-SLOTS 0 :REQUEST-PARAM-SLOTS 0 :DUPLICATE-SLOTS 0 :EQUAL-SLOTS 2181038080 :NEGATED-SLOTS 0 :RELATIVE-SLOTS 0 :VARIABLES NIL :SLOT-VARS NIL :DEPENDENCIES ...) is not of the expected type (MEMBER SHIFT LEFT-SHIFT LEFT-CONTROL LEFT-OPTION LEFT-COMMAND RIGHT-SHIFT RIGHT-CONTROL RIGHT-OPTION RIGHT-COMMAND).
> While executing: #<STANDARD-METHOD HOLD-KEY (MOTOR-MODULE T)>, in process Listener(4).

I suspect release-key will likewise fail.
|#
;;; Yes, the issue was a rather silly mistake: I forgot that the hold-key
;;; and release-key motor module request extensions must work with chunk-specs,
;;; not keys.
;;;
;;; 2015.03.05 fpt 9
;;; 1. Added non-alphanumeric characters to the key-to-command hashtable.
;;; This is important so that when the model fails to hold a modifier key,
;;; like shift, the appropriate alphanumeric character for that key should
;;; get typed.
;;;
;;; 2. It turns out that if you want to remember a sequence including numerals,
;;; that ACT-R doesn't like the name of a chunk to be a numeral. So it helps 
;;; to work around that by adding English names of numerals to the key-to-
;;; command hashtable.
;;;
;;; 2015.02.17 fpt 8
;;; 1. Models can now request press-key for the top row: escape & function 
;;; keys 1-15 because I filled the rest of the populate-key-to-command-ht
;;; hashtable for those keys.
;;;
;;; 2015.02.17 fpt 7
;;; 1. Changed typing-error's on-axis-noise to return the max of 0 or
;;; (+ radius on-noise), where on-noise is also constrained to +-1. Result:
;;; model misses its intended key by at most one key.
;;;
;;; 2015.02.17 fpt 6
;;; 1. Squashed some code execution order bugs.
;;; 2. Upgraded development to ACT-R 6.1, although I think this code should
;;; still work with ACT-R 6.0.
;;;
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


;; for development, just loads ACT-R automatically
(unless
    (member ':act-r-6.1 *features*)
  (load "/Users/frank/Documents/ACT-R-current/actr6.1/load-act-r-6.lisp"))

(defvar *held-keys* nil "Keys currently held down by the ACT-R model.")

;; I must redefine populate-key-to-command-ht here to include the top row
;; because ACT-R's lacks that
(defmethod populate-key-to-command-ht ((ht hash-table))
  (setf (gethash 'space ht) '(punch :hand left :finger thumb))
  (setf (gethash 'backquote ht) 
    '(peck-recoil :hand left :finger pinkie :r 2.24 :theta -2.03))
  (setf (gethash 'tab ht) 
    '(peck-recoil :hand left :finger pinkie :r 1.41 :theta -2.36))
  (setf (gethash '1 ht) 
    '(peck-recoil :hand left :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'one ht) 
    '(peck-recoil :hand left :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'Q ht) 
    '(peck-recoil :hand left :finger pinkie :r 1 :theta -1.57))
  (setf (gethash 'A ht) '(punch :hand left :finger pinkie))
  (setf (gethash 'Z ht) 
    '(peck-recoil :hand left :finger pinkie :r 1 :theta 1.57))
  (setf (gethash '2 ht) 
    '(peck-recoil :hand left :finger ring :r 2 :theta -1.57))
  (setf (gethash 'two ht) 
        '(peck-recoil :hand left :finger ring :r 2 :theta -1.57))
  (setf (gethash 'W ht) 
    '(peck-recoil :hand left :finger ring :r 1 :theta -1.57))
  (setf (gethash 'S ht) 
    '(punch :hand left :finger ring))
  (setf (gethash 'X ht) 
    '(peck-recoil :hand left :finger ring :r 1 :theta 1.57))
  (setf (gethash '3 ht) 
    '(peck-recoil :hand left :finger middle :r 2 :theta -1.57))
  (setf (gethash 'three ht) 
    '(peck-recoil :hand left :finger middle :r 2 :theta -1.57))
  (setf (gethash 'E ht) 
    '(peck-recoil :hand left :finger middle :r 1 :theta -1.57))
  (setf (gethash 'D ht) '(punch :hand left :finger middle))
  (setf (gethash 'C ht) 
    '(peck-recoil :hand left :finger middle :r 1 :theta 1.57))
  (setf (gethash '4 ht) 
    '(peck-recoil :hand left :finger index :r 2 :theta -1.57))
  (setf (gethash 'four ht) 
    '(peck-recoil :hand left :finger index :r 2 :theta -1.57))
  (setf (gethash 'R ht) 
    '(peck-recoil :hand left :finger index :r 1 :theta -1.57))
  (setf (gethash 'F ht) '(punch :hand left :finger index))
  (setf (gethash 'V ht) 
    '(peck-recoil :hand left :finger index :r 1 :theta 1.57))
  (setf (gethash '5 ht) 
    '(peck-recoil :hand left :finger index :r 2.24 :theta -1.11))
  (setf (gethash 'five ht) 
    '(peck-recoil :hand left :finger index :r 2.24 :theta -1.11))
  (setf (gethash 'T ht) 
    '(peck-recoil :hand left :finger index :r 1.41 :theta -0.79))
  (setf (gethash 'G ht) 
    '(peck-recoil :hand left :finger index :r 1 :theta 0))
  (setf (gethash 'B ht) 
    '(peck-recoil :hand left :finger index :r 1.41 :theta 0.79))
  (setf (gethash '6 ht) 
    '(peck-recoil :hand right :finger index :r 2.24 :theta -2.03))
  (setf (gethash 'six ht) 
    '(peck-recoil :hand right :finger index :r 2.24 :theta -2.03))
  (setf (gethash 'Y ht) 
    '(peck-recoil :hand right :finger index :r 1.41 :theta -2.36))
  (setf (gethash 'H ht) 
    '(peck-recoil :hand right :finger index :r 1 :theta 3.14))
  (setf (gethash 'N ht) 
    '(peck-recoil :hand right :finger index :r 1.41 :theta 2.36))
  (setf (gethash '7 ht) 
    '(peck-recoil :hand right :finger index :r 2 :theta -1.57))
  (setf (gethash 'seven ht) 
    '(peck-recoil :hand right :finger index :r 2 :theta -1.57))
  (setf (gethash 'U ht) 
    '(peck-recoil :hand right :finger index :r 1 :theta -1.57))
  (setf (gethash 'J ht) 
    '(punch :hand right :finger index))
  (setf (gethash 'M ht) 
    '(peck-recoil :hand right :finger index :r 1 :theta 1.57))
  (setf (gethash '8 ht) 
    '(peck-recoil :hand right :finger middle :r 2 :theta -1.57))
  (setf (gethash 'eight ht) 
    '(peck-recoil :hand right :finger middle :r 2 :theta -1.57))
  (setf (gethash 'I ht) 
    '(peck-recoil :hand right :finger middle :r 1 :theta -1.57))
  (setf (gethash 'K ht) '(punch :hand right :finger middle))
  (setf (gethash 'comma ht) 
    '(peck-recoil :hand right :finger middle :r 1 :theta 1.57))
  (setf (gethash '9 ht) 
    '(peck-recoil :hand right :finger ring :r 2 :theta -1.57))
  (setf (gethash 'nine ht) 
    '(peck-recoil :hand right :finger ring :r 2 :theta -1.57))
  (setf (gethash 'O ht) 
    '(peck-recoil :hand right :finger ring :r 1 :theta -1.57))
  (setf (gethash 'L ht) 
    '(punch :hand right :finger ring))
  (setf (gethash 'period ht) 
    '(peck-recoil :hand right :finger ring :r 1 :theta 1.57))
  (setf (gethash 'dot ht) 
    '(peck-recoil :hand right :finger ring :r 1 :theta 1.57))
  (setf (gethash '0 ht) 
    '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'zero ht) 
    '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'P ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta -1.57))
  (setf (gethash 'semicolon ht) '(punch :hand right :finger pinkie))
  (setf (gethash 'slash ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta 1.57))
  (setf (gethash '/ ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta 1.57))
  (setf (gethash 'hyphen ht) 
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -1.11))
  (setf (gethash '- ht) 
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -1.11))
  (setf (gethash '[ ht) 
    '(peck-recoil :hand right :finger pinkie :r 1.41 :theta -0.78))
  (setf (gethash 'quote ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta 0))
  (setf (gethash 'return ht) 
    '(peck-recoil :hand right :finger pinkie :r 2 :theta 0))
  
  ;; some of these are stretching the pinkies to fill in the primary area...
  (setf (gethash 'backslash ht)
    '(peck-recoil :hand right :finger pinkie :r 3.16 :theta -0.32))
  (setf (gethash '= ht)
    '(peck-recoil :hand right :finger pinkie :r 2.83 :theta -0.78))
  (setf (gethash '] ht)
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -0.46))
  (setf (gethash 'delete ht)
    '(peck-recoil :hand right :finger pinkie :r 3.6 :theta -0.59))
  (setf (gethash 'right-shift ht)
    '(peck-recoil :hand right :finger pinkie :r 1.41 :theta 0.78))
  (setf (gethash 'right-control ht)
    '(peck-recoil :hand right :finger pinkie :r 3.6 :theta 0.59))
  (setf (gethash 'right-option ht)
    '(peck-recoil :hand right :finger pinkie :r 2.83 :theta 0.78))
  (setf (gethash 'right-command ht)
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta 1.11))
  
  (setf (gethash 'shift ht)
    '(peck-recoil :hand left :finger pinkie :r 1.41 :theta 2.36))
  (setf (gethash 'left-shift ht)
    '(peck-recoil :hand left :finger pinkie :r 1.41 :theta 2.36))
  (setf (gethash 'left-control ht)
    '(peck-recoil :hand left :finger pinkie :r 2.24 :theta 2.03))
  (setf (gethash 'left-option ht)
    '(peck-recoil :hand left :finger pinkie :r 2.0 :theta 1.57))
  (setf (gethash 'left-command ht)
    '(peck-recoil :hand left :finger ring :r 2.0 :theta 1.57))
  (setf (gethash 'caps-lock ht)
    '(peck-recoil :hand left :finger pinkie :r 1.0 :theta 3.14))

;; the top row: escape & function keys 1-15
  (setf (gethash 'esc ht)
      '(peck-recoil :hand left :finger pinkie :r 4.1231055 :theta -1.8157749493890485D0))
  (setf (gethash 'f1 ht)
      '(peck-recoil :hand left :finger pinkie :r 4.1231055 :theta -1.3258177))
  (setf (gethash 'f2 ht)
      '(peck-recoil :hand left :finger ring :r 4.1231055 :theta -1.3258177))
  (setf (gethash 'f3 ht)
      '(peck-recoil :hand left :finger middle :r 4.1231055 :theta -1.3258177))
  (setf (gethash 'f4 ht)
      '(peck-recoil :hand left :finger index :r 4.1231055 :theta -1.3258177))
  (setf (gethash 'f5 ht)
      '(peck-recoil :hand right :finger index :r 4 :theta -1.5707963267948966D0))
  (setf (gethash 'f6 ht)
      '(peck-recoil :hand right :finger middle :r 4 :theta -1.5707963267948966D0))
  (setf (gethash 'f7 ht)
      '(peck-recoil :hand right :finger ring :r 4 :theta -1.5707963267948966D0))
  (setf (gethash 'f8 ht)
      '(peck-recoil :hand right :finger pinkie :r 4 :theta -1.5707963267948966D0))
  (setf (gethash 'f9 ht)
      '(peck-recoil :hand right :finger index :r 6.4031243 :theta -0.67474097))
  (setf (gethash 'f10 ht)
      '(peck-recoil :hand right :finger middle :r 6.4031243 :theta -0.67474097))
  (setf (gethash 'f11 ht)
      '(peck-recoil :hand right :finger ring :r 6.4031243 :theta -0.67474097))
  (setf (gethash 'f12 ht)
      '(peck-recoil :hand right :finger pinkie :r 6.4031243 :theta -0.67474097))
  (setf (gethash 'f13 ht)
      '(peck-recoil :hand right :finger index :r 10.770329 :theta -0.38050637))
  (setf (gethash 'f14 ht)
      '(peck-recoil :hand right :finger middle :r 10.770329 :theta -0.38050637))
  (setf (gethash 'f15 ht)
      '(peck-recoil :hand right :finger ring :r 10.770329 :theta -0.38050637))
      
  ht)


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

;; I need these so models can mistakenly type the appropriate unmodded key to go with
;; the intended nonalphanumeric character.
(defgeneric populate-nonalpha-char-to-command-ht (ht)
  (:documentation "Populates the hash table that maps non-alphanumeric characters to
  motor commands."))

(defmethod populate-nonalpha-char-to-command-ht ((ht hash-table))
  (setf (gethash '~ ht) 
    '(peck-recoil :hand left :finger pinkie :r 2.24 :theta -2.03))
  (setf (gethash 'tilde ht) 
    '(peck-recoil :hand left :finger pinkie :r 2.24 :theta -2.03))
  (setf (gethash '! ht) 
    '(peck-recoil :hand left :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'bang ht) 
        '(peck-recoil :hand left :finger pinkie :r 2 :theta -1.57))
  (setf (gethash '@ ht) 
    '(peck-recoil :hand left :finger ring :r 2 :theta -1.57))
  (setf (gethash 'at ht) 
        '(peck-recoil :hand left :finger ring :r 2 :theta -1.57))
  (setf (gethash '\# ht)
    '(peck-recoil :hand left :finger middle :r 2 :theta -1.57))
  (setf (gethash 'hash ht)
        '(peck-recoil :hand left :finger middle :r 2 :theta -1.57))
  (setf (gethash '$ ht) 
    '(peck-recoil :hand left :finger index :r 2 :theta -1.57))
  (setf (gethash 'dollar ht) 
        '(peck-recoil :hand left :finger index :r 2 :theta -1.57))
  (setf (gethash '% ht) 
    '(peck-recoil :hand left :finger index :r 2.24 :theta -1.11))
  (setf (gethash 'percent ht) 
        '(peck-recoil :hand left :finger index :r 2.24 :theta -1.11))
  (setf (gethash '^ ht) 
    '(peck-recoil :hand right :finger index :r 2.24 :theta -2.03))
  (setf (gethash 'carrot ht) 
        '(peck-recoil :hand right :finger index :r 2.24 :theta -2.03))
  (setf (gethash '& ht) 
    '(peck-recoil :hand right :finger index :r 2 :theta -1.57))
  (setf (gethash 'ampersand ht) 
        '(peck-recoil :hand right :finger index :r 2 :theta -1.57))
  (setf (gethash '* ht) 
    '(peck-recoil :hand right :finger middle :r 2 :theta -1.57))
  (setf (gethash 'asterisk ht) 
        '(peck-recoil :hand right :finger middle :r 2 :theta -1.57))
  (setf (gethash '\( ht) 
    '(peck-recoil :hand right :finger ring :r 2 :theta -1.57))
  (setf (gethash 'open-parenthesis ht) 
        '(peck-recoil :hand right :finger ring :r 2 :theta -1.57))
  (setf (gethash '> ht) 
    '(peck-recoil :hand right :finger ring :r 1 :theta 1.57))
  (setf (gethash 'close-angle-brace ht) 
        '(peck-recoil :hand right :finger ring :r 1 :theta 1.57))
  (setf (gethash '< ht) 
    '(peck-recoil :hand right :finger middle :r 1 :theta 1.57))
  (setf (gethash 'open-angle-brace ht) 
        '(peck-recoil :hand right :finger middle :r 1 :theta 1.57))
  (setf (gethash '\) ht) 
    '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'close-parenthesis ht) 
    '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'backslash ht) 
        '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'semicolon ht) '(punch :hand right :finger pinkie))
  (setf (gethash '? ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta 1.57))
  (setf (gethash 'question-mark ht)
      '(peck-recoil :hand right :finger pinkie :r 1 :theta 1.57))
  (setf (gethash '_ ht) 
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -1.11))
  (setf (gethash 'under-score ht) 
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -1.11))
  (setf (gethash '{ ht) 
    '(peck-recoil :hand right :finger pinkie :r 1.41 :theta -0.78))
  (setf (gethash 'open-curly-brace ht) 
    '(peck-recoil :hand right :finger pinkie :r 1.41 :theta -0.78))
  (setf (gethash '\" ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta 0))
  (setf (gethash 'double-quote ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta 0))
  (setf (gethash '+ ht)
    '(peck-recoil :hand right :finger pinkie :r 2.83 :theta -0.78))
  (setf (gethash 'plus ht)
    '(peck-recoil :hand right :finger pinkie :r 2.83 :theta -0.78))
  (setf (gethash '} ht)
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -0.46))
  (setf (gethash 'close-curly-brace ht)
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -0.46))
  (setf (gethash '\| ht)
    '(peck-recoil :hand right :finger pinkie :r 3.16 :theta -0.32))
  (setf (gethash 'pipe ht)
    '(peck-recoil :hand right :finger pinkie :r 3.16 :theta -0.32))
  ht)

(defgeneric populate-modded-key-to-command-ht (ht)
  (:documentation "Populate the modded key hash table."))

(defmethod populate-modded-key-to-command-ht ((ht hash-table))
  (setf (gethash '~ ht) 
    '(peck-recoil :hand left :finger pinkie :r 2.24 :theta -2.03))
  (setf (gethash 'tilde ht) 
    '(peck-recoil :hand left :finger pinkie :r 2.24 :theta -2.03))
  (setf (gethash '! ht) 
    '(peck-recoil :hand left :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'bang ht) 
        '(peck-recoil :hand left :finger pinkie :r 2 :theta -1.57))
  (setf (gethash '@ ht) 
    '(peck-recoil :hand left :finger ring :r 2 :theta -1.57))
  (setf (gethash 'at ht) 
        '(peck-recoil :hand left :finger ring :r 2 :theta -1.57))
  (setf (gethash '\# ht)
    '(peck-recoil :hand left :finger middle :r 2 :theta -1.57))
  (setf (gethash 'hash ht)
        '(peck-recoil :hand left :finger middle :r 2 :theta -1.57))
  (setf (gethash '$ ht) 
    '(peck-recoil :hand left :finger index :r 2 :theta -1.57))
  (setf (gethash 'dollar ht) 
        '(peck-recoil :hand left :finger index :r 2 :theta -1.57))
  (setf (gethash '% ht) 
    '(peck-recoil :hand left :finger index :r 2.24 :theta -1.11))
  (setf (gethash 'percent ht) 
        '(peck-recoil :hand left :finger index :r 2.24 :theta -1.11))
  (setf (gethash '^ ht) 
    '(peck-recoil :hand right :finger index :r 2.24 :theta -2.03))
  (setf (gethash 'carrot ht) 
        '(peck-recoil :hand right :finger index :r 2.24 :theta -2.03))
  (setf (gethash '& ht) 
    '(peck-recoil :hand right :finger index :r 2 :theta -1.57))
  (setf (gethash 'ampersand ht) 
        '(peck-recoil :hand right :finger index :r 2 :theta -1.57))
  (setf (gethash '* ht) 
    '(peck-recoil :hand right :finger middle :r 2 :theta -1.57))
  (setf (gethash 'asterisk ht) 
        '(peck-recoil :hand right :finger middle :r 2 :theta -1.57))
  (setf (gethash '\( ht) 
    '(peck-recoil :hand right :finger ring :r 2 :theta -1.57))
  (setf (gethash 'open-parenthesis ht) 
        '(peck-recoil :hand right :finger ring :r 2 :theta -1.57))
  (setf (gethash '\) ht) 
    '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'close-parenthesis ht) 
    '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57))
  (setf (gethash '_ ht) 
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -1.11))
  (setf (gethash 'under-score ht) 
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -1.11))
  (setf (gethash '+ ht)
    '(peck-recoil :hand right :finger pinkie :r 2.83 :theta -0.78))
  (setf (gethash 'plus ht)
    '(peck-recoil :hand right :finger pinkie :r 2.83 :theta -0.78))
  (setf (gethash 'backslash ht) 
        '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57))
  (setf (gethash 'semicolon ht) '(punch :hand right :finger pinkie))
  (setf (gethash '{ ht) 
    '(peck-recoil :hand right :finger pinkie :r 1.41 :theta -0.78))
  (setf (gethash 'open-curly-brace ht) 
    '(peck-recoil :hand right :finger pinkie :r 1.41 :theta -0.78))
  (setf (gethash '\" ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta 0))
  (setf (gethash 'double-quote ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta 0))
  (setf (gethash '} ht)
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -0.46))
  (setf (gethash 'close-curly-brace ht)
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -0.46))
  (setf (gethash '\| ht)
    '(peck-recoil :hand right :finger pinkie :r 3.16 :theta -0.32))
  (setf (gethash 'pipe ht)
    '(peck-recoil :hand right :finger pinkie :r 3.16 :theta -0.32))
  (setf (gethash '< ht) 
    '(peck-recoil :hand right :finger middle :r 1 :theta 1.57))
  (setf (gethash 'open-angle-brace ht) 
        '(peck-recoil :hand right :finger middle :r 1 :theta 1.57))
  (setf (gethash '> ht) 
    '(peck-recoil :hand right :finger ring :r 1 :theta 1.57))
  (setf (gethash 'close-angle-brace ht) 
        '(peck-recoil :hand right :finger ring :r 1 :theta 1.57))
  (setf (gethash '? ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta 1.57))
  (setf (gethash 'question-mark ht)
      '(peck-recoil :hand right :finger pinkie :r 1 :theta 1.57))


;; letters for modded letter presses, like for typing capital letters
  (setf (gethash 'tab ht) 
    '(peck-recoil :hand left :finger pinkie :r 1.41 :theta -2.36))
  (setf (gethash 'Q ht) 
    '(peck-recoil :hand left :finger pinkie :r 1 :theta -1.57))
  (setf (gethash 'A ht) '(punch :hand left :finger pinkie))
  (setf (gethash 'Z ht) 
    '(peck-recoil :hand left :finger pinkie :r 1 :theta 1.57))
  (setf (gethash 'W ht) 
    '(peck-recoil :hand left :finger ring :r 1 :theta -1.57))
  (setf (gethash 'S ht) 
    '(punch :hand left :finger ring))
  (setf (gethash 'X ht) 
    '(peck-recoil :hand left :finger ring :r 1 :theta 1.57))
  (setf (gethash 'E ht) 
    '(peck-recoil :hand left :finger middle :r 1 :theta -1.57))
  (setf (gethash 'D ht) '(punch :hand left :finger middle))
  (setf (gethash 'C ht) 
    '(peck-recoil :hand left :finger middle :r 1 :theta 1.57))
  (setf (gethash 'R ht) 
    '(peck-recoil :hand left :finger index :r 1 :theta -1.57))
  (setf (gethash 'F ht) '(punch :hand left :finger index))
  (setf (gethash 'V ht) 
    '(peck-recoil :hand left :finger index :r 1 :theta 1.57))
  (setf (gethash 'T ht) 
    '(peck-recoil :hand left :finger index :r 1.41 :theta -0.79))
  (setf (gethash 'G ht) 
    '(peck-recoil :hand left :finger index :r 1 :theta 0))
  (setf (gethash 'B ht) 
    '(peck-recoil :hand left :finger index :r 1.41 :theta 0.79))
  (setf (gethash 'Y ht) 
    '(peck-recoil :hand right :finger index :r 1.41 :theta -2.36))
  (setf (gethash 'H ht) 
    '(peck-recoil :hand right :finger index :r 1 :theta 3.14))
  (setf (gethash 'N ht) 
    '(peck-recoil :hand right :finger index :r 1.41 :theta 2.36))
  (setf (gethash 'U ht) 
    '(peck-recoil :hand right :finger index :r 1 :theta -1.57))
  (setf (gethash 'J ht) 
    '(punch :hand right :finger index))
  (setf (gethash 'M ht) 
    '(peck-recoil :hand right :finger index :r 1 :theta 1.57))
  (setf (gethash 'I ht) 
    '(peck-recoil :hand right :finger middle :r 1 :theta -1.57))
  (setf (gethash 'K ht) '(punch :hand right :finger middle))
  (setf (gethash 'comma ht) 
    '(peck-recoil :hand right :finger middle :r 1 :theta 1.57))
  (setf (gethash 'O ht) 
    '(peck-recoil :hand right :finger ring :r 1 :theta -1.57))
  (setf (gethash 'L ht) 
    '(punch :hand right :finger ring))
  (setf (gethash 'P ht) 
    '(peck-recoil :hand right :finger pinkie :r 1 :theta -1.57))

  (setf (gethash 'return ht) 
    '(peck-recoil :hand right :finger pinkie :r 2 :theta 0))
  
  ;; some of these are stretching the pinkies to fill in the primary area...
  (setf (gethash 'delete ht)
    '(peck-recoil :hand right :finger pinkie :r 3.6 :theta -0.59))
  (setf (gethash 'right-control ht)
    '(peck-recoil :hand right :finger pinkie :r 3.6 :theta 0.59))
  (setf (gethash 'right-option ht)
    '(peck-recoil :hand right :finger pinkie :r 2.83 :theta 0.78))
  (setf (gethash 'right-command ht)
    '(peck-recoil :hand right :finger pinkie :r 2.24 :theta 1.11))
  (setf (gethash 'left-control ht)
    '(peck-recoil :hand left :finger pinkie :r 2.24 :theta 2.03))
  (setf (gethash 'left-option ht)
    '(peck-recoil :hand left :finger pinkie :r 2.0 :theta 1.57))
  (setf (gethash 'left-command ht)
    '(peck-recoil :hand left :finger ring :r 2.0 :theta 1.57))

;; the top row: escape & function keys 1-15
  (setf (gethash 'esc ht)
      '(peck-recoil :hand left :finger pinkie :r 4.1231055 :theta -1.8157749493890485D0))
  (setf (gethash 'f1 ht)
      '(peck-recoil :hand left :finger pinkie :r 4.1231055 :theta -1.3258177))
  (setf (gethash 'f2 ht)
      '(peck-recoil :hand left :finger ring :r 4.1231055 :theta -1.3258177))
  (setf (gethash 'f3 ht)
      '(peck-recoil :hand left :finger middle :r 4.1231055 :theta -1.3258177))
  (setf (gethash 'f4 ht)
      '(peck-recoil :hand left :finger index :r 4.1231055 :theta -1.3258177))
  (setf (gethash 'f5 ht)
      '(peck-recoil :hand right :finger index :r 4 :theta -1.5707963267948966D0))
  (setf (gethash 'f6 ht)
      '(peck-recoil :hand right :finger middle :r 4 :theta -1.5707963267948966D0))
  (setf (gethash 'f7 ht)
      '(peck-recoil :hand right :finger ring :r 4 :theta -1.5707963267948966D0))
  (setf (gethash 'f8 ht)
      '(peck-recoil :hand right :finger pinkie :r 4 :theta -1.5707963267948966D0))
  (setf (gethash 'f9 ht)
      '(peck-recoil :hand right :finger index :r 6.4031243 :theta -0.67474097))
  (setf (gethash 'f10 ht)
      '(peck-recoil :hand right :finger middle :r 6.4031243 :theta -0.67474097))
  (setf (gethash 'f11 ht)
      '(peck-recoil :hand right :finger ring :r 6.4031243 :theta -0.67474097))
  (setf (gethash 'f12 ht)
      '(peck-recoil :hand right :finger pinkie :r 6.4031243 :theta -0.67474097))
  (setf (gethash 'f13 ht)
      '(peck-recoil :hand right :finger index :r 10.770329 :theta -0.38050637))
  (setf (gethash 'f14 ht)
      '(peck-recoil :hand right :finger middle :r 10.770329 :theta -0.38050637))
  (setf (gethash 'f15 ht)
      '(peck-recoil :hand right :finger ring :r 10.770329 :theta -0.38050637))

  ht)

(defclass virtual-keyboard ()
   ((key->cmd-ht :accessor key->cmd-ht 
                  :initform (make-hash-table :test #'equal))
    (modded-key->cmd-ht :accessor modded-key->cmd-ht
                        :initform (make-hash-table :test #'equal))
    (key->loc-ht :accessor key->loc-ht
                  :initform (make-hash-table :test #'equal))
    (loc->key-array :accessor loc->key-arr 
                     :initform (make-array '(23 7) :initial-element nil))
    (loc->non-alph-key-array :accessor loc->non-alph-key-arr 
                     :initform (make-array '(23 7) :initial-element nil))))

(defmethod initialize-instance :after ((vk virtual-keyboard) &key)
  (populate-key-to-command-ht (key->cmd-ht vk))
;; nonalpha-char should probably be split among key-to-command & modded-key-to-command
  (populate-nonalpha-char-to-command-ht (key->cmd-ht vk))
  (populate-modded-key-to-command-ht (modded-key->cmd-ht vk))
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

(clear-all)

(define-model noisy-typer)

(install-device (make-instance 'exp-wind))

(setf (keyboard (current-device-interface))
      (make-instance 'virtual-keyboard))





(defun typing-error (radius theta)
  (let ((on-noise (act-r-noise (* .1  (/ 1 4.133) (/ (sqrt 3) pi)))) 
        (off-noise (act-r-noise (* .1 0.75 (/ 1 4.133) (/ (sqrt 3) pi)))))
;; differ from given radius by no more than 1 
    (cond
     ((> on-noise 1) (setf on-noise 1))
     ((< on-noise -1) (setf on-noise -1)))
    (list 
;; distance is always positive
     (max 0 (+ radius on-noise))
     (+ theta off-noise))))

(defmethod queue-output-events ((mtr-mod motor-module) (self peck-recoil))
  (let ((new-xy (polar-move-xy (finger-loc-m mtr-mod (hand self) (finger self))
                               (vector (r self) (theta self)))))
    ;; Finger bounds are 0:22 & 0:6
    (setf (svref new-xy 0) (max 0 (min 22 (svref new-xy 0))))
    (setf (svref new-xy 1) (max 0 (min 6 (svref new-xy 1))))
    
    (schedule-event-relative (exec-time self) 'output-key :destination :device :module :motor :output 'medium
                             :params (list new-xy))))




;; I must redefine this so that when a mod key is held and a nonalphanumeric character is
;; intended, the modded keypress actually happens. This is made necessary be the necessity
;; of being able to simulate errors wherein a nonalphanumeric character was intended, but
;; the typist failed to hold the mod key, eg meant to type "<", but failed to hold shift
;; and so typed "," instead.
(defmethod key-to-command ((vk virtual-keyboard) key)
  (if *held-keys*
      (gethash key (modded-key->cmd-ht vk))
      (gethash key (key->cmd-ht vk))))


(defmethod key->cmd ((devin device-interface) key)
  (let ((key-cmd (key-to-command (keyboard devin) key)))
    (if (eq (car key-cmd) 'punch)
        key-cmd
        (destructuring-bind 
            (cmd h0 h1 f0 f1 r0 r1 t0 t1)
            (key-to-command (keyboard devin) key)
          (destructuring-bind
              (rad the)
              (typing-error r1 t1)
            (list cmd h0 h1 f0 f1 r0 rad t0 the))))))

;;; ACT-R needs to perform key chords like people do, e.g. hold shift while 
;;; pressing another key.
;;; Hold Key
(defgeneric hold-key (mtr-mod chunk-spec)
  (:documentation "Like press-key, except ACT-R holds the key until release-key is called."))

(defmethod hold-key ((mtr-mod motor-module) chunk-spec)
  (let ((key (nth 2 (car (chunk-spec-slot-spec chunk-spec 'key)))))
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
    (push key *held-keys*)))
  

;;; Release Key
(defgeneric release-key (mtr-mod chunk-spec)
  (:documentation "Releases a key already held by hold-key."))

(defmethod release-key ((mtr-mod motor-module) chunk-spec)
  (let ((key (nth 2 (car (chunk-spec-slot-spec chunk-spec 'key)))))
    (setf *held-keys* (delete key *held-keys*))))

(extend-manual-requests-fct '((hold-key (:include press-key))) 'hold-key)
(extend-manual-requests-fct '((release-key (:include press-key))) 'release-key)

;; Press some keys
#| 
(progn
  (dotimes (i 100)
    (hold-key (get-module :motor) 'shift)
    (press-key (get-module :motor) :key 'u)
    (run-until-condition #'(lambda () nil))))
|#



