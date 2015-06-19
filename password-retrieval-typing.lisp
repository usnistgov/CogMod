;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Authors     : Kristen Greene, Frank Tamborello
;;; Copyright   : (c) 2014, 2015 Kristen Greene & Frank Tamborello
;;; Availability: GNU LGPL, see LGPL.txt
;;; Address     : https://github.com/usnistgov/CogMod
;;;		:
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
;;; Filename    : password-retrieval-typing.lisp
;;; Revision    : 6
;;; 
;;; Description : This is an ACT-R model for password remembering and typing.
;;;		
;;;
;;; Usage	: Call the run-model function. That will print out the 
;;; trace of the model doing the task, return a list of the model’s responses,
;;; and save a file of the stimulus, retrieved character, and typed character
;;; to the Macintosh user's desktop.
;;; Note: This model is meant to be used in conjuction with typing-error.lisp,
;;; a motor & device extension that permits ACT-R to type non-alphanumeric ASCII
;;; characters and to mistype. Attempting to type any non-alphanumeric ASCII
;;; character without having first loaded typing-error.lisp will result in 
;;; errors. 
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;;
;;;
;;;
;;; Theoretical Issues	: 
;;;
;;;
;;; Practical Issues :
;;;
;;;
;;; 
;;; ----- History -----
;;; 2015.03.30	fpt	6
;;; Record trial data to experiment data when the trial ends rather than at keypress.
;;; Note: The model will fail to record data for a trial "Stopped because no events 
;;; to process." Therefore the model must always complete a trial either because 
;;; it types as many characters as are in the stimulus or because it presses return.
;;;
;;; 2015.03.30	fpt	5
;;; 1. There was also a push-reverse error per trial. Should be fixed now.
;;;
;;; 2015.03.27	fpt	4
;;; 1. I had some problems because I failed to appreciate the need, when elements 
;;; of a sequence repeat, for making each element a unique token. So I fixed that
;;; by adding an id slot to the sequence chunks and setting the sjis to be from
;;; that id to the other chunks in the sequence.
;;; 
;;; 2015.03.07	fpt	3
;;; 1. Figured out how I want this model to remember to and press and hold, and
;;; release, shift
;;;
;;; 2015.03.06	fpt	2
;;; 1. Now, using typing-error.lisp revision 9, this model can issue a presskey
;;; request for a non-alphanumeric character and, if shift is not held, the 
;;; appropriate unmodified keypress will occur, rather than a crash.
;;; 
;;;
;;; 2015.02.28	fpt	1
;;; Forked from passwordlearning_2014-11-17_noPM.lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *retrieveds* nil)
(defvar *typeds* nil)
(defvar *m-name* 'password-retrieval-typing)

(defclass trial ()
  ((retrieveds :accessor retrieveds :initarg :retrieveds :initform nil)
   (typeds :accessor typeds :initarg :typeds :initform nil)))


(defun run-model ()
  (with-open-file (file (concatenate 'string "~/Desktop/" (string *m-name*) ".txt")
                        :direction :output :if-exists :supersede)
    (setf *retrieveds* nil
          *typeds* nil)

    (format t "Begin model runs for ~a.~%" *m-name*)

    (format file "stimulus	retrieved      	typed~%")
    ;; Don't forget to add a modifier value to the slots of the chunks encoding the 
    ;; uppercase letters!
    (do* ((stim 
           (list
;; spell out the names of numerals because these values get used for chunk names
;; and ACT-R won't use numerals for chunk names
            '(five c two quote Q e) 
            '(m hash o close-parenthesis f p carrot two a R f two zero seven) 
            '(m three close-parenthesis six one f H w) 
            '(d five one close-parenthesis u four semicolon X three w r f) 
            '(p four d four six asterisk three T x Y) 
            '(q eight zero open-angle-brace U slash C two m v) 
            '(six n zero four percent E i quote H m three V) 
            '(four i under-score five five f Q dollar two M n h three zero) 
            '(three period b H one o) 
            '(u a seven t question-mark C two hash) 
            '(R m o f p a f two two zero seven hash close-parenthesis carrot) 
            '(Q M i f n h four five five two three zero under-score dollar)))
;; Gee, wouldn't it be great if there were some way to process keypress names for the
;; need to shift, rather than having to code it ourselves. What might that look like?
          (shift-keys 
           (list '(nil nil nil nil t nil) 
                 '(nil t nil t nil nil t nil nil t nil nil nil nil)
                 '(nil nil t nil nil nil t nil) 
                 '(nil nil nil t nil nil nil t nil nil nil nil) 
                 '(nil nil nil nil nil t nil t nil t) 
                 '(nil nil nil t t nil t nil nil nil) 
                 '(nil nil nil nil t t nil nil t nil nil t) 
                 '(nil nil t nil nil nil t t nil t nil nil nil nil) 
                 '(nil nil nil t nil nil) 
                 '(nil nil nil nil t t nil t) 
                 '(t nil nil nil nil nil nil nil nil nil nil t t t) 
                 '(t t nil nil nil nil nil nil nil nil nil nil t t)))
          (trl 0 (incf trl))
          (this-shift-keys (nth trl shift-keys) (nth trl shift-keys))
          (pwd (nth trl stim) (nth trl stim)))

         ((= trl (length stim))
          (progn
            (setf *retrieveds* (reverse *retrieveds*)
                  *typeds* (reverse *typeds*))
            (do* ((i 0 (incf i))
                  (this-stim (nth i stim) (nth i stim))
                  (this-retrieveds (nth i *retrieveds*) (nth i *retrieveds*))
                  (this-typeds (nth i *typeds*) (nth i *typeds*))
                  (k 0 (incf k (length this-stim))))
                 ((= i (1+ trl)))
              (do ((j 0 (incf j)))
                  ((= j (length this-stim)))
                (format 
                 file
                 "~a	~a	~a~%"
                 (nth j this-stim)
                 (nth j this-retrieveds)
                 (nth j this-typeds))))))

      
      (let ((mas 20)
            (sjis)
            (pwd-chunks)
            (starter-ck)
            (eop))

        (defun current-pwd-state () (values pwd pwd-chunks starter-ck))

        (defmethod device-handle-keypress ((device trial) key)
          (declare (ignore device))
          (model-output "typed: ~a" key)
          (if (or
               (eq key 'return)
               (= (length (typeds (current-device))) (length pwd)))
              (schedule-break-relative 0)
              (push key (typeds (current-device)))))




        (clear-all)

        ;; When the meta-process stops, collect the trial's data into the experiment data
        (add-post-event-hook
         (lambda (evt)
           ;; Stop because a break is scheduled.
           (when (eq (evt-action evt) 'act-r-event-break-action)
             (terpri)
             (format t "Trial: ~a~%" trl)
             (format t "Retrieveds: ~a~%" (retrieveds (current-device)))
             (format t "Typeds: ~a~%" (typeds (current-device)))

             (push (reverse (retrieveds (current-device))) *retrieveds*)
             (push (reverse (typeds (current-device))) *typeds*)
             ;; Check if the model retrieved & typed as many characters as there were in the 
             ;; password & fill in as necessary to meet the password's length
             (do ((i 0 (incf i)))
                 ((= i (length pwd)))
               (unless (nth i (retrieveds (current-device)))
                 (setf 
                  (retrieveds (current-device)) 
                  (append (retrieveds (current-device)) (list nil))))
               (unless (nth i (typeds (current-device)))
                 (setf 
                  (typeds (current-device)) 
                  (append (typeds (current-device)) (list nil))))))))
     
        (define-model-fct *m-name* nil)
        (sgp-fct 
         (list
          :v t :trace-detail 'low :act nil :sact 'medium :esc t :ans .03 :rt -20 :mas mas
          :needs-mouse nil :trace-mouse nil))

        

;; Here is where run-model fails
        ;; add the member values of the password to ACT-R's DM
        (dotimes (i (length pwd) (setf pwd-chunks (reverse pwd-chunks)))
          (let ((ck (new-symbol-fct (nth i pwd))))
            (push
             (car
              (add-dm-fct
               `((,ck
                  id ,ck
                  value ,(nth i pwd) 
                  ;; uh, should I only call for a modifier slot when there's a value for that slot?
                  ;; but didn't it work just fine always including the slot name regardless?
                  modifier ,(when (nth i this-shift-keys) 'shift))))) pwd-chunks)))


        (format t "~{~a ~}~%" pwd-chunks)
        
        (setf eop (car (add-dm-fct '((value end-of-password)))))

        ;; give the password member values sjis to each other:
        ;; mas to itself
        ;; otherwise mas / 1+ distance i to j modulo length of the password
        ;; Why 1+? To index distance to 1 rather than to 0, so that values that are not
        ;; the same aren't associated as strongly as though they were.
        ;; What about sequence values that repeat?
        (do ((j 0 (incf j)))
            ((= j (length pwd-chunks)) (add-sji-fct 
                                        (push
                                         `(,(nth (1- (length pwd-chunks)) pwd-chunks)
                                           ,eop
                                           ,(/ mas 2))
                                         sjis)))
          (do ((i 0 (incf i)))
              ((= i (length pwd-chunks)))
            (progn
;              (format t "j: ~a	i: ~a~%" (nth j pwd-chunks) (nth i pwd-chunks))
              (push `(,(chunk-slot-value-fct 
                        (nth j pwd-chunks) 'id)
                      ,(nth i pwd-chunks) 
                      ,(if (eq j i)
                           mas
                           (/ 
                            mas 
                            (1+
                             ;; Actually, the model's not going to type each password over and over, but just once.
                             ;; Maybe the sjis shouldn't wrap?
                             (if (> j i)
                                 (- (+ i (length pwd-chunks)) j)
                                 (- i j)))))) sjis)
              (format t "~a~%" (car sjis)))))
        
        
        ;; prime the pump
        (set-buffer-chunk 
         'imaginal 
         (setf starter-ck (car (define-chunks-fct '((value start modifier nil))))))
        (add-sji-fct `((start ,(nth 0 pwd-chunks) ,(/ mas 2))))

        (p retrieve
           =goal>
           operator retrieve
           =imaginal>
           value =item
           ?retrieval>
           buffer empty
           state  free
           ?manual>
           state free
           ==>
           =imaginal>
           =goal>
           operator harvest
           +retrieval>
           - value     =item)

        (p type-mod
           =goal>
           operator harvest
           =retrieval>
           value =item
           =imaginal>
           ?manual>
           preparation free
           !eval! (chunk-slot-value-fct 
                   (no-output (car (buffer-chunk-fct '(retrieval)))) 'modifier)
           !bind! =mod (chunk-slot-value-fct 
                        (no-output (car (buffer-chunk-fct '(retrieval)))) 'modifier)
           ==>
           =goal>
           operator type
           +imaginal>
           =retrieval
           +manual>
           cmd hold-key
           key =mod)

        (p no-mod
           =goal>
           operator harvest
           =retrieval>
           value =item
           =imaginal>
           ?manual>
           preparation free
           !eval! (not 
                   (chunk-slot-value-fct
                    (no-output (car (buffer-chunk-fct '(retrieval)))) 'modifier))
           ==>
           =goal>
           operator type
           +imaginal>
           =retrieval)

        (p end-of-password
           =retrieval>
           value end-of-password
           ?manual>
           preparation free
           =goal>
           =imaginal>
           ==>
           +manual>
           cmd press-key
           key return)
           
        (p type
           =goal>
           operator type
           =imaginal>
           value =item
           ?manual>
           preparation free
           ==>
           !eval! (model-output 
                   "retrieved: ~a" 
                   (car (push =item (retrieveds (current-device)))))
           !eval! (format t "Keys held: ~{~a ~}" *held-keys*)
           =goal>
           operator release
           =imaginal>
           +manual>
           cmd press-key
           key =item)

        (p release-mod
           =goal>
           operator release
           =imaginal>
           value =item
           modifier =mod
           ?manual>
           state free
           !eval! (chunk-slot-value-fct
                   (no-output (car (buffer-chunk-fct '(imaginal)))) 'modifier)
           ==>
           =goal>
           operator retrieve
           =imaginal>
           +manual>
           cmd release-key
           key =mod)

        (p release-no-mod
           =goal>
           operator release
           =imaginal>
           value =item
           ?manual>
           state free
           !eval! (not 
                   (chunk-slot-value-fct 
                    (no-output (car (buffer-chunk-fct '(imaginal)))) 'modifier))
           ==>
           =goal>
           operator retrieve
           =imaginal>)

        (install-device (make-instance 'trial))
        (goal-focus-fct (car (define-chunks-fct '((start-task operator retrieve)))))
        (reset)
        ;; because I haven't yet devised a reasonable way to reset the held keys!
        (setf *held-keys* nil)
        (format t "Trial #: ~a~%" trl)
        (format t "~{~a ~}~%" pwd)
        (run-until-condition #'(lambda () nil))))))
;        (format t "~{~a ~}~%" *retrieveds*)  
;        (format t "~{~a ~}~%" *typeds*)))))

        
        

  



 

