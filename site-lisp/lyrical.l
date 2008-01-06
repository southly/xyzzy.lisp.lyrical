;;; lyrical.l
;;;
;;;     �u���@���� �����J����Lisp�vhttp://lyrical.bugyo.tk/download.html
;;;     �̃t���[���[�h�̎g�p���x�����܂��B
;;;
;;; * �g�p�@
;;;  1. $XYZZY/site-lisp/ �� lyrical.l ���A$XYZZY/etc/ �� Lyrical ��z�u���܂��B
;;;  2. ~/.xyzzy �Ɉȉ����L�q���Alyrical.l �����[�h����悤�ɂ��܂��B
;;;    (require "lyrical")
;;;  3. �u���@���� �����J����Lisp�v���N�����A�t���[���[�h�̏�Ԃɂ��܂��B
;;;  4. M-x lyrical-lisp �����s���܂��B
;;;  5. Ctrl+J ��S����]���ł��܂��B
;;;
;;; * �ˑ�
;;;  api.l�Ɉˑ����Ă��܂��B
;;;  2007-11-06���_�ł̍ŐV�ł� http://xyzzy.s53.xrea.com/ni/api2006.06.15.tgz �ł��B
;;;
;;; * ����
;;;  - 2007-11-21 : xyzzy �� read �����܂���
;;;  - 2007-11-15 : �C���f���g�E�L�[���[�h�t�@�C������������
;;;  - 2007-11-06 : �����
;;;
;;; * ���C�Z���X
;;;
;;;  NYSL Version 0.9982  http://www.kmonos.net/nysl/
;;;  A. �{�\�t�g�E�F�A�� Everyone'sWare �ł��B���̃\�t�g����ɂ�����l��l���A
;;;     �������̍�������̂������̂Ɠ����悤�ɁA���R�ɗ��p���邱�Ƃ��o���܂��B
;;;
;;;    A-1. �t���[�E�F�A�ł��B��҂���͎g�p������v�����܂���B
;;;    A-2. �L��������}�̂̔@�����킸�A���R�ɓ]�ځE�Ĕz�z�ł��܂��B
;;;    A-3. �����Ȃ��ނ� ���ρE���v���O�����ł̗��p ���s���Ă��\���܂���B
;;;    A-4. �ύX�������̂╔���I�Ɏg�p�������̂́A���Ȃ��̂��̂ɂȂ�܂��B
;;;         ���J����ꍇ�́A���Ȃ��̖��O�̉��ōs���ĉ������B
;;;
;;;  B. ���̃\�t�g�𗘗p���邱�Ƃɂ���Đ��������Q���ɂ��āA��҂�
;;;     �ӔC�𕉂�Ȃ����̂Ƃ��܂��B�e���̐ӔC�ɂ����Ă����p�������B
;;;
;;;  C. ����Ґl�i���� NANRI<southly@gmail.com> �ɋA�����܂��B���쌠�͕������܂��B
;;;
;;;  D. �ȏ�̂R���́A�\�[�X�E���s�o�C�i���̑o���ɓK�p����܂��B
;;;
(provide "lyrical")

(require "api")

(defvar *lyrical-interval* 5)

(defvar *lyrical-keyword-hash-table* nil)
(defvar *lyrical-keyword-file* "Lyrical")

(defvar *lyrical-lisp-mode-map* nil)
(unless *lyrical-lisp-mode-map*
  (setq *lyrical-lisp-mode-map* (make-sparse-keymap))
  (define-key *lyrical-lisp-mode-map* #\LFD 'lyrical-eval-last-sexp)
  (define-key *lyrical-lisp-mode-map* '(#\C-x #\C-e) 'lyrical-eval-last-sexp)
  (define-key *lyrical-lisp-mode-map* #\TAB 'lisp-indent-line)
  (define-key *lyrical-lisp-mode-map* #\C-h 'backward-delete-char-untabify-or-selection)
  (define-key *lyrical-lisp-mode-map* #\RET 'lisp-newline-and-indent)
  (define-key *lyrical-lisp-mode-map* #\C-M-q 'indent-sexp)
  (define-key *lyrical-lisp-mode-map* #\) 'lisp-electric-close))

(defvar *lyrical-lisp-syntax-table* nil)
(unless *lyrical-lisp-syntax-table*
  (setq *lyrical-lisp-syntax-table* (make-syntax-table))
  (dotimes (x 127)
    (let ((c (code-char x)))
      (unless (alphanumericp c)
        (set-syntax-symbol *lyrical-lisp-syntax-table* c))))
  (set-syntax-whitespace *lyrical-lisp-syntax-table* #\SPC)
  (set-syntax-whitespace *lyrical-lisp-syntax-table* #\TAB)
  (set-syntax-whitespace *lyrical-lisp-syntax-table* #\C-l)
  (set-syntax-whitespace *lyrical-lisp-syntax-table* #\RET)
  (set-syntax-start-comment *lyrical-lisp-syntax-table* #\; nil)
  (set-syntax-end-comment *lyrical-lisp-syntax-table* #\LFD nil t)
  (set-syntax-string *lyrical-lisp-syntax-table* #\")
  (set-syntax-quote *lyrical-lisp-syntax-table* #\')
  (set-syntax-quote *lyrical-lisp-syntax-table* #\,)
  (set-syntax-quote *lyrical-lisp-syntax-table* #\`)
  (set-syntax-match *lyrical-lisp-syntax-table* #\( #\))
  (set-syntax-escape *lyrical-lisp-syntax-table* #\\)
  (set-syntax-start-multi-comment *lyrical-lisp-syntax-table* "#|")
  (set-syntax-end-multi-comment *lyrical-lisp-syntax-table* "|#"))

(setf (get 'define 'lisp-indent-hook) 'defun)
(setf (get 'nlambda 'lisp-indent-hook) 'defun)
(setf (get 'begin 'lisp-indent-hook) 'progn)
(setf (get 'letrec 'lisp-indent-hook) 'let)

(let ((hwnd nil))
  (defun find-lyrical-window-handle ()
    (setq hwnd (winapi::FindWindow (si:make-string-chunk "NScripter")
                                   (si:make-string-chunk "���@����@�����J�����k������ �e������"))))

  (defun lyrical-topmost ()
    (winapi::SetWindowPos hwnd winapi::HWND_TOPMOST 0 0 0 0 (logior winapi::SWP_NOMOVE winapi::SWP_NOSIZE)))

  (defun lyrical-find-edit-handle()
    (winapi::FindWindowEx hwnd 0 (si:make-string-chunk "Edit") 0))

  (defun lyrical-do-click()
    (winapi::PostMessage hwnd winapi::WM_LBUTTONUP 0 0))

  (defun lyrical-lisp-eval(sexp)
    (let ((edit (lyrical-find-edit-handle)))
      (winapi::SendMessage edit winapi::WM_SETTEXT 0 (si:make-string-chunk sexp))
      (winapi::PostMessage edit winapi::WM_KEYDOWN winapi::VK_RETURN 0))
    (start-timer *lyrical-interval* #'lyrical-do-click t))
  )

(defun lyrical-eval-last-sexp ()
  (interactive)
  (let ((syntab (syntax-table)))
    (let (from to)
      (cond ((selection-start-end (start end)
               (setq from start to end)))
            (t
             (use-syntax-table ed::*lisp-mode-syntax-table*)
             (unwind-protect
                 (let ((opoint (point)))
                   (setq from (progn
                                (backward-sexp)
                                (point))
                         to (progn
                              (forward-sexp)
                              (point)))
                   (goto-char (max opoint to)))
               (use-syntax-table syntab))))
      (lyrical-lisp-eval (format nil "~s" (read-from-string (buffer-substring from to))))))
  (insert "\n")
  t)

(defun lyrical-lisp()
  (interactive)
  (unless (find-lyrical-window-handle)
    (error "�����J�����k���������N�����Ă��܂���"))
  (lyrical-topmost)
  (set-buffer (get-buffer-create "*Lyrical-Lisp*"))
  (lyrical-lisp-mode)
  (si:*activate-toplevel))

(defun lyrical-lisp-mode ()
  (interactive)
  (let ((*lisp-interaction-mode-hook* nil))
    (lisp-interaction-mode))
  (setq indent-tabs-mode nil)
  (use-keymap *lyrical-lisp-mode-map*)
  (setq buffer-mode 'lyrical-lisp-mode)
  (setq mode-name "�����J����")
  (use-syntax-table *lyrical-lisp-syntax-table*)
  (and *lyrical-keyword-file*
       (null *lyrical-keyword-hash-table*)
       (setq *lyrical-keyword-hash-table*
             (load-keyword-file *lyrical-keyword-file*)))
  (when *lyrical-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *lyrical-keyword-hash-table*))
  (run-hooks '*lyrical-lisp-mode-hook*))