;;; lyrical.l
;;;
;;;     「魔法言語 リリカル☆Lisp」http://lyrical.bugyo.tk/download.html
;;;     のフリーモードの使用を支援します。
;;;
;;; * 使用法
;;;  1. $XYZZY/site-lisp/ に lyrical.l を、$XYZZY/etc/ に Lyrical を配置します。
;;;  2. ~/.xyzzy に以下を記述し、lyrical.l をロードするようにします。
;;;    (require "lyrical")
;;;  3. 「魔法言語 リリカル☆Lisp」を起動し、フリーモードの状態にします。
;;;  4. M-x lyrical-lisp を実行します。
;;;  5. Ctrl+J でS式を評価できます。
;;;
;;; * 依存
;;;  api.lに依存しています。
;;;  2007-11-06時点での最新版は http://xyzzy.s53.xrea.com/ni/api2006.06.15.tgz です。
;;;
;;; * 履歴
;;;  - 2007-11-21 : xyzzy の read をかませた
;;;  - 2007-11-15 : インデント・キーワードファイルをいじった
;;;  - 2007-11-06 : 作った
;;;
;;; * ライセンス
;;;
;;;  NYSL Version 0.9982  http://www.kmonos.net/nysl/
;;;  A. 本ソフトウェアは Everyone'sWare です。このソフトを手にした一人一人が、
;;;     ご自分の作ったものを扱うのと同じように、自由に利用することが出来ます。
;;;
;;;    A-1. フリーウェアです。作者からは使用料等を要求しません。
;;;    A-2. 有料無料や媒体の如何を問わず、自由に転載・再配布できます。
;;;    A-3. いかなる種類の 改変・他プログラムでの利用 を行っても構いません。
;;;    A-4. 変更したものや部分的に使用したものは、あなたのものになります。
;;;         公開する場合は、あなたの名前の下で行って下さい。
;;;
;;;  B. このソフトを利用することによって生じた損害等について、作者は
;;;     責任を負わないものとします。各自の責任においてご利用下さい。
;;;
;;;  C. 著作者人格権は NANRI<southly@gmail.com> に帰属します。著作権は放棄します。
;;;
;;;  D. 以上の３項は、ソース・実行バイナリの双方に適用されます。
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
                                   (si:make-string-chunk "魔法言語　リリカル☆Ｌｉｓｐ Ｆｒｅｅ"))))

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
    (error "リリカル☆Ｌｉｓｐが起動していません"))
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
  (setq mode-name "リリカル☆")
  (use-syntax-table *lyrical-lisp-syntax-table*)
  (and *lyrical-keyword-file*
       (null *lyrical-keyword-hash-table*)
       (setq *lyrical-keyword-hash-table*
             (load-keyword-file *lyrical-keyword-file*)))
  (when *lyrical-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *lyrical-keyword-hash-table*))
  (run-hooks '*lyrical-lisp-mode-hook*))
