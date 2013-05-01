;; -*-Emacs-Lisp-*-

;;;==============================ido设置==============================
(require 'ido)
(provide 'ido-ubiquitous)
(require 'ido-other-window)
(require 'icomplete)
(icomplete-mode 1)
;; ido smex支持 (命令自动查询)
(require 'smex)
(smex-initialize)
(global-set-key [(meta x)] 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (require 'ido-better-flex)
;; (ido-better-flex/enable)

(when (> emacs-major-version 21)
  (ido-mode 1)
  ;; (ido-everywhere t)
  (setq ido-enable-prefix nil
        confirm-nonexistent-file-or-buffer nil
        ido-enable-tramp-completion nil
        ido-enable-flex-matching t
        ido-show-dot-for-dired t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 8
        ido-max-directory-size 100000
        ))

;; 关闭自动搜索功能.
;; (setq ido-auto-merge-delay-time 99999)
(define-key ido-file-dir-completion-map (kbd "C-c C-s")
  (lambda()
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))

(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq  ido-ignore-buffers
       '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
         "^\*compilation" "^\*GTAGS" "^session\.*" "^\*"))

(global-set-key [(control x) (f)] 'recentf-ido-find-file) ;ido浏览最近打开的文件
(global-set-key [(control c) (f)] 'ido-display-file) ;在另一个窗口, 显示.(不切换) C-c n
(global-set-key [(control c) (j)] 'ido-imenu) ;按照当前模式的语法表, 显示指定的主题.

(add-hook 'ido-setup-hook
          '(lambda ()
             (define-key ido-common-completion-map [(meta J)] 'ido-next-match)
             (define-key ido-common-completion-map [(meta L)] 'ido-prev-match)
             (define-key ido-common-completion-map [(control r)] 'ido-reread-directory)
             (define-key ido-common-completion-map [(control n)] 'ido-magic-backward-char)
             (define-key ido-buffer-completion-map [(meta O)] 'ido-invoke-in-horizontally-buffer)
             (define-key ido-file-completion-map [(meta O)] 'ido-invoke-in-horizontally-buffer)
             (define-key ido-file-dir-completion-map [(meta O)] 'ido-invoke-in-horizontally-buffer)
             (define-key ido-common-completion-map [(control \5)] 'ido-invoke-in-new-frame)
             ))

;;;==============================ido 增强==============================

;; 针对文件和目录, 按照修改时间排序, 而非字母排序.
;; sort ido filelist by mtime instead of alphabetically
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (time-less-p
                 (sixth (file-attributes (concat ido-current-directory b)))
                 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))

(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

;; ido查找tag.
(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

;; 在最近打开文件中查找.
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; 类似于ido-find-file, 但是打开的默认目录为光标所在文件的目录. 在ibuffer内被调用.
(defun ibuffer-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    default-directory)
                                default-directory))))
     (ido-find-file-in-dir default-directory))))

;; 强大的ido-imenu......
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(provide 'my-customize-ido-setting)

;;; my-customize-ido-setting.el ends here
