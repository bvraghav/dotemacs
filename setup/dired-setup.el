;;; dired-setup.el --- Setup Dired Enhancements -*- lexical-binding: t -*-

;; Author: B.V. Raghav
;; Maintainer: B.V. Raghav
;; Version: 0.1
;; Package-Requires: ()
;; Homepage: https://github.com/bvraghav/dotemacs.git
;; Keywords: dired,dotemacs


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Setup Dired Enhancements

;;; Code:


(use-package dired-x
  :after dired-aux

  :bind (:map dired-mode-map
              ("C-c C-n" . dired-narrow)
              ("C-c ."   . bvr-dired-kill-dotfiles)
              ("C-c C-d" . bvr-dired-desktop-revert)
              ("C-c C-t" . bvr-dired-open-tmux-here))

  :init
  (defun bvr-dired-kill-dotfiles ()
    (interactive)
    (dired-mark-files-regexp "\\..*")
    (dired-do-kill-lines))


  (defun bvr-dired-desktop-revert ()
    (interactive)
    (let* ((filename (dired-get-file-for-visit))
	   (desktop-dirname (file-name-directory filename))
	   (desktop-base-file-name (file-name-nondirectory filename)))
      (desktop-revert)))

  (defun bvr-dired-open-tmux-here()
    (interactive)
    (let ((dirname (or (when (equal major-mode 'dired-mode)
                         (bvr-dired-dwim-directory))
                       default-directory)))
      (async-shell-command
       (format "xterm -e 'tmux new-session -c \"%s\"'" dirname))))

  (defun bvr-dired-dwim-directory()
    (let ((sel (dired-get-file-for-visit)))
      (if (file-directory-p sel)
          sel
        (directory-file-name (file-name-directory sel)))))

  :config

  ;; Dired Mode archive handler
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(with-eval-after-load 'dired
  (setq  dired-isearch-filenames t
         dired-listing-switches "-alh"))

(use-package dired-quick-sort
  :ensure t
  :hook ((dired-mode . dired-quick-sort))
  :config
  (dired-quick-sort-setup)
  (dired-quick-sort "time" nil 121))

(use-package dired-hacks-utils
  :ensure t)


(use-package dired-filter
  :ensure t
  :config
  (define-key dired-mode-map (kbd "F") dired-filter-map)
  (define-key dired-mode-map (kbd "f") dired-filter-mark-map))

(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions
        '(
          ;; XournalPP xournalpp
          ;; ("xopp" . "xdg-open")
          ("xopp" . "xournalpp")    ; Require this hack
                                    ; because of
                                    ; https://github.com/xournalpp/xournalpp/issues/6069
          ;; Images/ Graphics
          ("jpg"  . "xdg-open")
          ("jif"  . "xdg-open")
          ("jpe"  . "xdg-open")
          ("jfi"  . "xdg-open")
          ("jfif" . "xdg-open")
          ("jpeg" . "xdg-open")
          ("png"  . "xdg-open")
          ("gif"  . "xdg-open")
          ("webp" . "xdg-open")
          ("tif"  . "xdg-open")
          ("tiff" . "xdg-open")
          ("psd"  . "xdg-open")
          ("raw"  . "xdg-open")
          ("arw"  . "xdg-open")
          ("cr2"  . "xdg-open")
          ("nrw"  . "xdg-open")
          ("k25"  . "xdg-open")
          ("bmp"  . "xdg-open")
          ("heif" . "xdg-open")
          ("heic" . "xdg-open")
          ("ind"  . "xdg-open")
          ("indd" . "xdg-open")
          ("indt" . "xdg-open")
          ("jp2"  . "xdg-open")
          ("j2k"  . "xdg-open")
          ("jpf"  . "xdg-open")
          ("jpx"  . "xdg-open")
          ("jpm"  . "xdg-open")
          ("mj2"  . "xdg-open")
          ("svg"  . "xdg-open")
          ("svgz" . "xdg-open")
          ("ai"   . "xdg-open")
          ("eps"  . "xdg-open")
          ("pdf"  . "xdg-open")
          ;; Archives
          ("iso"  . "xdg-open")
          ("mar"  . "xdg-open")
          ("tar"  . "xdg-open")
          ("br"   . "xdg-open")
          ("bz2"  . "xdg-open")
          ("gz"   . "xdg-open")
          ("lz"   . "xdg-open")
          ("lz4"  . "xdg-open")
          ("lzma" . "xdg-open")
          ("lzo"  . "xdg-open")
          ("rz"   . "xdg-open")
          ("sz"   . "xdg-open")
          ("xz"   . "xdg-open")
          ("z"    . "xdg-open")
          ("Z"    . "xdg-open")
          ("zst"  . "xdg-open")
          ("7z"   . "xdg-open")
          ("rar"  . "xdg-open")
          ("xar"  . "xdg-open")
          ("zip"  . "xdg-open")
          ("zz"   . "xdg-open")
          )

        dired-open-functions
        '(dired-open-by-extension
          dired-open-call-function-by-extension
          dired-open-subdir))
  )

(use-package dired-rainbow
  :ensure t
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg" "xopp"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    ))

(use-package dired-subtree
  :ensure t)

(use-package dired-ranger
  :ensure t)

(use-package dired-narrow
  :ensure t)

(use-package dired-list
  :ensure t)

(use-package dired-collapse
  :ensure t)

(provide 'dired-setup)

;;; dired-setup.el ends here
