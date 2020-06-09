;;; Настраиваем управление пакетами
(require 'package)
(package-initialize)

;;; Источники для установки пакетов. Дело в том, что MELPA Stable
;;; содержит не очень свежие версии, поэтому иногда лучше ставить
;;; пакеты из основного репозитория.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"          . "https://orgmode.org/elpa/"))
(setf (cdr (assoc "gnu" package-archives)) "https://elpa.gnu.org/packages/")

;;; Приоритеты репозиториев
(setq package-archive-priorities
      '(("melpa-stable" . 5)
	("gnu" . 5)
	("melpa" . 10)))
(setq package-enable-at-startup nil)

;;; Если пакет use-package не установлен, его нужно скачать и
;;; установить
(unless (package-installed-p 'use-package)
  (message "EMACS install use-package.el")
  (package-refresh-contents)
  (package-install 'use-package))

;;; Установили, загрузили, указали, что недостающие пакеты нужно
;;; автоматически загружать и устанавливать.
(require 'use-package)
(setq use-package-always-ensure t)



;;; Подключаем git
(use-package magit)



;;; Подключаем Orgmode
(use-package org
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  (setq org-directory "~/workspace/org-files")
  (setq org-agenda-file-regexp "\\(?:\\.org\\|\\.org\\.gpg\\)\\'")
  (setq org-agenda-files (list org-directory))
  )

;;; Подключаем gpg
(use-package pinentry)
(pinentry-start)



;;; Подключаем  Language server
(use-package flycheck)
(use-package lsp-mode
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package company-lsp
  :commands company-lsp)
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
	 (lambda () (require 'ccls) (lsp))))
(setq ccls-executable "/usr/bin/ccls")


;;; Подключаем управление проектами
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))



;;; Включаем перенос по словам
(setq word-wrap          t)
(global-visual-line-mode t)

;;; Отключаем автосохранение
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)



;;; Определяем место хранения специфичных настроек
;;; (задаются через customize)
(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)
