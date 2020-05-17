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



;;; Определяем место хранения специфичных настроек
;;; (задаются через customize)
(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)
