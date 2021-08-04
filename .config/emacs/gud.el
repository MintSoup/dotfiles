;;; -*- lexical-binding: t -*-

(general-define-key :states 'normal
					"<f12>"		'gud-finish
					"<f11>"		'gud-step
					"<f10>"		'gud-next
					"<f9>"		'gud-break
					"<f5>"		'gud-cont
					"<delete>"	'gud-remove)
(setq gdb-many-windows t)
