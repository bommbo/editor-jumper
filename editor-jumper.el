;;; editor-jumper.el --- Jump between Emacs and JetBrains IDEs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: bommbo
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/bommbo/editor-jumper

;;; Commentary:

;; EditorJumper allows you to seamlessly jump between Emacs and JetBrains IDEs
;; (IntelliJ IDEA, WebStorm, PyCharm, etc.), maintaining cursor position and
;; editing context in the existing project window.

;;; Code:

(require 'cl-lib)

(defgroup editor-jumper nil
  "Jump between Emacs and JetBrains IDEs."
  :group 'tools
  :prefix "editor-jumper-")

(defcustom editor-jumper-default-ide "idea"
  "Default JetBrains IDE command to jump to.
Common values:
- \"idea\" for IntelliJ IDEA
- \"webstorm\" for WebStorm
- \"pycharm\" for PyCharm
- \"phpstorm\" for PhpStorm
- \"goland\" for GoLand
- \"rider\" for Rider
- \"clion\" for CLion"
  :type 'string
  :group 'editor-jumper)

(defcustom editor-jumper-ide-alist
  '(("IntelliJ IDEA" . "idea")
	("WebStorm" . "webstorm")
	("PyCharm" . "pycharm")
	("PhpStorm" . "phpstorm")
	("GoLand" . "goland")
	("Rider" . "rider")
	("CLion" . "clion")
	("RubyMine" . "rubymine")
	("DataGrip" . "datagrip")
	("Android Studio" . "studio"))
  "Alist of JetBrains IDE display names to command names."
  :type '(alist :key-type string :value-type string)
  :group 'editor-jumper)

(defcustom editor-jumper-custom-ide-paths nil
  "Custom paths to JetBrains IDE executables.
This is an alist where keys are IDE command names (e.g., \"idea\")
and values are full paths to the executable.
Example: ((\"idea\" . \"/usr/local/bin/idea\"))"
  :type '(alist :key-type string :value-type string)
  :group 'editor-jumper)

(defcustom editor-jumper-common-paths
  '("/usr/local/bin"
	"/usr/bin"
	"/opt/homebrew/bin"
	"~/bin"
	"~/.local/bin"
	"~/Library/Application Support/JetBrains/Toolbox/scripts")
  "Common paths to search for JetBrains IDE commands."
  :type '(repeat string)
  :group 'editor-jumper)

(defun editor-jumper--find-command-path (command)
  "Find the full path for COMMAND.
Similar to the findCommandPath function in the original plugin."
  (or
   ;; 1. Check custom paths first
   (cdr (assoc command editor-jumper-custom-ide-paths))

   ;; 2. If already absolute path and exists, use it
   (when (file-name-absolute-p command)
	 (when (file-exists-p command)
	   command))

   ;; 3. Try using 'which' command on Unix-like systems
   (when (memq system-type '(gnu/linux darwin))
	 (let ((which-output (ignore-errors
						  (string-trim
						   (shell-command-to-string (format "which %s" command))))))
	   (when (and which-output
				  (not (string-empty-p which-output))
				  (file-exists-p which-output))
		 which-output)))

   ;; 4. Search in common paths
   (cl-loop for dir in editor-jumper-common-paths
			for expanded-dir = (expand-file-name dir)
			when (file-directory-p expanded-dir)
			for possible-path = (expand-file-name command expanded-dir)
			when (file-exists-p possible-path)
			return possible-path)

   ;; 5. Fallback to command name (let system PATH handle it)
   command))

(defun editor-jumper--find-project-root ()
  "Find the project root directory.
Tries various methods: project.el, projectile, vc, or defaults to default-directory."
  (or
   ;; Try project.el (Emacs 27+)
   (when (fboundp 'project-root)
	 (when-let ((project (project-current)))
	   (if (fboundp 'project-root)
		   (project-root project)
		 (car (project-roots project)))))

   ;; Try projectile
   (when (fboundp 'projectile-project-root)
	 (ignore-errors (projectile-project-root)))

   ;; Try vc
   (when (fboundp 'vc-root-dir)
	 (vc-root-dir))

   ;; Fallback to current directory
   default-directory))

(defun editor-jumper--open-in-ide (ide-command project-root file-path line column)
  "Open FILE-PATH at LINE and COLUMN in IDE-COMMAND within PROJECT-ROOT.
The PROJECT-ROOT is passed first to ensure the IDE reuses existing project window."
  (let* ((full-command (editor-jumper--find-command-path ide-command))
		 (args (list full-command)))

	;; CRITICAL: Add project root FIRST (before file path)
	;; This tells the IDE which project window to use
	(when (and project-root (file-directory-p project-root))
	  (setq args (append args (list (expand-file-name project-root)))))

	;; Then add file and line/column information
	(when (and file-path (file-exists-p file-path))
	  (setq args (append args (list "--line" (number-to-string line)
									"--column" (number-to-string column)
									(expand-file-name file-path)))))

	;; Log the command for debugging
	(message "EditorJumper command: %s" (mapconcat #'identity args " "))

	;; Execute command
	(condition-case err
		(apply #'start-process "editor-jumper" nil args)
	  (error
	   (message "Failed to launch IDE: %s" (error-message-string err))
	   nil))))

;;;###autoload
(defun editor-jumper-open-in-jetbrains (&optional ide-name)
  "Open current file in JetBrains IDE at current cursor position.
If IDE-NAME is provided, use that IDE; otherwise use the default or prompt.
With prefix argument, prompt for IDE selection.

The IDE will reuse the existing project window instead of opening a new instance."
  (interactive)
  (let* ((file-path (buffer-file-name))
		 (line (line-number-at-pos))
		 (column (current-column))
		 (project-root (editor-jumper--find-project-root))
		 (ide (or ide-name
				  (if current-prefix-arg
					  (completing-read "Select IDE: "
									 (mapcar #'car editor-jumper-ide-alist)
									 nil t)
					(car (rassoc editor-jumper-default-ide
							   editor-jumper-ide-alist)))))
		 (ide-command (if (assoc ide editor-jumper-ide-alist)
						 (cdr (assoc ide editor-jumper-ide-alist))
					   (or ide-name editor-jumper-default-ide))))

	(cond
	 ((not file-path)
	  (message "No file associated with current buffer"))

	 ((not project-root)
	  (message "No project root found"))

	 (t
	  (editor-jumper--open-in-ide ide-command project-root file-path line column)
	  (message "Opening in %s: %s (line %d, column %d)"
			   ide-command (file-name-nondirectory file-path) line column)))))

;;;###autoload
(defun editor-jumper-select-ide ()
  "Select the default JetBrains IDE to use for jumping."
  (interactive)
  (let* ((ide-name (completing-read "Select default IDE: "
								   (mapcar #'car editor-jumper-ide-alist)
								   nil t))
		 (ide-command (cdr (assoc ide-name editor-jumper-ide-alist))))
	(customize-save-variable 'editor-jumper-default-ide ide-command)
	(message "Default IDE set to: %s (%s)" ide-name ide-command)))

;;;###autoload
(defun editor-jumper-open-project-in-jetbrains (&optional ide-name)
  "Open current project in JetBrains IDE without opening a specific file.
If IDE-NAME is provided, use that IDE; otherwise use the default or prompt.
With prefix argument, prompt for IDE selection."
  (interactive)
  (let* ((project-root (editor-jumper--find-project-root))
		 (ide (or ide-name
				  (if current-prefix-arg
					  (completing-read "Select IDE: "
									 (mapcar #'car editor-jumper-ide-alist)
									 nil t)
					(car (rassoc editor-jumper-default-ide
							   editor-jumper-ide-alist)))))
		 (ide-command (if (assoc ide editor-jumper-ide-alist)
						 (cdr (assoc ide editor-jumper-ide-alist))
					   (or ide-name editor-jumper-default-ide))))

	(if (and project-root (file-directory-p project-root))
		(progn
		  (message "Opening project in %s: %s" ide-command project-root)
		  (start-process "editor-jumper" nil
						(editor-jumper--find-command-path ide-command)
						(expand-file-name project-root)))
	  (message "No project root found"))))

;;;###autoload
(defun editor-jumper-configure ()
  "Open EditorJumper configuration using customize."
  (interactive)
  (customize-group 'editor-jumper))

;;;###autoload
(defun editor-jumper-add-custom-ide (name command-path)
  "Add a custom IDE configuration.
NAME is the display name of the IDE.
COMMAND-PATH is the command or path to the IDE executable."
  (interactive "sIDE Name: \nfIDE Command or Path: ")
  (let ((expanded-path (expand-file-name command-path)))
	(customize-save-variable
	 'editor-jumper-custom-ide-paths
	 (cons (cons name expanded-path)
		   (assq-delete-all name editor-jumper-custom-ide-paths)))
	(message "Added custom IDE: %s -> %s" name expanded-path)))

;; Define a minor mode for easier key binding
;;;###autoload
(define-minor-mode editor-jumper-mode
  "Minor mode for EditorJumper key bindings."
  :lighter " EJ"
  :keymap (let ((map (make-sparse-keymap)))
			(define-key map (kbd "C-c j j") #'editor-jumper-open-in-jetbrains)
			(define-key map (kbd "C-c j p") #'editor-jumper-open-project-in-jetbrains)
			(define-key map (kbd "C-c j s") #'editor-jumper-select-ide)
			(define-key map (kbd "C-c j c") #'editor-jumper-configure)
			map)
  :group 'editor-jumper)

;;;###autoload
(define-globalized-minor-mode global-editor-jumper-mode
  editor-jumper-mode
  (lambda () (editor-jumper-mode 1))
  :group 'editor-jumper)

(provide 'editor-jumper)

;;; editor-jumper.el ends here
