;; Open launch/yaml file in new buffer

(require 'helm)

(setq re-ros-path "\\$(find [^ ]*)[^ ]*\\.\\(launch\\|yaml\\|srdf\\|xacro\\)")
(setq re-pkg "pkg=\"\\([^\"]*\\)\"")
(setq re-py "type=\"\\([^\"]*\\.py\\)\"")

(defun get-match-from-current-line-- (re)
  (let* ((current-line (thing-at-point 'line t))
         (found-match (string-match re current-line)))
    (if found-match
        (match-string 1 current-line))))

(defun remove-new-line-- (str)
  (replace-in-string-- "\n" "" str))

(defun get-pkg-path (pkg-name)
  (let* ((pkg-path (shell-command-to-string (concat "rospack find " pkg-name)))
         (no-package (string-match-p "\\[rospack\\] Error: package .* not found" pkg-path)))
    (when (not no-package) (remove-new-line-- pkg-path))))

(defun get-pkg-file-path (pkg-name file-name)
  (let* ((pkg-path (get-pkg-path pkg-name))
         (file-path-list (directory-files-recursively pkg-path file-name)))
    (car file-path-list)))

(defun jump-to-py ()
  (let ((pkg-name (get-match-from-current-line-- re-pkg))
        (py-file-name (get-match-from-current-line-- re-py)))
    (if (and pkg-name py-file-name)
        (find-file (get-pkg-file-path pkg-name py-file-name)))))

(defun jump-to-path ()
  (let* ((current-line (thing-at-point 'line t))
         (found-match (string-match re-ros-path current-line)))
    (when found-match
      (let* ((raw-ros-path (match-string 0 current-line))
             (ros-path (replace-in-string-- "find" "rospack find" raw-ros-path))
             (absolute-path (shell-command-to-string (concat "/bin/echo -n " ros-path)))
             (no-package (string-match "\\[rospack\\] Error: package .* not found" absolute-path)))
        (if no-package
            (message (match-string 0 absolute-path))
          (find-file absolute-path))))))

(defun jump-to-pkg (search)
  (let* (
         (pkg-name (get-match-from-current-line-- re-pkg))
         (absolute-path
          (replace-regexp-in-string "\n$" "" (shell-command-to-string (format "rospack find %s" pkg-name)))))
    (if search
        (helm-browse-project-find-files (concat absolute-path "/"))
      (helm-find-files-1 (concat absolute-path "/")))))

(defun jump-to-file ()
  (interactive)
  (when (not (jump-to-py)) (jump-to-path)))

(defun jump-to-pkg-browse-dir ()
  (interactive)
  (jump-to-pkg nil))

(defun jump-to-pkg-find-files ()
  (interactive)
  (jump-to-pkg t))

(defun replace-in-string-- (pattern replacement original-text)
  (replace-regexp-in-string pattern replacement original-text nil 'literal))

(when (fboundp 'nxml-mode)
  (defun my-launch-file-config ()
    "For use in `nxml-mode-hook'."
    (spacemacs/declare-prefix-for-mode 'nxml-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'nxml-mode "mgp" "ros-package")
    (spacemacs/set-leader-keys-for-major-mode 'nxml-mode "gg" 'jump-to-file)
    (spacemacs/set-leader-keys-for-major-mode 'nxml-mode "gpp" 'jump-to-pkg-browse-dir)
    (spacemacs/set-leader-keys-for-major-mode 'nxml-mode "gpf" 'jump-to-pkg-find-files))
    (spacemacs/set-leader-keys-for-major-mode 'nxml-mode "=" 'editorconfig-format-buffer)
    (add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
  (add-hook 'nxml-mode-hook 'company-mode)
  (add-hook 'nxml-mode-hook 'my-launch-file-config))

;; Path autocompletion for $(find pkg-name)/...
(require 'company)

(defun prepare-candidates-- (prefix file-list)
  (let (new-list)
    (dolist (element file-list new-list)
      (setq new-list (cons (concat prefix element) new-list)))))

(defun get-rospack-absolute-path (rospack-find-str)
  (let* ((pkg-path (replace-in-string-- "find" "rospack find" rospack-find-str))
         (absolute-path (shell-command-to-string (concat "/bin/echo -n " pkg-path)))
         (no-package (string-match "\\[rospack\\] Error: package .* not found" absolute-path)))
    (unless no-package
      absolute-path)))

(defun get-candidates-- (rospack-find-str)
  (let (absolute-path)
    (setq absolute-path (get-rospack-absolute-path rospack-find-str))
    (if (file-directory-p absolute-path)
          (setq rospack-find-str (replace-in-string-- "/*\\'" "/" rospack-find-str))
      (setq rospack-find-str (replace-in-string-- "/[^/ \"]+\\'" "/" rospack-find-str)))
    (prepare-candidates-- rospack-find-str (cdr (cdr (directory-files (get-rospack-absolute-path rospack-find-str)))))))

(defun company-rospack-find-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-rospack-find-backend))
    (prefix (and (eq major-mode 'nxml-mode)
                 (when (looking-back "\\$(find [^\"\/ )]+)/[^\" ]*")
                   (match-string 0))))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (get-candidates-- arg)))))

(add-to-list 'company-backends 'company-rospack-find-backend)
