;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'helm)

(defvar roslaunch-jump--re-ros-path)
(defvar roslaunch-jump--re-pkg)
(defvar roslaunch-jump--re-py)

(setq roslaunch-jump--re-ros-path "\\$(find [^ ]*)[^ ]*\\.\\(launch\\|yaml\\|srdf\\|xacro\\|rviz\\|py\\)")
(setq roslaunch-jump--re-pkg "pkg=\"\\([^\"]*\\)\"")
(setq roslaunch-jump--re-py "type=\"\\([^\"]*\\.py\\)\"")

(defun roslaunch-jump--get-match-from-current-line (re)
  (let* ((current-line (thing-at-point 'line t))
         (found-match (string-match re current-line)))
    (if found-match
        (match-string 1 current-line))))

(defun roslaunch-jump--remove-new-line (str)
  (roslaunch-jump--replace-in-string "\n" "" str))

(defun roslaunch-jump-get-pkg-path (pkg-name)
  (let* ((pkg-path (shell-command-to-string (concat "rospack find " pkg-name)))
         (no-package (string-match-p "\\[rospack\\] Error: package .* not found" pkg-path)))
    (when (not no-package) (roslaunch-jump--remove-new-line pkg-path))))

(defun roslaunch-jump-get-pkg-file-path (pkg-name file-name)
  (let* ((pkg-path (roslaunch-jump-get-pkg-path pkg-name))
         (file-path-list (directory-files-recursively pkg-path file-name)))
    (car file-path-list)))

(defun roslaunch-jump-to-py ()
  (let ((pkg-name (roslaunch-jump--get-match-from-current-line roslaunch-jump--re-pkg))
        (py-file-name (roslaunch-jump--get-match-from-current-line roslaunch-jump--re-py)))
    (if (and pkg-name py-file-name)
        (find-file (roslaunch-jump-get-pkg-file-path pkg-name py-file-name)))))

(defun roslaunch-jump-to-path ()
  (let* ((current-line (thing-at-point 'line t))
         (found-match (string-match roslaunch-jump--re-ros-path current-line)))
    (when found-match
      (let* ((raw-ros-path (match-string 0 current-line))
             (ros-path (roslaunch-jump--replace-in-string "find" "rospack find" raw-ros-path))
             (absolute-path (shell-command-to-string (concat "/bin/echo -n " ros-path)))
             (no-package (string-match "\\[rospack\\] Error: package .* not found" absolute-path)))
        (if no-package
            (message (match-string 0 absolute-path))
          (find-file absolute-path))))))

(defun roslaunch-jump-to-pkg (search)
  (let* (
         (pkg-name (roslaunch-jump--get-match-from-current-line roslaunch-jump--re-pkg))
         (absolute-path
          (replace-regexp-in-string "\n$" "" (shell-command-to-string (format "rospack find %s" pkg-name)))))
    (if search
        (helm-browse-project-find-files (concat absolute-path "/"))
      (helm-find-files-1 (concat absolute-path "/")))))

(defun roslaunch-jump-jump-to-file ()
  (interactive)
  (when (not (roslaunch-jump-to-py)) (roslaunch-jump-to-path)))

(defun roslaunch-jump-to-pkg-browse-dir ()
  (interactive)
  (roslaunch-jump-to-pkg nil))

(defun roslaunch-jump-to-pkg-find-files ()
  (interactive)
  (roslaunch-jump-to-pkg t))

(defun roslaunch-jump--replace-in-string (pattern replacement original-text)
  (replace-regexp-in-string pattern replacement original-text nil 'literal))

(when (fboundp 'nxml-mode)
  (defun my-launch-file-config ()
    "For use in `nxml-mode-hook'."
    (spacemacs/declare-prefix-for-mode 'nxml-mode "mg" "goto")
    (spacemacs/set-leader-keys-for-major-mode 'nxml-mode "=" 'editorconfig-format-buffer)
    (spacemacs/set-leader-keys-for-major-mode 'nxml-mode "gg" 'roslaunch-jump-jump-to-file)
    (spacemacs/set-leader-keys-for-major-mode 'nxml-mode "gd" 'roslaunch-jump-to-pkg-browse-dir)
    (spacemacs/set-leader-keys-for-major-mode 'nxml-mode "gf" 'roslaunch-jump-to-pkg-find-files))
  (add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
  (add-hook 'nxml-mode-hook 'company-mode)
  (add-hook 'nxml-mode-hook 'my-launch-file-config))

;; Path autocompletion for $(find pkg-name)/...
(require 'company)

(defun roslaunch-jump--prepare-candidates (prefix file-list)
  (let (new-list)
    (dolist (element file-list new-list)
      (setq new-list (cons (concat prefix element) new-list)))))

(defun roslaunch-jump--get-rospack-absolute-path (rospack-find-str)
  (let* ((pkg-path (roslaunch-jump--replace-in-string "find" "rospack find" rospack-find-str))
         (absolute-path (shell-command-to-string (concat "/bin/echo -n " pkg-path)))
         (no-package (string-match "\\[rospack\\] Error: package .* not found" absolute-path)))
    (unless no-package
      absolute-path)))

(defun roslaunch-jump--get-candidates (rospack-find-str)
  (let (absolute-path)
    (setq absolute-path (roslaunch-jump--get-rospack-absolute-path rospack-find-str))
    (if (file-directory-p absolute-path)
        (setq rospack-find-str (roslaunch-jump--replace-in-string "/*\\'" "/" rospack-find-str))
      (setq rospack-find-str (roslaunch-jump--replace-in-string "/[^/ \"]+\\'" "/" rospack-find-str)))
    (roslaunch-jump--prepare-candidates rospack-find-str (cdr (cdr (directory-files (roslaunch-jump--get-rospack-absolute-path rospack-find-str)))))))

(defun roslaunch-jump-company-rospack-find-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'roslaunch-jump-company-rospack-find-backend))
    ; TODO: replace looking-back with more efficient methods
    (prefix (and (eq major-mode 'nxml-mode)
                 (when (looking-back "\\$(find [^\"\/ )]+)/[^\" ]*")
                   (match-string 0))))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (roslaunch-jump--get-candidates arg)))))

(add-to-list 'company-backends 'roslaunch-jump-company-rospack-find-backend)

(defun roslaunch-jump--recursively-up-find-file (search-path target-file-name)
  (let ((parent-dir (expand-file-name (directory-file-name (file-name-directory search-path)))))
    (if (file-exists-p (expand-file-name target-file-name parent-dir)) parent-dir
      (if (string= parent-dir "/") nil
        (recursively-up-find-ws parent-dir target-file-name)))))

(defun roslaunch-jump-find_current_catkin_workspace ()
  (interactive)
  (recursively-up-find-ws (spacemacs--file-path)))
