;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'helm)
(require 'thingatpt)

(defvar roslaunch-jump--re-ros-path)
(defvar roslaunch-jump--re-pkg)
(defvar roslaunch-jump--re-py)

(setq roslaunch-jump--re-ros-path "\\$(find [^ ]*)[^ ]*\\.\\(launch\\|yaml\\|srdf\\|xacro\\|urdf\\|rviz\\|py\\)")
(setq roslaunch-jump--re-pkg "\\(pkg=\"\\([^\"]*\\)\"\\)\\|\\(find \\([-_A-Za-z0-9]+\\)\\)")
(setq roslaunch-jump--re-py "type=\"\\([^\"]*\\.py\\)\"")

(defun roslaunch-jump--get-match-by-index (idx)
  (let ((start (match-beginning idx))
        (end (match-end idx)))
    (when (and start end)
      (buffer-substring start end))))

(defun roslaunch-jump--get-pkg-at-point()
  (when-let* ((_ (thing-at-point-looking-at roslaunch-jump--re-pkg))
              (start (match-beginning 0))
              (end (match-end 0)))
    (let ((pkg-name1 (roslaunch-jump--get-match-by-index 2))
          (pkg-name2 (roslaunch-jump--get-match-by-index 4)))
      (if pkg-name1 pkg-name1
        (if pkg-name2 pkg-name2)))))

(defun roslaunch-jump--get-match-from-current-line (re)
  (let* ((current-line (thing-at-point 'line t))
         (found-match (string-match re current-line)))
    (when found-match
      (if (match-string 2 current-line)
          (match-string 2 current-line)
        (if (match-string 4 current-line)
            (match-string 4 current-line))))))

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
             (absolute-path (shell-command-to-string (concat "/bin/echo -n " ros-path))))
        (if (file-exists-p absolute-path)
            (find-file absolute-path)
          (message absolute-path))))))

(defun roslaunch-jump-to-pkg (search)
  (let* (
         (pkg-name (roslaunch-jump--get-pkg-at-point))
         (absolute-path
          (when pkg-name (replace-regexp-in-string "\n$" "" (shell-command-to-string (format "rospack find %s" pkg-name))))))
    (when absolute-path
      (if search
          (helm-browse-project-find-files (concat absolute-path "/"))
        (helm-find-files-1 (concat absolute-path "/"))))))

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
    (spacemacs/set-leader-keys-for-major-mode 'nxml-mode "d" 'roslaunch-jump-to-pkg-browse-dir)
    (spacemacs/set-leader-keys-for-major-mode 'nxml-mode "f" 'roslaunch-jump-to-pkg-find-files))
  (add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
  (add-hook 'nxml-mode-hook 'company-mode)
  (add-hook 'nxml-mode-hook 'my-launch-file-config))

(provide 'roslaunch-jump)
