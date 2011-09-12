## Installation

- cd ~; mv .emacs.d old-emacs.d
- git clone git://github.com/technomancy/emacs-starter-kit.git .emacs.d
- start emacs
- M-x package-list-packages
- install color-theme, clojure-mode, slime, slime-repl, durendal and whatever you like
- cd .emacs.d
- git clone git://github.com/contentjon/emacs-clojure-setup.git <your-unix-username>
- cd <your-username>; cp local-settings.el.example local-settings.el
- edit local-settings.el to have development-dir point to your root
  directory for your hacking. This file can also be used for all elisp
  stuff you don't want to share with us...
- restart emacs, enjoy

## Coding projects

If you want to keep different sets of open files for different projects,
add functions like this to local-settings.el:

      (defun my-project ()
        (interactive)
        (project "my-project")) ;; path relative to development-dir

Now M-x my-project will jump to your set of open files from that project
