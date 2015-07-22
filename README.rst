===========================
 HYAI |travis| |coveralls|
===========================

Overview
========
Hyai is an indentation minor mode for Haskell, written from scratch.
It supports only one style that basically follows Johan Tibell's `Style Guide`_.

Installation
============

Setup from GitHub
-----------------
1. Install from GitHub::

     git clone https://github.com/iquiw/hyai.git

2. Add ``hyai-mode`` to ``haskell-mode-hook``

   .. code:: emacs-lisp

      (add-to-list 'load-path "/path/to/hyai")
      (require 'hyai)
      (add-hook 'haskell-mode-hook #'hyai-mode)

.. _Style Guide: https://github.com/tibbe/haskell-style-guide
.. |travis| image:: https://travis-ci.org/iquiw/hyai.svg?branch=dawn
            :target: https://travis-ci.org/iquiw/hyai
.. |coveralls| image:: https://coveralls.io/repos/iquiw/hyai/badge.svg?branch=dawn&service=github
               :target: https://coveralls.io/github/iquiw/hyai?branch=dawan
