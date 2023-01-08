# lisp-lox
This is an interpreter for the lox programming language written in Common Lisp. This was created by following the first part of the
https://craftinginterpreters.com/ book. This was my first time using Lisp so be advised that the code starts off pretty bad and
(at least in my opinion) gets better as I go on. I did my best to use the functional paradigm as much as possible but I did use a few
imperative hacks to make my life easier. I also did not implement a resolver so some invalid inputs will cause ugly errors, but I really couldn't be bothered by the end.

There also isn't a way to run this interpreter from the CLI so if you want to play around with it you need to run the `main` function in the `lisp-lox.lisp` file.
If I feel like it in the future I will remove this hurdle.
