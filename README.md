rebar
=====

rebar is an Erlang build tool that makes it easy to compile and  
test Erlang applications, port drivers and releases.

rebar is a self-contained Erlang script, so it's easy to distribute or even  
embed directly in a project. Where possible, rebar uses standard Erlang/OTP  
conventions for project structures, thus minimizing the amount of build  
configuration work. rebar also provides dependency management, enabling  
application writers to easily re-use common libraries from a variety of  
locations (git, hg, etc).

Hyperthunk's enhancements (pending inclusion to basho/rebar)
--------

1. Plugins can be loaded dynamically from source, instead of having to be compiled.
2. Source based Plugins (1) can be loaded from a specific folder `plugin_dir`
3. Plugins can run before (`pre_` prefix) or after (`post_` prefix) any command
4. No destructive commands run in deps folders, unless you specifically ask for them 

Building
--------

Information on building and installing Erlang/OTP can be found
in the `INSTALL.md` document.

### Dependencies

To build rebar you will need a working installation of Erlang R13B03 (or
later).

Should you want to clone the rebar repository, you will also require git.

#### Downloading

Clone the git repository:

    $ git clone git://github.com/basho/rebar.git

#### Building rebar

```sh
$ cd rebar
$ ./bootstrap
Recompile: src/getopt
...
Recompile: src/rebar_utils
==> rebar (compile)
Congratulations! You now have a self-contained script called "rebar" in
your current working directory. Place this script anywhere in your path
and you can use rebar to build OTP-compliant apps.
```


Contributing to rebar
=====================

Pull requests and branching
---------------------------

Use one topic branch per pull request.

Do not commit to master in your fork.

Provide a clean branch without any merge commits from upstream.

Usually you should squash any intermediate commits into the original single commit.

Code style
----------

Do not introduce trailing whitespace.

Do not mix spaces and tabs.

Do not introduce lines longer than 80 characters.

[erlang-mode (emacs)](http://www.erlang.org/doc/man/erlang.el.html) indentation is preferred.
vi-only users are encouraged to give [Vimpulse](http://www.emacswiki.org/emacs/Vimpulse) a try.

Writing Commit Messages
-----------------------

Structure your commit message like this:

<pre>
One line summary (less than 50 characters)

Longer description (wrap at 72 characters)
</pre>

### Summary

* Less than 50 characters
* What was changed
* Imperative present tense (fix, add, change)
  * `Fix bug 123`
  * `Add 'foobar' command`
  * `Change default timeout to 123`
* No period

### Description

* Wrap at 72 characters
* Why, explain intention and implementation approach
* Present tense

### Atomicity

* Break up logical changes
* Make whitespace changes separately

Dialyzer and Tidier
-------------------

Before you submit a patch check for discrepancies with
[Dialyzer](http://www.erlang.org/doc/man/dialyzer.html):

```sh
$ cd rebar
$ ./bootstrap debug
$ dialyzer ebin -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs
```

The following discrepancies are known and safe to ignore:

```
rebar_utils.erl:147: Call to missing or unexported function escript:foldl/3
rebar_utils.erl:180: The created fun has no local return
```

It is **strongly recommended** to check the code with
[Tidier](http://tidier.softlab.ntua.gr:20000/tidier/getstarted).  
Select all transformation options and enable **automatic**
transformation.  
If Tidier suggests a transformation apply the changes **manually**
to the source code.  
Do not use the code from the tarball (*out.tgz*) as it will have
white-space changes  
applied by Erlang's pretty-printer.
