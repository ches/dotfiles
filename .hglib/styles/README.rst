Mercurial CLI Templates
***********************

by `Steve Losh`_

Mercurial has a great command line interface and many people use it without
ever feeling the need for a GUI to manage their repositories. However, we can
make it even better by taking advantage of Mercurial’s templating features.

This repository contains three new templates for Mercurial:

- Short Log
- Nice Log
- Short Graphlog

Check out ``hg help templating`` or the chapter on `customizing the output of
Mercurial`_ in  the Mercurial book if you want more details on how the
templating actually works.

Short Log
=========

This command will print out a log of all the changesets in the repository, one
per line, with each line having the revision number, hash identifier, and
summary.

To use this template you can edit your ``~/.hgrc`` file to contain the
following::

    [alias]
    slog = log --style=/full/path/to/map-cmdline.slog

After adding the alias ``hg slog`` should display the short log.

Nice Log
========

The short log is great a quick review of the past few changesets, but for a
much more detailed view of a particular changeset nice log is more suitable.

To use this template you can edit your ``~/.hgrc`` file to contain the
following::

    [alias]
    nlog = log --style=/full/path/to/map-cmdline.nlog

Now you should be able to display the nice log with the ``hg nlog`` command.

Short Graphlog
==============

The ``graphlog`` command is wonderful for reviewing the history of repositories
with branches, but we can make it more compact and easier to read with another
template.

To use this template you can edit your ``~/.hgrc`` file to contain the
following::

    [alias]
    sglog = glog --style=/full/path/to/map-cmdline.sglog

To show the short graphlog just type ``hg sglog``.

.. _Steve Losh: http://stevelosh.com/
.. _customizing the output of Mercurial: http://hgbook.red-bean.com/read/customizing-the-output-of-mercurial.html

Changelog
=========

* 2012-05-18: Add README
* 2012-02-04: Support phases
* 2011-09-28: Show desc in quiet slog
* 2011-03-09: Wrap bookmarks in [] and show them even in non-verbose mode
