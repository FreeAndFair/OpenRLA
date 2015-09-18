Development
===========

This document contains development information about Quick Sampler.
For example, it includes information about how to build a release
and run and test the source code locally.  It also includes information
for project maintainers like how to release a new version.


Project Background
------------------

This project was started using version 0.9.5 of
[`generator-angular`](https://github.com/yeoman/generator-angular),
the Yeoman generator for AngularJS.

For needed math functions, it depends on the following Javascript libraries,
which are imported via Bower:

* [jsSHA](https://github.com/Caligatio/jsSHA) by Brian Turek for a
  Javascript implementation of SHA-256, and
* [BigInt](https://github.com/Evgenus/BigInt), a thin wrapper around
  a Javascript library by Baird Leemon for high-precision integer arithmetic.


Getting Started
---------------

To develop locally, you will need to download and install [Node.js][node-js].
This will also install Node's package manager [`npm`][npm].

After cloning the repository using [Git][git], run the following from the
repository root--

    $ npm install

This installs the dependencies listed in the file
[`package.json`](package.json) into a subdirectory called `node_modules`.

You will also need to install [Bower][bower], which is a command-line
utility for web application javascript dependencies:

    $ npm install -g bower

Then run the following from the repository root--

    $ bower install

This installs the dependencies listed in the file
[`bower.json`](../bower.json) into a subdirectory called `bower_components`.


Testing
-------

To run unit tests, run the following from the repository root--

    $ grunt test


Running
-------

To run the source code locally from a browser:

    $ grunt serve


Building
--------

To build a release, run the following command from the repository root:

    $ grunt build

This creates the build in a subdirectory called `dist`.

To test a release locally in a browser, run the following:

    $ grunt serve:dist

This runs `grunt build` prior to serving the files.


Releasing
---------

This section describes all of the steps to release a new version
of Quick Sampler.


### 1. Commit and tag the release

Update the version number in [`package.json`](../package.json#L3)
and in the footer of the application home page
[`index.html`](../app/index.html#L36).  Also make sure the
[`HISTORY.md`][history] file is updated so that the latest version
appears at the top.

Make sure the tests pass by running tests.  Also make sure the
application runs from the browser.

Commit remaining changes and tag the release, for example:

    $ git tag v0.2.0
    $ git push origin v0.2.0

In preparation for the following steps, build the release by
following the instructions in the [Building](#building) section above:

    $ grunt build


### 2. Update the GitHub pages release

We maintain a working demo version of the latest stable release
[here][quick-sampler-demo] using [GitHub Pages][github-pages].

Create a new clone of the repository with the `gh-pages` branch checked out
and tracking the remote branch `origin/gh-pages`:

    $ git clone git@github.com:cjerdonek/quick-sampler.git quick-sampler-gh-pages
    $ cd quick-sampler-gh-pages/
    $ git checkout --track origin/gh-pages

Now update the `gh-pages` branch to the latest build by deleting everything:

    $ git rm -r *

Then from your clone of the master branch:

    $ cp -r dist/* <path-to-gh-pages-clone>

Then back in the `gh-pages` clone:

    $ git add --all
    $ git status  # to double-check changes
    $ git commit -m "Update to v0.2.0."
    $ git push

You should now be able to view the updated demo on GitHub pages.
Note that GitHub says it can take up to 10 minutes for changes to appear.


### 3. Create the pre-built release on GitHub

This section describes how to create a new pre-built release on GitHub
using their [Releases](https://github.com/blog/1547-release-your-software)
feature.

Rename the directory containing the build, and gzip it:

    $ mv dist quick-sampler-app-v0.2.0
    $ tar -czf quick-sampler-app-v0.2.0.tar.gz quick-sampler-app-v0.2.0/

Go to the GitHub project page and click the "Releases" tab.
You should see the latest tag you pushed at the top.

Click to "Draft a new release."  Fill in the release information by
doing the following.

Select the latest tag.  For the release title, type a name of the form
"Quick Sample v0.2.0".

For the release description, copy the contents of the section of
[`HISTORY.md`][history] for the version.

Also add the `.tar.gz` file to the binaries section.

Finally, publish the release!


[bower]: http://bower.io/
[git]: http://git-scm.com/
[github-pages]: https://help.github.com/categories/20/articles
[history]: ../HISTORY.md
[node-js]: http://nodejs.org/
[npm]: https://www.npmjs.org/
[quick-sampler-demo]: http://cjerdonek.github.io/quick-sampler/
