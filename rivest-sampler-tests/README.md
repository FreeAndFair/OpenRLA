Test Cases for Rivest Sampling Algorithm
========================================

The purpose of this repository is to make test cases publicly available
for the [SHA-256][sha-256] pseudo-random sampling algorithm described by
[Ronald L. Rivest][rivest] in 2011.

The test cases in this repository can be used to help check that new
implementations are correct.


Algorithm Background
--------------------

The Rivest sampling algorithm is for things like selecting precincts
at random for an election audit.

The reference implementation of the algorithm (written in Python), along
with a description by Rivest, can be found [here][ref-impl] on Rivest's
web site.

Other implementations include--

* an AngularJS browser implementation called [Quick Sampler][quick-sampler]
  by Chris Jerdonek, and
* an earlier [browser implementation][browser-impl] by
  [Philip B. Stark][stark].


Test Details
------------

The tests are located in the file [`tests.json`](tests.json).

The sample range for each test is the list of whole numbers from 1 to
the upper limit `total`.  The tests should be run "allowing duplicates."

To check that UTF-8 is used for encoding, the tests include some test cases
with non-ASCII seed values, and at least one test case has a seed with a
[non-BMP Unicode character][unicode-non-bmp].

The test cases were checked with Python 2.6.9 and the reference
implementation downloaded from Rivest's web site on August 8, 2014
with [`sampler_version`][sampler-version] "November 14, 2011."
(The implementation states that it was checked using Python 2.6.7.)
For convenience, we include a copy of that download in this repository
as the file [`sampler.py`](sampler.py).

The test cases were also checked using [Quick Sampler][quick-sampler].


Deployment
----------

For convenience, this repository has a [`bower.json`](bower.json) file,
which lets one include the test cases in another web application
using [Bower](http://bower.io/).


For Maintainers
---------------

To deploy a new version, bump the [`"version"`][version-number] number in
`tests.json`, and tag the release with a [SemVer][semver]
Git tag (it is okay to add a prefix of "v").  For example--

    $ git tag v0.2.0
    $ git push origin v0.2.0

Note that the [Bower spec](https://github.com/bower/bower.json-spec#version)
says to leave the version number out of `bower.json`.


License
-------

The contents of this repository are licensed under a BSD 3-clause license,
unless indicated otherwise.  See the [`LICENSE`](LICENSE) file
for the BSD license.  Note that Rivest's reference implementation,
which this repository contains a copy of, is licensed under
the MIT License and not BSD.


Author
------

Chris Jerdonek (<chris.jerdonek@gmail.com>)


[browser-impl]: http://www.stat.berkeley.edu/~stark/Java/Html/sha256Rand.htm
[quick-sampler]: https://github.com/cjerdonek/quick-sampler
[ref-impl]: http://people.csail.mit.edu/rivest/sampler.py
[rivest]: http://people.csail.mit.edu/rivest/
[sampler-version]: https://github.com/cjerdonek/rivest-sampler-tests/blob/master/sampler.py#L6
[semver]: http://semver.org/
[sha-256]: http://en.wikipedia.org/wiki/SHA-2
[stark]: http://www.stat.berkeley.edu/~stark/
[unicode-non-bmp]: http://en.wikipedia.org/wiki/Plane_(Unicode)#Basic_Multilingual_Plane
[version-number]: https://github.com/cjerdonek/rivest-sampler-tests/blob/master/tests.json#L2
