Here you will find the Hopac libraries:

* Hopac
* Hopac.Core
* Hopac.Extra
* Hopac.Experimental

**Hopac** is the main library that an application using Hopac links to.

**Hopac.Core** is essentially an internal implementation library, written in
C#, that defines some of the types and internal operations of the Hopac
library.  As a user of Hopac, you should not need to explicitly think about
the core library aside from knowing that it is there and needs to be
distributed with an application that uses Hopac.  Pretty much the only reason
for having a separate core library is performance.  At the time of writing, C#
still allows many of the internal synchronization protocols fundamental to
Hopac to be written in such a way that it makes an order of magnitude
performance difference compared to what is conveniently expressible within F#.

**Hopac.Extra** provides additional abstractions that are programmed on top of
the Hopac library.  You may find some of these abstractions useful, but there
is no need to use the extra library when using Hopac.

**Hopac.Experimental** is similar to Hopac.Extra, but the additional
abstractions are more experimental in nature and subject to radical changes.
