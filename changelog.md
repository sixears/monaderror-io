1.2.5.19 2022-11-20
===================
- fix version ID

1.2.5.18 2022-11-19
===================
- natural->0.0.1.13

1.2.5.17 2022-11-19
===================
- has-callstack->1.0.1.16

1.2.5.16 2022-11-19
===================
- include mapPkg in callPackage args

1.2.5.15 2022-11-19
===================
- flake-build-utils->1.0.0.13

1.2.5.14 2022-11-18
===================
- index->1.0.1.17;base0t->0.0.1.10;number->1.1.2.11;base0->0.0.4.8;more-unicode->0.0.17.9;flake-build-utils->1.0.0.12

1.2.5.13 2022-11-17
===================
- use callPackage

1.2.5.12 2022-11-17
===================
- has-callstack->1.0.1.12

1.2.5.11 2022-11-17
===================
- base0->r0.0.4.7

1.2.5.10 2022-11-17
===================
- upgrade to callPackage-based versions

1.2.5.9 2022-11-13
==================
- fix fixed-package-name typo in flake-build-utils

1.2.5.8 2022-11-03
==================
- remove redundant "output" flake-utils

1.2.5.7 2022-11-03
==================
- flake-build-utils->1.0.0.6

1.2.5.6 2022-11-03
==================
- base0->0.0.4.2; base0t->0.0.1.2

1.2.5.5 2022-11-03
==================
- base0->0.0.4.1; base0t->0.0.1.1

1.2.5.4 2022-11-02
==================
- natural->0.0.1.2

1.2.5.3 2022-11-02
==================
- more-unicode -> 0.0.17.2

1.2.5.2 2022-11-02
==================
- upgrade flake-build-utils to 1.0.0.3

1.2.5.1 2022-10-27
==================
- add flake
- use ghc-8.10.7 for tfmt

1.2.5.0 2022-07-05
==================
- add splitMError', AKA sme

1.2.4.0 2022-06-04
==================
- add eitherME

1.2.3.0 2022-05-23
==================
- add unsquashNoSuchThing{,'}

1.2.2.0 2022-01-17
==================
- add leftFail, leftFailP, leftFailShow

1.2.1.0 2021-10-17
==================
- add modifyError, annotateIOE

1.2.0.0 2021-09-04
==================
- add explicit 'forall's throughout MonadError.hs to fix the ordering of the
  type variables

1.1.2.0 2021-07-24
==================
- add ioeErrorString,ioeFilename,ioeHandle,ioeLocation,ioeType

1.1.1.0 2021-07-12
==================
- add NFData instance of IOError

1.1.0.1 2021-05-04
==================
- aggressive littering of HasCallStack

1.1.0.0 2021-05-03
==================
- add stacktrace to IOError

1.0.6.0 2021-04-20
==================
- add asIOErrorT

1.0.5.0 2020-09-14
==================
- add ӂ, asIOErrorY

1.0.4.0 2020-09-13
==================
- add isInappropriateTypeError and friends

1.0.3.0 2020-02-04
==================
- add IOEAddable, ioeAdd, (~~)

1.0.2.0 2019-11-30
==================
- add eToMaybe

1.0.1.1 2019-10-03
==================
- change == to >= in .cabal

1.0.1.0 2019-09-13
==================
- add __monadError__/ж, __monadIOError__/ӝ, eitherIOThrow{,T}, ioThrow

1.0.0.0 2019-09-12
==================
- factored out from fluffy
