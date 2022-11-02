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
