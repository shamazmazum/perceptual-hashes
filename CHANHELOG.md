# Changelog

## Version 0.4.0

* Improvement: `phash` algorithm is added which is like `ahash` but acts in
  Fourier domain. Single-float FFTW is now required to build and run
  perceptual-hashes.
* Incompatible change: Now hash functions only accept images of type
  `imago:image`.

## Version 0.3.1

* Incompatible change: `perceptual-hashes` system does not load any imago
  extensions as dependencies. You may want to load `imago/pngio`,
  `imago/jpeg-turbo` and/or `imago/libheif` manually or add these extensions as
  a dependency to your project.
