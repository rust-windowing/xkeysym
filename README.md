# xkeysyms

This crate provides constants representing all of the X11 keyboard symbols.
It also provides utility functions for working with those symbols, and for
converting between keyboard codes and keyboard symbols. This crate does not
depend on a particular implementation of the X11 protocol and can therefore be
used in any context where X11 keyboard symbols are needed.

In addition, this crate contains no unsafe code and is fully compatible with
`no_std` environments.

## MSRV Policy

The Minimum Safe Rust Version for this crate is **1.48.0**.

## License

This package is distributed under the Boost Software License Version 1.0.
Consult the [LICENSE](./LICENSE) file or consult the [web mirror] for more
information.

[web mirror]: https://www.boost.org/LICENSE_1_0.txt 