# extendable-data-helpers
Helper methods for [extendable-data](https://crates.io/crates/extendable-data). Exists because I can't just EXPORT REGULAR FUNCTIONS IN A PROC-MACRO PACKAGE.

Methods in here are really for internal use. Ideally, these would be in an internal package for `extendable-data`, but cargo does not allow me to publish crates with a hidden internal package, nor am I able to expose proc macros and regular functions at the same time, so we're stuck with this additional package.
