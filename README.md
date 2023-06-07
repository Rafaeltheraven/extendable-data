# extendable-enums
A set of rust macros that allows you to specify enums that can be "extended" or inherited from. 

## Why not composition/traits/some other method
This project started because I was using the very nice [logos](https://github.com/maciejhirsz/logos) library and I wanted to define 2 lexer with some of the same base tokens
but extended. I could not find a proper way to do this that was not simply copy-pasting the parts of the enum that I needed, so I set about bodging it with macro abuse.

Technically this approach could also be used for doing the same with structs, but I haven't looked into that yet.

## How to Use
Simply, define the base enum (`A`) that you want to use. Then add the `#[extendable_enum]` attribute to it. This will automatically generate a new macro called `extend_from_A` (or you can specify a name in the attribute arguments). Now, use this new macro for the extended enum `B`.

### Example
```rust
use extendable_enums::extendable_enum;

#[extendable_enum(extend_a)]
enum A {
	One,
	Two,
	Three
}
```
In a crate (we'll use `X` here) that has `proc-macro` set to true, then:

```rust
use X::extend_a;

#[extend_a]
enum B {
	Four,
	Five,
	Six
}

fn main() {
	let e: B ...;
	...
	match e {
		B::One => ...,
		B::Four => ...,
	}
}
```

## Cargo
Because this package uses `proc-macro`, I am not allowed to export any functions which are not procedural macros. However, the `extendable_enum` macro makes use of a helper function to construct the proper `TokenStream`, which is in itself not a procedural macro. As such, a second package exists which exports this function: [extendable-enums-helpers](https://github.com/Rafaeltheraven/extendable-enums-helpers). You are required to include both packages in your dependencies section of `Cargo.toml`.