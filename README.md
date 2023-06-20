# extendable-data
A rust macro that allows you to specify data that can be "extended" or inherited from. With data, I mean specifically `enum`, `struct` and `union`.

## Why not composition/traits/some other method
This project started because I was using the very nice [logos](https://github.com/maciejhirsz/logos) library and I wanted to define 2 lexers with some of the same base tokens,
but extended. I could not find a proper way to do this that was not simply copy-pasting the parts of the enum that I needed, so I set about bodging it with macro abuse. I then extended
the approach I used for enums to also support structs and unions.

## How to Use
Simply, define the base enum (`A`) that you want to use. Then add the `#[extendable_data]` attribute to it. This will automatically generate a new macro called `extend_from_A` (or you can specify a name in the attribute arguments). Now, use this new macro for the extended enum `B`.

### Example
```rust
use extendable_data::extendable_data;

#[extendable_data(extend_a)]
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
 |
 V
enum B {
	One,
	Two,
	Three,
	Four,
	Five,
	Six
}
```

Any attributes and generics used in the definitions for enums `A` and `B` are combined and copied over. For the name and visibility, only those of `B` are used and are directly copied over.

## Filter
You can provide a list of fields to filter when using the `extend_from_*` macros. It works as follows:

```rust
#[extend_a(filter(Two))]
enum B {    
	Four
}
 |
 V
enum B {
	One,
	Three,
	Four
}
```

You can also use the filter to specify specific attributes to remove from the original data structure. (For example, `derive` if it this new version should no longer derive).

If you are using the `merge_on_conflict` argument (see below) and you want to remove an attribute from a field, you MUST force conflict resolution by writing the field again:

```rust
#[extendable_data]
enum A {
	#[attr]
	One,
	Two
}

...

#[extend_from_A(filter(attr))]
enum B {
	One
}
 |
 V
enum B {
	One,
	Two
}
```

## Conflict Resolution
By default, if a name conflict occurs (I.E, you have a field `X` in both `A` and `B`), then the field in `B` will completely overwrite the field in `A`. Optionally, you can pass the `merge_on_conflict` argument to `extend_from_*` to make the library attempt to merge the fields. Merging is only be possible in `enum`s, because nested structs and unions are not supported in Rust.

## Structs
As opposed to enums and unions, not all types of structs make sense to combine together. As such, the following design decisions were made:

* Combining two named structs just generates a new named structs.
* Combining two unnamed structs just generates a new unnamed struct.
* Combining two unit structs just generates a new unit struct.
* Combining a unit struct with anything else will generate the other type of struct (so a unit with a named will generate a named), regardless of which is the "parent".

Technically you could allow the combining of named and unnamed structs, or have the parent matter more when combining with unit structs, but the former would promote even more ugly coding habits than this library already does, and the latter seemed a less common use-case.
