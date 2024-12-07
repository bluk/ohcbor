# OhCbor

OhCbor is a library which decodes and encodes in the [CBOR][cbor] data format.
CBOR is specified in [RFC 8949][rfc_8949]. Imagine starting with the JSON data
model, making it more efficient by using a binary (instead of plain text)
format, and adding some extensibility to allow more types.

It uses the [Serde][serde] library to serialize and deserialize Bencode data.
It is similar to [Serde JSON][serde_json] in terms of functionality and
implementation.

## Installation

```sh
cargo add ohcbor
```

By default, the `std` feature is enabled.

### Alloc only

If the host environment has an allocator but does not have access to the Rust
`std` library:

```sh
cargo add --no-default-features --features alloc ohcbor
```
## License

Licensed under either of [Apache License, Version 2.0][LICENSE_APACHE] or [MIT
License][LICENSE_MIT] at your option.

### Contributions

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

[LICENSE_APACHE]: LICENSE-APACHE
[LICENSE_MIT]: LICENSE-MIT
[cbor]: https://cbor.io/
[rfc_8949]: https://www.rfc-editor.org/rfc/rfc8949.html
[serde]: https://serde.rs
[serde_json]: https://github.com/serde-rs/json