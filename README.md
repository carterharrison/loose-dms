# loose-dms
A Rust crate for parsing geographical coordinates from strings in various formats, including Degrees Minutes Seconds (DMS) and decimal degrees. This crate is designed to be flexible with input formats while ensuring parsed values are valid geographical coordinates.

This is a direct port of the [parse-dms](https://github.com/gmaclennan/parse-dms) JavaScript library by [gmaclennan](https://github.com/gmaclennan).

## Features

- Parse DMS strings with degrees, minutes, and seconds
- Support for hemisphere indicators (N, E, S, W) in different positions
- Handle decimal degrees format
- Flexible separator handling
- Returns structured coordinates with latitude and longitude

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
loose-dms = "0.1.2"
```

### Example

```rust
use loose_dms::parse;

fn main() {
    // Parse DMS format
    let coord = parse("59°12'7.7\"N 02°15'39.6\"W").unwrap();
    assert_eq!(coord.lat, 59.20213888888889);
    assert_eq!(coord.lng, -2.261);

    // Parse decimal degrees
    let coord = parse("51.5, -0.126").unwrap();
    assert_eq!(coord.lat, 51.5);
    assert_eq!(coord.lng, -0.126);
}
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Credits

Thanks to [gmaclennan](https://github.com/gmaclennan) for the original JavaScript implementation.
