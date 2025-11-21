//! # loose_dms
//!
//! This crate provides functionality to parse coordinates from strings
//! in Degrees Minutes Seconds (DMS) or decimal degrees format, accommodating
//! various separators and hemisphere indicators. It aims to be "loose" in the
//! sense of allowing flexible formatting while ensuring that the parsed values
//! are valid geographical coordinates.
//!
//! **Note:** This crate is a direct port of the [parse-dms](https://github.com/gmaclennan/parse-dms)
//! javascript library by [gmaclennan](https://github.com/gmaclennan). Big thanks to the
//! original author for their work.
//!
//! ## Features
//! - Supports DMS strings with degrees, minutes, and seconds.
//! - Accommodates hemispheres (`N`, `E`, `S`, `W`) in various positions.
//! - Parses decimal degrees directly.
//! - Returns structured `Coordinate` objects with `lat` and `lng` fields.
//!
//! ## Example
//!
//! ```rust
//! use loose_dms::parse;
//!
//! let coord = parse("59°12'7.7\"N 02°15'39.6\"W").unwrap();
//! assert_eq!(coord.lat, 59.20213888888889);
//! assert_eq!(coord.lng, -2.261);
//! ```

use lazy_static::lazy_static;
use regex::Regex;
use thiserror::Error;

lazy_static! {
    /// For more information, look at https://github.com/gmaclennan/parse-dms
    static ref DMS_REGEX: Regex = Regex::new(
        r#"(?i)([NSEW])?\s?(-)?(\d+(?:\.\d+)?)[°º:d\s]?\s?(?:(\d+(?:\.\d+)?)['’‘′:]?\s?(?:(\d{1,2}(?:\.\d+)?)(?:"|″|’’|'')?)?)?\s?([NSEW])?"#
    ).unwrap();
}

/// A geographical coordinate expressed as latitude and longitude in decimal degrees.
///
/// The coordinate uses a conventional Earth-based reference system, where:
/// - Latitude ranges from -90.0° (south pole) to +90.0° (north pole).
/// - Longitude ranges from -180.0° (west) to +180.0° (east).
///
/// # Fields
///
/// * `lat` - The latitude in decimal degrees, where positive values represent the northern hemisphere.
/// * `lng` - The longitude in decimal degrees, where positive values represent the eastern hemisphere.
#[derive(Debug, Clone, Copy)]
pub struct Coordinate {
    pub lat: f64,
    pub lng: f64,
}

/// Parse coordinates from a variety of string formats.
///
/// Parses coordinates in DMS (Degrees Minutes Seconds) format with different separators
/// and hemisphere indicators. Also handles decimal degree formats.
///
/// # Examples
///
/// ```
/// use loose_dms::parse;
///
/// // Parse DMS coordinates with hemisphere at end
/// let coord = parse("59°12'7.7\"N 02°15'39.6\"W").unwrap();
/// assert_eq!(coord.lat, 59.20213888888889);
/// assert_eq!(coord.lng, -2.261);
///
/// // Parse DMS coordinates with hemisphere at start
/// let coord = parse("N59°12'7.7\" W02°15'39.6\"").unwrap();
/// assert_eq!(coord.lat, 59.20213888888889);
/// assert_eq!(coord.lng, -2.261);
///
/// // Parse decimal degrees
/// let coord = parse("51.5, -0.126").unwrap();
/// assert_eq!(coord.lat, 51.5);
/// assert_eq!(coord.lng, -0.126);
/// ```
///
/// # Errors
///
/// Returns `Error::CouldNotParse` if the string cannot be parsed as valid coordinates.
/// This includes if:
/// - The string format is invalid
/// - Values are out of valid ranges (degrees: 0-180, minutes/seconds: 0-60)
/// - Required parts are missing
pub fn parse(input: &str) -> Result<Coordinate, Error> {
    let dms_str = input.trim();
    let matched = regex_match(dms_str).ok_or(Error::CouldNotParse("no matches found"))?;

    let mut parts: Vec<&str> = (0..matched.len())
        .map(|i| matched.get(i).map_or("", |m| m.as_str()))
        .collect();

    if parts.len() < 7 {
        return Err(Error::CouldNotParse("not enough matches found"));
    }

    let secondary_dms = if !parts[1].is_empty() {
        parts[6] = "";
        dms_str[parts[0].len() - 1..].trim()
    } else {
        dms_str[parts[0].len()..].trim()
    };

    let mut degree_1 = dec_deg_from_parts(parts)?;

    let secondary_parts: Vec<&str> = match regex_match(secondary_dms) {
        None => vec![],
        Some(secondary_matched) => (0..secondary_matched.len())
            .map(|i| secondary_matched.get(i).map_or("", |m| m.as_str()))
            .collect(),
    };

    let mut degree_2 = if secondary_parts.is_empty() {
        (None, None)
    } else {
        dec_deg_from_parts(secondary_parts)?
    };

    if degree_1.1.is_none() {
        if degree_1.0.is_some() && degree_2.0.is_none() {
            return Ok(Coordinate {
                lat: degree_1.0.unwrap_or(0.0),
                lng: 0.0,
            });
            // then return 1
        } else if degree_1.0.is_some() && degree_2.0.is_some() {
            degree_1.1 = Some(CoordinatePart::Lat);
            degree_2.1 = Some(CoordinatePart::Lng);
        } else {
            return Err(Error::CouldNotParse(
                "provided string does not have lat or lng",
            ));
        }
    }

    let degree_1_is_lat = matches!(
        degree_1.1.unwrap_or(CoordinatePart::Lat),
        CoordinatePart::Lat
    );

    if degree_2.1.is_none() {
        if degree_1_is_lat {
            degree_2.1 = Some(CoordinatePart::Lng);
        } else {
            degree_2.1 = Some(CoordinatePart::Lat);
        };
    };

    if degree_1_is_lat {
        Ok(Coordinate {
            lat: degree_1.0.unwrap_or_default(),
            lng: degree_2.0.unwrap_or_default(),
        })
    } else {
        Ok(Coordinate {
            lat: degree_2.0.unwrap_or_default(),
            lng: degree_1.0.unwrap_or_default(),
        })
    }
}

fn dec_deg_from_parts(parts: Vec<&str>) -> Result<(Option<f64>, Option<CoordinatePart>), Error> {
    let sign = direction_to_sign(parts[2])
        .or_else(|| direction_to_sign(parts[1]))
        .or_else(|| direction_to_sign(parts[6]))
        .unwrap_or(1.0);

    let degrees = match correct_str_num(parts[3]) {
        None => return Ok((None, None)),
        Some(d) => d,
    };

    let minutes: f64 = match correct_str_num(parts[4]) {
        None => return Ok((None, None)),
        Some(d) => d,
    };

    let seconds: f64 = match correct_str_num(parts[5]) {
        None => return Ok((None, None)),
        Some(d) => d,
    };

    let lat_lng = direction_to_lat_lng(parts[1]).or_else(|| direction_to_lat_lng(parts[6]));

    if !is_in_range(degrees, 0.0, 180.0) {
        return Err(Error::CouldNotParse("degress is not in the range [0, 180]"));
    }

    if !is_in_range(minutes, 0.0, 60.0) {
        return Err(Error::CouldNotParse("minutes is not in the range [0, 60]"));
    }

    if !is_in_range(seconds, 0.0, 60.0) {
        return Err(Error::CouldNotParse("seconds is not in the range [0, 60]"));
    }

    let decimal_degree = sign * (degrees + minutes / 60.0 + seconds / (60.0 * 60.0));

    Ok((Some(decimal_degree), lat_lng))
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("{0}")]
    CouldNotParse(&'static str),
}

impl PartialEq for Coordinate {
    fn eq(&self, other: &Self) -> bool {
        self.lat == other.lat && self.lng == other.lng
    }
}

impl Eq for Coordinate {}

enum CoordinatePart {
    Lat,
    Lng,
}

fn direction_to_sign(dir: &str) -> Option<f64> {
    match dir {
        "-" => Some(-1.0),
        "N" => Some(1.0),
        "S" => Some(-1.0),
        "E" => Some(1.0),
        "W" => Some(-1.0),
        _ => None,
    }
}

fn direction_to_lat_lng(dir: &str) -> Option<CoordinatePart> {
    match dir {
        "N" => Some(CoordinatePart::Lat),
        "S" => Some(CoordinatePart::Lat),
        "E" => Some(CoordinatePart::Lng),
        "W" => Some(CoordinatePart::Lng),
        _ => None,
    }
}

fn correct_str_num(str: &str) -> Option<f64> {
    if str.is_empty() {
        return Some(0.0);
    }

    str.parse().ok()
}

fn regex_match(dms_string: &str) -> Option<regex::Captures<'_>> {
    DMS_REGEX.captures(dms_string)
}

fn is_in_range(v: f64, min: f64, max: f64) -> bool {
    v >= min && v <= max
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parses_dms_pairs_with_different_separators_hemisphere_at_end() {
        let test_data = [
            "59°12'7.7\"N 02°15'39.6\"W",
            "59º12'7.7\"N 02º15'39.6\"W",
            "59 12' 7.7\" N 02 15' 39.6\" W",
            "59 12'7.7''N 02 15'39.6'' W",
            "59:12:7.7\"N 2:15:39.6W",
            "59 12'7.7''N 02 15'39.6''W",
        ];

        let expected = Coordinate {
            lat: 59.0 + 12.0 / 60.0 + 7.7 / 3600.0,
            lng: -1.0 * (2.0 + 15.0 / 60.0 + 39.6 / 3600.0),
        };

        for test_str in test_data.iter() {
            assert_eq!(parse(test_str).unwrap(), expected);
        }
    }

    #[test]
    fn test_parses_dms_pairs_with_hemisphere_at_beginning() {
        let test_data = [
            "N59°12'7.7\" W02°15'39.6\"",
            "N 59°12'7.7\" W 02°15'39.6\"",
            "N 59.20213888888889° W 2.261°",
            "N 59.20213888888889 W 2.261",
            "W02°15'39.6\" N59°12'7.7\"",
        ];

        let expected = Coordinate {
            lat: 59.0 + 12.0 / 60.0 + 7.7 / 3600.0,
            lng: -1.0 * (2.0 + 15.0 / 60.0 + 39.6 / 3600.0),
        };

        for test_str in test_data.iter() {
            assert_eq!(parse(test_str).unwrap(), expected);
        }
    }

    #[test]
    fn test_parses_different_separators_between_pairs() {
        let test_data = [
            "59°12'7.7\"N  02°15'39.6\"W",
            "59°12'7.7\"N , 02°15'39.6\"W",
            "59°12'7.7\"N,02°15'39.6\"W",
        ];

        let expected = Coordinate {
            lat: 59.0 + 12.0 / 60.0 + 7.7 / 3600.0,
            lng: -1.0 * (2.0 + 15.0 / 60.0 + 39.6 / 3600.0),
        };

        for test_str in test_data.iter() {
            assert_eq!(parse(test_str).unwrap(), expected);
        }
    }

    #[test]
    fn test_parses_single_coordinate_with_hemisphere() {
        let test_data = ["59°12'7.7\"N", "02°15'39.6\"W"];

        let expected = [
            Coordinate {
                lat: 59.0 + 12.0 / 60.0 + 7.7 / 3600.0,
                lng: 0.0,
            },
            Coordinate {
                lat: 0.0,
                lng: -1.0 * (2.0 + 15.0 / 60.0 + 39.6 / 3600.0),
            },
        ];

        for (test_str, expected) in test_data.iter().zip(expected.iter()) {
            println!("{:?} <----> {:?}", expected, &parse(test_str).unwrap());
            assert_eq!(&parse(test_str).unwrap(), expected);
        }
    }

    #[test]
    fn test_infers_first_coordinate_is_lat() {
        let test_data = ["59°12'7.7\" -02°15'39.6\"", "59°12'7.7\", -02°15'39.6\""];

        let expected = Coordinate {
            lat: 59.0 + 12.0 / 60.0 + 7.7 / 3600.0,
            lng: -1.0 * (2.0 + 15.0 / 60.0 + 39.6 / 3600.0),
        };

        for test_str in test_data.iter() {
            assert_eq!(parse(test_str).unwrap(), expected);
        }
    }

    #[test]
    fn test_fails_for_invalid_data() {
        assert!(parse("Not DMS string").is_err());
    }

    #[test]
    fn test_decimal_degrees_parsed_correctly() {
        let test_data = ["51.5, -0.126", "51.5,-0.126", "51.5 -0.126"];

        let expected = Coordinate {
            lat: 51.5,
            lng: -0.126,
        };

        for test_str in test_data.iter() {
            assert_eq!(parse(test_str).unwrap(), expected);
        }
    }

    #[test]
    fn test_dms_with_separators_and_spaces() {
        let test_data = [
            "59° 12' 7.7\" N 02° 15' 39.6\" W",
            "59º 12' 7.7\" N 02º 15' 39.6\" W",
            "59 12' 7.7''N 02 15' 39.6''W",
        ];

        let expected = Coordinate {
            lat: 59.0 + 12.0 / 60.0 + 7.7 / 3600.0,
            lng: -1.0 * (2.0 + 15.0 / 60.0 + 39.6 / 3600.0),
        };

        for test_str in test_data.iter() {
            assert_eq!(parse(test_str).unwrap(), expected);
        }
    }
}
