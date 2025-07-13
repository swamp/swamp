/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

#[allow(clippy::too_many_lines)]
#[must_use]
pub fn core_text() -> String {
    let text = r"
  /// DO NOT EDIT!

/// # Swamp Core Module
/// Welcome to Swamp! This module provides basic implementations for common types.
/// **Note on Intrinsics:**
/// Some functions in this module, such as `int_rnd` and `float_rnd`, are not normal functions.
/// They are compiler intrinsics that are replaced with optimized code during compilation.

type SparseId = Int

impl Byte {
  fn string(self) -> String {
      byte_to_string(self)
  }
  fn short_string(self) -> String {
    byte_to_string(self)
  }
  fn pretty_string(self) -> String {
    byte_to_string(self)
  }
  fn pretty_string_with_indent(self, indentation: Int) -> String {
    byte_to_string(self)
  }
}

impl Char {
  fn string(self) -> String {
    codepoint_to_string(self)
  }
  fn short_string(self) -> String {
    codepoint_to_string(self)
  }
  fn pretty_string(self) -> String {
    codepoint_to_string(self)
  }
  fn pretty_string_with_indent(self, indentation: Int) -> String {
    codepoint_to_string(self)
  }
}

impl Bool {
  fn string(self) -> String {
      bool_to_string(self)
  }
  fn short_string(self) -> String {
    bool_to_string(self)
  }
  fn pretty_string(self) -> String {
    bool_to_string(self)
  }
  fn pretty_string_with_indent(self, indentation: Int) -> String {
    bool_to_string(self)
  }
}

impl Float {
    fn string(self) -> String {
        float_to_string(self)
    }
    fn short_string(self) -> String {
        float_to_string(self)
    }
    fn short_string(self) -> String {
        float_to_string(self)
    }
    fn pretty_string(self) -> String {
        float_to_string(self)
    }
    fn pretty_string_with_indent(self, indentation: Int) -> String {
        float_to_string(self)
    }

    /// Rounds the Float value down.
    ///
    /// Uses intrinsic function `float_floor` to return the largest integer value not greater than the Float.
    ///
    /// # Returns
    /// An `Int` representing the floor value.
    fn floor(self) -> Int {
        float_floor(self)
    }

    /// Rounds the Float value to the nearest integer.
    ///
    /// Uses intrinsic function `float_round` to round the Float.
    ///
    /// # Returns
    /// An `Int` representing the rounded value.
    fn round(self) -> Int {
        float_round(self)
    }

    /// Computes the square root.
    ///
    /// Uses intrinsic function `float_sqrt` to calculate the square root of the Float value.
    ///
    /// # Returns
    /// A `Float` representing the square root.
    fn sqrt(self) -> Float {
        float_sqrt(self)
    }

    /// Determines the sign of the Float.
    ///
    /// Uses intrinsic function `float_sign` to indicate whether the value is positive or negative.
    ///
    /// # Returns
    /// A `Float` representing the sign.
    fn sign(self) -> Float {
        float_sign(self)
    }

    /// Computes the absolute value.
    ///
    /// Uses intrinsic function `float_abs` to return the absolute (non-negative) value of the Float.
    ///
    /// # Returns
    /// A `Float` with the absolute value.
    fn abs(self) -> Float {
        float_abs(self)
    }

    /// Returns a pseudo-random number between 0.0 and 1.0.
    ///
    /// Uses intrinsic function `float_rnd` to generate the number.
    /// **Note:** This function is by design 100% deterministic and not designed for cryptographic or security-sensitive use.
    ///
    /// # Returns
    /// A `Float` between 0.0 and 1.0.
    fn rnd(self) -> Float {
        float_rnd(self)
    }

    /// Calculates the cosine.
    ///
    /// Uses intrinsic function `float_cos` to compute the cosine of the `Float` value.
    ///
    /// # Returns
    /// A `Float` representing the cosine.
    fn cos(self) -> Float {
        float_cos(self)
    }

    /// Calculates the sine.
    ///
    /// Uses intrinsic function `float_sin` to compute the sine of the Float value.
    ///
    /// # Returns
    /// A `Float` representing the sine.
    fn sin(self) -> Float {
        float_sin(self)
    }

    /// Calculates the arc cosine.
    ///
    /// Uses intrinsic function `float_acos` to compute the arc cosine (inverse cosine) of the Float value.
    ///
    /// # Returns
    /// A `Float` representing the arc cosine.
    fn acos(self) -> Float {
        float_acos(self)
    }

    /// Calculates the arc sine.
    ///
    /// Uses intrinsic function `float_asin` to compute the arc sine (inverse sine) of the Float value.
    ///
    /// # Returns
    /// A `Float` representing the arc sine.
    fn asin(self) -> Float {
        float_asin(self)
    }

    /// Computes the angle from the x-axis.
    ///
    /// # Parameters
    /// - `x`: The second coordinate as a `Float`.
    ///
    /// Uses intrinsic function `float_atan2` to calculate the angle (in radians) from the x-axis to the point `(self, x)`.
    ///
    /// # Returns
    /// A `Float` representing the computed angle.
    fn atan2(self, x: Float) -> Float {
        float_atan2(self, x)
    }

    /// Returns the minimum of two Floats.
    ///
    /// # Parameters
    /// - `x`: Another `Float` to compare.
    ///
    /// Uses intrinsic function `float_min` to return the smaller of the two values.
    ///
    /// # Returns
    /// A `Float` representing the minimum value.
    fn min(self, x: Float) -> Float {
        float_min(self, x)
    }

    /// Returns the maximum of two Floats.
    ///
    /// # Parameters
    /// - `x`: Another `Float` to compare.
    ///
    /// Uses intrinsic function `float_max` to return the larger of the two values.
    ///
    /// # Returns
    /// A `Float` representing the maximum value.
    fn max(self, x: Float) -> Float {
        float_max(self, x)
    }

    /// Clamps the Float within a range.
    ///
    /// # Parameters
    /// - `min`: The minimum allowed value.
    /// - `max`: The maximum allowed value.
    ///
    /// Uses intrinsic function `float_clamp` to restrict the Float to the given range.
    ///
    /// # Returns
    /// A `Float` that is clamped between `min` and `max`.
    fn clamp(self, min: Float, max: Float) -> Float {
        float_clamp(self, min, max)
    }

}

impl Int {
    fn string(self) -> String {
        int_to_string(self)
    }

    fn short_string(self) -> String {
        int_to_string(self)
    }

    fn pretty_string(self) -> String {
        int_to_string(self)
    }

    fn pretty_string_with_indent(self, indentation: Int) -> String {
        int_to_string(self)
    }

    /// Computes the absolute value.
    ///
    /// Uses intrinsic function `int_abs` to return the non-negative value of the Int.
    ///
    /// # Returns
    /// An `Int` with the absolute value.
    fn abs(self) -> Int {
        int_abs(self)
    }

    /// Returns a pseudo-random number between 0 and 32767.
    ///
    /// Uses intrinsic function `int_rnd` to generate the number.
    ///
    /// **Note:** This function is by design 100% deterministic and not designed for cryptographic or security-sensitive use.
    ///
    /// # Returns
    /// An `Int` between 0 and 32767.
    fn rnd(self) -> Int {
        int_rnd(self)
    }

    /// Returns the maximum of two Int values.
    ///
    /// # Parameters
    /// - `x`: Another `Int` to compare.
    ///
    /// Uses intrinsic function `int_max` to return the larger of the two values.
    ///
    /// # Returns
    /// An `Int` representing the maximum value.
    fn max(self, x: Int) -> Int {
        int_max(self, x)
    }

    /// Returns the minimum of two Int values.
    ///
    /// # Parameters
    /// - `x`: Another `Int` to compare.
    ///
    /// Uses intrinsic function `int_min` to return the smaller of the two values.
    ///
    /// # Returns
    /// An `Int` representing the minimum value.
    fn min(self, x: Int) -> Int {
        int_min(self, x)
    }

    fn clamp(self, min: Int, max: Int) -> Int {
        int_clamp(self, min, max)
    }

    /// Converts an Int to a Float.
    ///
    /// Uses intrinsic function `int_to_float` to convert the integer value to its floating-point representation.
    ///
    /// # Returns
    /// A `Float` representing the integer.
    fn to_float(self) -> Float {
        int_to_float(self)
    }
}

impl String {
    fn string(self) -> String {
        string_to_string(self)
    }

    fn short_string(self) -> String {
        self
    }

    fn pretty_string(self) -> String {
        self
    }

    fn pretty_string_with_indent(self, indentation: Int) -> String {
        self
    }

    /// Computes the length of a String.
    ///
    /// # Parameters
    /// - `s`: The `String` whose length is to be determined.
    ///
    /// Uses intrinsic function `string_len` to return the number of characters in the string.
    ///
    /// # Returns
    /// An `Int` representing the length of the string.
    fn len(self) -> Int {
        string_len(self)
    }
}

struct Range {
    start: Int,
    end: Int,
    is_inclusive: Bool,
}

impl Range {
    fn new(start: Int, end: Int, is_inclusive: Bool) -> Range {
        Range {
            start: start,
            end: end,
            is_inclusive: is_inclusive,
        }
    }

    fn iter(self) -> (Int, Int) {
        //range_iter(self)
        (0, 0)
    }
}

/// Immediately terminates program execution with an error message
///
/// # Parameters
/// - `message`: The error message to display
///
/// Uses intrinsic function `runtime_panic` to terminate execution.
fn panic(message: String) {
    runtime_panic(message)
}

/// Asserts that a condition is true, panics with a message if false
///
/// # Parameters
/// - `condition`: The condition to check
/// - `message`: The error message to display if condition is false
fn assert(condition: Bool, message: String) {
    if !condition {
        panic(message)
    }
}


fn halt() {
    runtime_halt()
}

fn step() {
    runtime_step()
}

    ";

    text.to_string()
}
