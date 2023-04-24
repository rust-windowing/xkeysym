// SPDX-License-Identifier: BSL-1.0
//               Copyright John Nunley, 2022.
// Distributed under the Boost Software License, Version 1.0.
//       (See accompanying file LICENSE or copy at
//         https://www.boost.org/LICENSE_1_0.txt)

//! Keyboard symbols for X11.

#![no_std]
#![allow(non_upper_case_globals)]
#![forbid(unsafe_code, rust_2018_idioms)]

use core::fmt;

macro_rules! matches {
    ($expr:expr, $( $pat:pat )|+ $( if $guard: expr )?) => {
        match $expr {
            $( $pat )|+ $( if $guard )? => true,
            _ => false,
        }
    };
}

#[rustfmt::skip]
mod automatically_generated;
pub use automatically_generated::*;

/// The type of a raw keyboard code.
pub type RawKeyCode = u32;

/// The keyboard code, often corresponding to a physical key.
///
/// Keyboard events usually return this type directly, and leave it to be the responsibility of the
/// user to convert it to a keyboard symbol.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[cfg_attr(feature = "bytemuck", derive(bytemuck::Pod, bytemuck::Zeroable))]
#[repr(transparent)]
pub struct KeyCode(RawKeyCode);

impl KeyCode {
    /// Create a new `KeyCode` from a raw keyboard code.
    pub const fn new(raw: RawKeyCode) -> Self {
        Self(raw)
    }

    /// Get the raw keyboard code.
    pub const fn raw(self) -> RawKeyCode {
        self.0
    }
}

impl From<RawKeyCode> for KeyCode {
    fn from(raw: RawKeyCode) -> Self {
        Self::new(raw)
    }
}

impl From<KeyCode> for RawKeyCode {
    fn from(keycode: KeyCode) -> Self {
        keycode.raw()
    }
}

impl From<u8> for KeyCode {
    fn from(raw: u8) -> Self {
        Self::new(raw as RawKeyCode)
    }
}

impl From<KeyCode> for u8 {
    fn from(keycode: KeyCode) -> Self {
        keycode.raw() as u8
    }
}

/// The type of a raw keyboard symbol.
pub type RawKeysym = u32;

/// The keyboard symbol, often corresponding to a character.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[cfg_attr(feature = "bytemuck", derive(bytemuck::Pod, bytemuck::Zeroable))]
#[repr(transparent)]
pub struct Keysym(RawKeysym);

impl fmt::Debug for Keysym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.name() {
            Some(name) => f.write_str(name),
            None => write!(f, "{:#x}", self.0),
        }
    }
}

impl Keysym {
    /// Create a new `Keysym` from a raw keyboard symbol.
    pub const fn new(raw: RawKeysym) -> Self {
        Self(raw)
    }

    /// Get the raw keyboard symbol.
    pub const fn raw(self) -> RawKeysym {
        self.0
    }

    /// Get a string corresponding to the name of this keyboard symbol.
    ///
    /// The output of this function is not stable and is intended for debugging purposes.
    pub const fn name(self) -> Option<&'static str> {
        name(self)
    }

    /// Tell whether a keysym is a keypad key.
    pub const fn is_keypad_key(self) -> bool {
        matches!(self.0, key::KP_Space..=key::KP_Equal)
    }

    /// Tell whether a keysym is a private keypad key.
    pub const fn is_private_keypad_key(self) -> bool {
        matches!(self.0, 0x11000000..=0x1100FFFF)
    }

    /// Tell whether a keysym is a cursor key.
    pub const fn is_cursor_key(self) -> bool {
        matches!(self.0, key::Home..=key::Select)
    }

    /// Tell whether a keysym is a PF key.
    pub const fn is_pf_key(self) -> bool {
        matches!(self.0, key::KP_F1..=key::KP_F4)
    }

    /// Tell whether a keysym is a function key.
    pub const fn is_function_key(self) -> bool {
        matches!(self.0, key::F1..=key::F35)
    }

    /// Tell whether a key is a miscellaneous function key.
    pub const fn is_misc_function_key(self) -> bool {
        matches!(self.0, key::Select..=key::Break)
    }

    /// Tell whether a key is a modifier key.
    pub const fn is_modifier_key(self) -> bool {
        matches!(
            self.0,
            key::Shift_L..=key::Hyper_R
            | key::ISO_Lock..=key::ISO_Level5_Lock
            | key::Mode_switch
            | key::Num_Lock
        )
    }

    /// Translate a keyboard symbol to its approximate ASCII character.
    ///
    /// This translation does not involve XKB in any way, and is intended to act
    /// as a fallback for when XKB is not available. This function explicitly
    /// does not support non-Latin alphabets, and is intended to be used as a
    /// fallback for when XKB is not available. Real world use cases should use
    /// `libxkbcommon` instead.
    pub fn key_char(self, has_control_key: bool) -> Option<char> {
        let keysym = self.0;

        // Tell if this fits as a valid ASCII char.
        let high_bytes = keysym >> 8;
        if high_bytes != 0 && high_bytes != 0xFF {
            return None;
        }

        if !matches!(keysym,
            key::BackSpace..=key::Clear
            | key::Return | key::Escape | key::KP_Space
            | key::KP_Tab | key::KP_Enter | key::KP_Multiply..=key::KP_9
            | key::KP_Equal | key::Delete
        ) {
            return None;
        }

        // Convert to ASCII by converting the low byte.
        let mut ascii_key = match (keysym, high_bytes) {
            (key::KP_Space, _) => b' ',
            (_, 0xFF) => (keysym & 0x7F) as u8,
            _ => keysym as u8,
        };

        // Apply the control key if it makes sense.
        if has_control_key {
            match ascii_key {
                b'@'..=126 | b' ' => {
                    ascii_key &= 0x1F;
                }
                b'2' => {
                    ascii_key = b'\0';
                }
                b'3'..=b'7' => {
                    ascii_key -= b'3' - 27;
                }
                b'8' => {
                    ascii_key = 127;
                }
                b'/' => {
                    ascii_key = b'_' & 0x1F;
                }
                _ => {}
            }
        }

        Some(char::from(ascii_key))
    }
}

/// The "empty" keyboard symbol.
pub const NO_SYMBOL: Keysym = Keysym(0);

/// Get the keyboard symbol from a keyboard code and its column.
///
/// `min_keycode` can be retrieved from the X11 setup, and `keysyms_per_keycode` and `keysyms` can be
/// retrieved from the X11 server through the `GetKeyboardMapping` request.
pub fn keysym(
    keycode: KeyCode,
    mut column: u8,
    min_keycode: KeyCode,
    keysyms_per_keycode: u8,
    keysyms: &[RawKeysym],
) -> Option<Keysym> {
    if column >= keysyms_per_keycode && column > 3 {
        return None;
    }

    // Get the keysyms to consider.
    let start = (keycode.0 - min_keycode.0) as usize * keysyms_per_keycode as usize;
    let end = start + keysyms_per_keycode as usize;
    let keysyms = &keysyms[start..end];

    // See which keysym we should get.
    let mut per = keysyms_per_keycode as usize;
    if column < 4 {
        // If we're going past the traditional upper/lower keys, we need to figure out where
        // our keysym is.
        if column >= 2 {
            // See how many keysyms we actually have in this column.
            loop {
                // There will always be at least one keysym in this column.
                if per <= 1 {
                    break;
                }

                // If the keysym we're looking at isn't NO_SYMBOL, we're done.
                if keysyms[per - 1] != NO_SYMBOL.0 {
                    break;
                }

                // This column isn't as big as `per`, subtract it.
                per -= 1;
            }

            // If this keysym doesn't go past the traditional upper/lower keys, adjust column
            // accordingly.
            if per <= 2 {
                column %= 2;
            }
        }

        // Convert to upper/lower ourselves if the keysym doesn't support it.
        let alt_column = (column | 1) as usize;
        if per <= alt_column || keysyms[alt_column] == NO_SYMBOL.0 {
            // Convert to upper/lower case.
            let (upper, lower) = convert_case(Keysym(*keysyms.get(column as usize & !1)?));
            return Some(if column & 1 == 0 { upper } else { lower });
        }
    }

    // Helps us lower the MSRV.
    #[allow(clippy::map_clone)]
    keysyms.get(column as usize).map(|&keysym| Keysym(keysym))
}

/// Convert a keysym to its uppercase/lowercase equivalents.
const fn convert_case(keysym: Keysym) -> (Keysym, Keysym) {
    // by default, they're both the regular keysym
    let (mut upper, mut lower) = (keysym.0, keysym.0);

    // tell which language it belongs to
    #[allow(non_upper_case_globals)]
    match keysym.0 {
        key::A..=key::Z => lower += key::a - key::A,
        key::a..=key::z => upper -= key::a - key::A,
        key::Agrave..=key::Odiaeresis => lower += key::agrave - key::Agrave,
        key::agrave..=key::odiaeresis => upper -= key::agrave - key::Agrave,
        key::Ooblique..=key::Thorn => lower += key::oslash - key::Ooblique,
        key::oslash..=key::thorn => upper -= key::oslash - key::Ooblique,
        key::Aogonek => lower = key::aogonek,
        key::aogonek => upper = key::Aogonek,
        key::Lstroke..=key::Sacute => lower += key::lstroke - key::Lstroke,
        key::lstroke..=key::sacute => upper -= key::lstroke - key::Lstroke,
        key::Scaron..=key::Zacute => lower += key::scaron - key::Scaron,
        key::scaron..=key::zacute => upper -= key::scaron - key::Scaron,
        key::Zcaron..=key::Zabovedot => lower += key::zcaron - key::Zcaron,
        key::zcaron..=key::zabovedot => upper -= key::zcaron - key::Zcaron,
        key::Racute..=key::Tcedilla => lower += key::racute - key::Racute,
        key::racute..=key::tcedilla => upper -= key::racute - key::Racute,
        key::Hstroke..=key::Hcircumflex => lower += key::hstroke - key::Hstroke,
        key::hstroke..=key::hcircumflex => upper -= key::hstroke - key::Hstroke,
        key::Gbreve..=key::Jcircumflex => lower += key::gbreve - key::Gbreve,
        key::gbreve..=key::jcircumflex => upper -= key::gbreve - key::Gbreve,
        key::Cabovedot..=key::Scircumflex => lower += key::cabovedot - key::Cabovedot,
        key::cabovedot..=key::scircumflex => upper -= key::cabovedot - key::Cabovedot,
        key::Rcedilla..=key::Tslash => lower += key::rcedilla - key::Rcedilla,
        key::rcedilla..=key::tslash => upper -= key::rcedilla - key::Rcedilla,
        key::ENG => lower = key::eng,
        key::eng => upper = key::ENG,
        key::Amacron..=key::Umacron => lower += key::amacron - key::Amacron,
        key::amacron..=key::umacron => upper -= key::amacron - key::Amacron,
        key::Serbian_DJE..=key::Serbian_DZE => lower -= key::Serbian_DJE - key::Serbian_dje,
        key::Serbian_dje..=key::Serbian_dze => upper += key::Serbian_DJE - key::Serbian_dje,
        key::Cyrillic_YU..=key::Cyrillic_HARDSIGN => lower -= key::Cyrillic_YU - key::Cyrillic_yu,
        key::Cyrillic_yu..=key::Cyrillic_hardsign => upper += key::Cyrillic_YU - key::Cyrillic_yu,
        key::Greek_ALPHAaccent..=key::Greek_OMEGAaccent => {
            lower += key::Greek_alphaaccent - key::Greek_ALPHAaccent
        }
        key::Greek_alphaaccent..=key::Greek_omegaaccent
            if !matches!(
                keysym.0,
                key::Greek_iotaaccentdieresis | key::Greek_upsilonaccentdieresis
            ) =>
        {
            upper -= key::Greek_alphaaccent - key::Greek_ALPHAaccent
        }
        key::Greek_ALPHA..=key::Greek_OMEGA => lower += key::Greek_alpha - key::Greek_ALPHA,
        key::Greek_alpha..=key::Greek_omega if !matches!(keysym.0, key::Greek_finalsmallsigma) => {
            upper -= key::Greek_alpha - key::Greek_ALPHA
        }
        key::Armenian_AYB..=key::Armenian_fe => {
            lower |= 1;
            upper &= !1;
        }
        _ => {}
    }

    (Keysym(upper), Keysym(lower))
}
