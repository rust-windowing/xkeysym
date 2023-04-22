// SPDX-License-Identifier: BSL-1.0
//               Copyright John Nunley, 2022.
// Distributed under the Boost Software License, Version 1.0.
//       (See accompanying file LICENSE or copy at
//         https://www.boost.org/LICENSE_1_0.txt)

//! Keyboard symbols for X11.

#![no_std]
#![allow(non_upper_case_globals)]
#![forbid(unsafe_code, rust_2018_idioms)]

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

/// The type of a keyboard code.
pub type KeyCode = u8;

/// The type of a keyboard symbol.
pub type Keysym = u32;

/// The "empty" keyboard symbol.
pub const NO_SYMBOL: Keysym = 0;

/// Get the keyboard symbol from a keyboard code and its column.
///
/// `min_keycode` can be retrieved from the X11 setup, and `keysyms_per_keycode` and `keysyms` can be
/// retrieved from the X11 server through the `GetKeyboardMapping` request.
pub fn keysym(
    keycode: KeyCode,
    mut column: u8,
    min_keycode: KeyCode,
    keysyms_per_keycode: u8,
    keysyms: &[Keysym],
) -> Option<Keysym> {
    if column >= keysyms_per_keycode && column > 3 {
        return None;
    }

    // Get the keysyms to consider.
    let start = (keycode - min_keycode) as usize * keysyms_per_keycode as usize;
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
                if keysyms[per - 1] != NO_SYMBOL {
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
        if per <= alt_column || keysyms[alt_column] == NO_SYMBOL {
            // Convert to upper/lower case.
            let (upper, lower) = convert_case(*keysyms.get(column as usize & !1)?);
            return Some(if column & 1 == 0 { upper } else { lower });
        }
    }

    // Helps us lower the MSRV.
    #[allow(clippy::map_clone)]
    keysyms.get(column as usize).map(|&keysym| keysym)
}

/// Translate a keyboard symbol to its approximate ASCII character.
///
/// This translation does not involve XKB in any way, and is intended to act
/// as a fallback for when XKB is not available. This function explicitly
/// does not support non-Latin alphabets, and is intended to be used as a
/// fallback for when XKB is not available. Real world use cases should use
/// `libxkbcommon` instead.
pub fn key_char(keysym: Keysym, has_control_key: bool) -> Option<char> {
    // Tell if this fits as a valid ASCII char.
    let high_bytes = keysym >> 8;
    if high_bytes != 0 && high_bytes != 0xFF {
        return None;
    }

    if !matches!(keysym,
        KEY_BackSpace..=KEY_Clear
        | KEY_Return | KEY_Escape | KEY_KP_Space
        | KEY_KP_Tab | KEY_KP_Enter | KEY_KP_Multiply..=KEY_KP_9
        | KEY_KP_Equal | KEY_Delete
    ) {
        return None;
    }

    // Convert to ASCII by converting the low byte.
    let mut ascii_key = match (keysym, high_bytes) {
        (KEY_KP_Space, _) => b' ',
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

/// Tell whether a keysym is a keypad key.
pub const fn is_keypad_key(keysym: Keysym) -> bool {
    matches!(keysym, KEY_KP_Space..=KEY_KP_Equal)
}

/// Tell whether a keysym is a private keypad key.
pub const fn is_private_keypad_key(keysym: Keysym) -> bool {
    matches!(keysym, 0x11000000..=0x1100FFFF)
}

/// Tell whether a keysym is a cursor key.
pub const fn is_cursor_key(keysym: Keysym) -> bool {
    matches!(keysym, KEY_Home..=KEY_Select)
}

/// Tell whether a keysym is a PF key.
pub const fn is_pf_key(keysym: Keysym) -> bool {
    matches!(keysym, KEY_KP_F1..=KEY_KP_F4)
}

/// Tell whether a keysym is a function key.
pub const fn is_function_key(keysym: Keysym) -> bool {
    matches!(keysym, KEY_F1..=KEY_F35)
}

/// Tell whether a key is a miscellaneous function key.
pub const fn is_misc_function_key(keysym: Keysym) -> bool {
    matches!(keysym, KEY_Select..=KEY_Break)
}

/// Tell whether a key is a modifier key.
pub const fn is_modifier_key(keysym: Keysym) -> bool {
    matches!(
        keysym,
        KEY_Shift_L..=KEY_Hyper_R
         | KEY_ISO_Lock..=KEY_ISO_Level5_Lock
         | KEY_Mode_switch
         | KEY_Num_Lock
    )
}

/// Convert a keysym to its uppercase/lowercase equivalents.
const fn convert_case(keysym: Keysym) -> (Keysym, Keysym) {
    // by default, they're both the regular keysym
    let (mut upper, mut lower) = (keysym, keysym);

    // tell which language it belongs to
    #[allow(non_upper_case_globals)]
    match keysym {
        KEY_A..=KEY_Z => lower += KEY_a - KEY_A,
        KEY_a..=KEY_z => upper -= KEY_a - KEY_A,
        KEY_Agrave..=KEY_Odiaeresis => lower += KEY_agrave - KEY_Agrave,
        KEY_agrave..=KEY_odiaeresis => upper -= KEY_agrave - KEY_Agrave,
        KEY_Ooblique..=KEY_Thorn => lower += KEY_oslash - KEY_Ooblique,
        KEY_oslash..=KEY_thorn => upper -= KEY_oslash - KEY_Ooblique,
        KEY_Aogonek => lower = KEY_aogonek,
        KEY_aogonek => upper = KEY_Aogonek,
        KEY_Lstroke..=KEY_Sacute => lower += KEY_lstroke - KEY_Lstroke,
        KEY_lstroke..=KEY_sacute => upper -= KEY_lstroke - KEY_Lstroke,
        KEY_Scaron..=KEY_Zacute => lower += KEY_scaron - KEY_Scaron,
        KEY_scaron..=KEY_zacute => upper -= KEY_scaron - KEY_Scaron,
        KEY_Zcaron..=KEY_Zabovedot => lower += KEY_zcaron - KEY_Zcaron,
        KEY_zcaron..=KEY_zabovedot => upper -= KEY_zcaron - KEY_Zcaron,
        KEY_Racute..=KEY_Tcedilla => lower += KEY_racute - KEY_Racute,
        KEY_racute..=KEY_tcedilla => upper -= KEY_racute - KEY_Racute,
        KEY_Hstroke..=KEY_Hcircumflex => lower += KEY_hstroke - KEY_Hstroke,
        KEY_hstroke..=KEY_hcircumflex => upper -= KEY_hstroke - KEY_Hstroke,
        KEY_Gbreve..=KEY_Jcircumflex => lower += KEY_gbreve - KEY_Gbreve,
        KEY_gbreve..=KEY_jcircumflex => upper -= KEY_gbreve - KEY_Gbreve,
        KEY_Cabovedot..=KEY_Scircumflex => lower += KEY_cabovedot - KEY_Cabovedot,
        KEY_cabovedot..=KEY_scircumflex => upper -= KEY_cabovedot - KEY_Cabovedot,
        KEY_Rcedilla..=KEY_Tslash => lower += KEY_rcedilla - KEY_Rcedilla,
        KEY_rcedilla..=KEY_tslash => upper -= KEY_rcedilla - KEY_Rcedilla,
        KEY_ENG => lower = KEY_eng,
        KEY_eng => upper = KEY_ENG,
        KEY_Amacron..=KEY_Umacron => lower += KEY_amacron - KEY_Amacron,
        KEY_amacron..=KEY_umacron => upper -= KEY_amacron - KEY_Amacron,
        KEY_Serbian_DJE..=KEY_Serbian_DZE => lower -= KEY_Serbian_DJE - KEY_Serbian_dje,
        KEY_Serbian_dje..=KEY_Serbian_dze => upper += KEY_Serbian_DJE - KEY_Serbian_dje,
        KEY_Cyrillic_YU..=KEY_Cyrillic_HARDSIGN => lower -= KEY_Cyrillic_YU - KEY_Cyrillic_yu,
        KEY_Cyrillic_yu..=KEY_Cyrillic_hardsign => upper += KEY_Cyrillic_YU - KEY_Cyrillic_yu,
        KEY_Greek_ALPHAaccent..=KEY_Greek_OMEGAaccent => {
            lower += KEY_Greek_alphaaccent - KEY_Greek_ALPHAaccent
        }
        KEY_Greek_alphaaccent..=KEY_Greek_omegaaccent
            if !matches!(
                keysym,
                KEY_Greek_iotaaccentdieresis | KEY_Greek_upsilonaccentdieresis
            ) =>
        {
            upper -= KEY_Greek_alphaaccent - KEY_Greek_ALPHAaccent
        }
        KEY_Greek_ALPHA..=KEY_Greek_OMEGA => lower += KEY_Greek_alpha - KEY_Greek_ALPHA,
        KEY_Greek_alpha..=KEY_Greek_omega if !matches!(keysym, KEY_Greek_finalsmallsigma) => {
            upper -= KEY_Greek_alpha - KEY_Greek_ALPHA
        }
        KEY_Armenian_AYB..=KEY_Armenian_fe => {
            lower |= 1;
            upper &= !1;
        }
        _ => {}
    }

    (upper, lower)
}
