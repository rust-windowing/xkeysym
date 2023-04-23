// SPDX-License-Identifier: BSL-1.0
//               Copyright John Nunley, 2022.
// Distributed under the Boost Software License, Version 1.0.
//       (See accompanying file LICENSE or copy at
//         https://www.boost.org/LICENSE_1_0.txt)

//! A generator that takes the system keysyms and compiles them into
//! a list of Rust constants.
//!
//! For some reason, the official source of truth for X11 keysyms are
//! the "keysymdef" files distributed in the X11 package. Parsing
//! these files typically involves using regexes to extract the values
//! and converting to the more important values. Why this source
//! isn't in a more conventionally parsed format, like JSON or even
//! XML, is a mystery.
//!
//! I challenged myself to write a parser that preforms most of its
//! processing in a single iterator combinator. I partially succeeded,
//! but overall I lean a little too much on the last `for_each()`.

use anyhow::Result;
use regex::Regex;
use std::{
    env,
    fmt::Write as _,
    fs,
    io::{prelude::*, BufReader, BufWriter},
    path::Path,
};

fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    // get the list of files to process
    let prefix = Path::new("/usr/include/X11/");
    let files = [
        "keysymdef.h",
        "XF86keysym.h",
        "Sunkeysym.h",
        "DECkeysym.h",
        "HPkeysym.h",
    ];

    // open the output file
    let outpath = env::args_os().nth(1).expect("output file name");
    let mut outfile = BufWriter::new(fs::File::create(outpath)?);

    writeln!(
        outfile,
        "
// This file was automatically generated using keysym-generator.

//               Copyright John Nunley, 2022.
// Distributed under the Boost Software License, Version 1.0.
//       (See accompanying file LICENSE or copy at
//         https://www.boost.org/LICENSE_1_0.txt)

use super::Keysym;

/// A list of keyboard symbols.
pub mod key {{
    use super::Keysym;
"
    )?;

    // The matcher for dumping the keysym's name.
    let mut keysym_dump = "
/// Get the name of a keysym.
/// 
/// The output of this function is not stable and is intended for debugging purposes.
#[allow(unreachable_patterns)]
pub const fn name(keysym: Keysym) -> Option<&'static str> {
    match keysym {
"
    .to_string();

    // we're looking for lines of the following form:
    // #define {some prefix}XK_{some key name} {some key code}
    // the keycode may be wrapped in _EVDEVK(*), wchich means we have to
    // add to it
    //
    // the keycode will always be in hexadecimal (0x2F) form
    let line_matcher = Regex::new(r#"^#define\s+([a-zA-Z0-9_]*XK_[A-Za-z0-9_]+)\s+(.*)"#)?;
    let evdevk = Regex::new(r#"_EVDEVK\(0x(.*)\)"#)?;
    let hex_matcher = Regex::new(r#"0x([0-9a-fA-F]+)"#)?;

    // open the file and process the lines
    files
        .iter()
        .map(|fname| prefix.join(fname))
        .inspect(|path| tracing::info!("Opening {:?}", path))
        .map(fs::File::open)
        .filter_map(|res| match res {
            Ok(f) => Some(BufReader::new(f)),
            Err(e) => {
                tracing::error!("Unable to open file: {:?}", e);
                None
            }
        })
        .flat_map(|file| file.lines())
        .filter_map(|line| match line {
            Ok(line) => Some(line),
            Err(e) => {
                tracing::error!("Unable to read line: {:?}", e);
                None
            }
        })
        .filter_map(|line| {
            let captures = line_matcher.captures(&line)?;
            match (captures.get(1), captures.get(2)) {
                (Some(name), Some(value)) => {
                    Some((name.as_str().to_string(), value.as_str().to_string()))
                }
                _ => None,
            }
        })
        .try_for_each(|(name, value)| {
            // if the value is wrapped in _EVDEVK(*), unwrap it
            let (is_evdevk, hex_value) = if let Some(captures) = evdevk.captures(&value) {
                (true, captures.get(1))
            } else {
                (
                    false,
                    hex_matcher.captures(&value).and_then(|caps| caps.get(1)),
                )
            };

            if let Some(hex_value) = hex_value {
                let hex_value = hex_value.as_str();

                // compute the value of the keysym
                let mut hex_value = u64::from_str_radix(hex_value, 16)?;
                if is_evdevk {
                    hex_value += 0x10081000;
                }

                // there is a duplicate symbol somewhere in here
                if name.contains("Ydiaeresis") && hex_value == 0x100000ee {
                    return Ok(());
                }

                // split apart the symbol at the end
                let mut parts = name.split("XK_");
                let prefix = parts.next().unwrap();
                let symbol = parts.next().unwrap();

                let needs_underscore = symbol.starts_with(|c: char| c.is_ascii_digit());

                let keysym_name = format!(
                    "{}{}{}",
                    heck::AsShoutySnakeCase(prefix),
                    if prefix.is_empty() && !needs_underscore {
                        ""
                    } else {
                        "_"
                    },
                    &symbol
                );

                writeln!(outfile, "    #[doc(alias = \"{}\")]", &name)?;

                // write out an entry for it
                writeln!(
                    outfile,
                    "    pub const {}: Keysym = {:#x};",
                    &keysym_name, hex_value
                )?;

                // Write a match entry for it.
                writeln!(
                    keysym_dump,
                    "        key::{} => Some(\"{}\"),",
                    &keysym_name, &name
                )
                .unwrap();
            }

            anyhow::Ok(())
        })?;

    writeln!(outfile, "}}")?;

    // Write out the keysym dump.
    keysym_dump.push_str(
        "
        _ => None,
    }
}",
    );

    writeln!(outfile, "{keysym_dump}",)?;

    Ok(())
}
