use std::{
    f64::consts::{E, PI},
    fmt::{self, Debug, Display},
    mem::transmute,
    str::FromStr,
};

use operation::operation;
use rand::random;
use unicode_width::UnicodeWidthStr;

/// Gamma function adapted from statrs 0.16.0: https://docs.rs/statrs/latest/statrs/index.html
///
/// MIT License
///
/// Copyright (c) 2016 Michael Ma
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
/// associated documentation files (the "Software"), to deal in the Software without restriction,
/// including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
/// and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
/// subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in all copies or substantial
/// portions of the Software.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
/// LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
/// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
/// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
/// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
mod gamma_fn {
    use std::f64::consts::{E, PI};

    /// Constant value for `2 * sqrt(e / pi)`
    const TWO_SQRT_E_OVER_PI: f64 = 1.860_382_734_205_265_7;

    /// Auxiliary variable when evaluating the `gamma_ln` function
    const GAMMA_R: f64 = 10.900511;

    /// Polynomial coefficients for approximating the `gamma_ln` function
    const GAMMA_DK: &[f64] = &[
        2.485_740_891_387_535_5e-5,
        1.051_423_785_817_219_7,
        -3.456_870_972_220_162_5,
        4.512_277_094_668_948,
        -2.982_852_253_235_766_4,
        1.056_397_115_771_267,
        -1.954_287_731_916_458_7e-1,
        1.709_705_434_044_412e-2,
        -5.719_261_174_043_057e-4,
        4.633_994_733_599_057e-6,
        -2.719_949_084_886_077_2e-9,
    ];

    /// Computes the gamma function with an accuracy
    /// of 16 floating point digits. The implementation
    /// is derived from "An Analysis of the Lanczos Gamma Approximation",
    /// Glendon Ralph Pugh, 2004 p. 116
    pub fn gamma(x: f64) -> f64 {
        if x.is_finite() && x.round() == x && x >= 1.0 {
            // integer, just do a looping factorial
            (1..x as u64).product::<u64>() as f64
        } else if x < 0.5 {
            let s = GAMMA_DK
                .iter()
                .enumerate()
                .skip(1)
                .fold(GAMMA_DK[0], |s, t| s + t.1 / (t.0 as f64 - x));

            PI / ((PI * x).sin() * s * TWO_SQRT_E_OVER_PI * ((0.5 - x + GAMMA_R) / E).powf(0.5 - x))
        } else {
            let s = GAMMA_DK
                .iter()
                .enumerate()
                .skip(1)
                .fold(GAMMA_DK[0], |s, t| s + t.1 / (x + t.0 as f64 - 1.0));

            s * TWO_SQRT_E_OVER_PI * ((x - 0.5 + GAMMA_R) / E).powf(x - 0.5)
        }
    }
}

enum Error {
    NotEnoughStack { expected: usize, got: usize },
    InvalidResult { nan: bool },
    BadDigitBase { expected: Base },
    BadMode { required: Mode },
    BadModeEither { either: Mode, or: Mode },
    DuplicateDecimalPoint,
    InvalidDecimalPoint,
    InvalidMemory { nan: bool },
    BadAngleOnlyToggle,
    BadPalette { required: Palette },
    BadCodepoint,
    UnknownWord,
    Quit,
}

impl Error {
    fn message(&self) -> String {
        match self {
            Error::NotEnoughStack { expected, got } => {
                format!("Not enough values on the stack - expected {expected}, got {got}")
            }
            Error::InvalidResult { nan } => {
                if *nan {
                    "The operation returned a NaN value".to_string()
                } else {
                    "The operation returned an infinite value".to_string()
                }
            }
            Error::BadDigitBase { expected } => {
                format!("This digit can only be used when the base is set to {expected}")
            }
            Error::BadMode { required } => {
                format!("This operation is only available in {required} mode")
            }
            Error::BadModeEither { either, or } => {
                format!("This operation is only available in either {either} mode or {or} mode")
            }
            Error::DuplicateDecimalPoint => "The number already has a decimal point".to_string(),
            Error::InvalidDecimalPoint => {
                "Decimal points can only be used for floating point numbers".to_string()
            }
            Error::InvalidMemory { nan } => {
                if *nan {
                    "The memory cell contains a NaN value".to_string()
                } else {
                    "The memory cell contains an infinite value".to_string()
                }
            }
            Error::BadAngleOnlyToggle => {
                "The angle cannot be set to what it already is".to_string()
            }
            Error::BadPalette { required } => {
                format!("This operation is only available with the {required} palette")
            }
            Error::BadCodepoint => "The input integer is not a valid Unicode codepoint".to_string(),
            Error::UnknownWord => "Unknown word".to_string(),
            Error::Quit => "Quit".to_string(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Angle {
    Degrees,
    Radians,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
// Ord instance used to determine if the current base is "sufficient" to input digits of a given base
enum Base {
    Octal,
    Decimal,
    Hexadecimal,
}
impl Base {
    fn value(self) -> u64 {
        match self {
            Base::Octal => 8,
            Base::Decimal => 10,
            Base::Hexadecimal => 16,
        }
    }
}
impl Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Base::Octal => "octal",
            Base::Decimal => "decimal",
            Base::Hexadecimal => "hexadecimal",
        })
    }
}

#[derive(Clone)]
pub struct Stack {
    mode: Mode,
    raw: Vec<u64>,
}

impl Debug for Stack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Mode::Programmer = self.mode {
            f.debug_struct("NewStack")
                .field("mode", &self.mode)
                .field("raw", &self.raw)
                .finish()
        } else {
            f.debug_struct("NewStack")
                .field("mode", &self.mode)
                // uhh this is sound, right?
                .field("raw", unsafe { &transmute::<&[u64], &[f64]>(&self.raw) })
                .finish()
        }
    }
}

impl Stack {
    pub fn mode(&self) -> Mode {
        self.mode
    }
    pub fn raw(&self) -> &[u64] {
        &self.raw
    }
    pub fn peek(&self) -> Result<f64, u64> {
        match self.mode {
            Mode::Basic | Mode::Scientific => Ok(f64::from_bits(self.raw[self.len() - 1])),
            Mode::Programmer => Err(self.raw[self.len() - 1]),
        }
    }
    pub fn set_mode(&mut self, mode: Mode, cast: Cast) {
        match (self.mode, mode) {
            (Mode::Basic | Mode::Scientific, Mode::Programmer) => {
                if let Cast::Numeric = cast {
                    for n in &mut self.raw {
                        *n = f64::from_bits(*n) as u64
                    }
                }
            }
            (Mode::Programmer, Mode::Basic | Mode::Scientific) => {
                if let Cast::Numeric = cast {
                    for n in &mut self.raw {
                        *n = (*n as f64).to_bits()
                    }
                }
            }
            _ => (), // no transformation necessary
        }
        self.mode = mode;
    }

    fn push_int(&mut self, n: u64) {
        self.raw.push(n)
    }

    fn pop_int(&mut self) -> u64 {
        let x = self
            .raw
            .pop()
            .expect("unreachable, the length should always be >= 1");
        if self.len() == 0 {
            self.push_int(0) // u64 and f64 zero have the same bit representation
        }
        x
    }

    fn pop_float(&mut self) -> f64 {
        f64::from_bits(self.pop_int())
    }

    fn len(&self) -> usize {
        self.raw.len()
    }

    fn swap(&mut self) {
        let last = self.len() - 1;
        self.raw.swap(last, last.saturating_sub(1))
    }

    fn get_int(&mut self, from_top: usize) -> &mut u64 {
        let len = self.raw.len();
        &mut self.raw[len - 1 - from_top]
    }

    fn get_float(&mut self, from_top: usize) -> &mut f64 {
        let len = self.raw.len();
        let ptr = &mut self.raw[len - 1 - from_top];
        // the original pointer is constructed safely,
        // hence this transmute is sound
        unsafe { transmute::<&mut u64, &mut f64>(ptr) }
    }

    fn const_int(&mut self, n: u64) {
        *self.get_int(0) = n;
    }

    fn const_float(&mut self, x: f64) {
        *self.get_float(0) = x;
    }

    fn unary_int(&mut self, func: impl Fn(u64) -> u64) {
        let x = *self.get_int(0);
        self.const_int(func(x))
    }

    fn unary_float(&mut self, func: impl Fn(f64) -> f64) {
        let x = *self.get_float(0);
        self.const_float(func(x))
    }

    fn binary_int(&mut self, func: impl Fn(u64, u64) -> Option<u64>) -> Result<(), Error> {
        if self.len() >= 2 {
            let y = self.pop_int();
            let x = *self.get_int(0);
            if let Some(result) = func(x, y) {
                self.const_int(result);
            }
            Ok(())
        } else {
            Err(Error::NotEnoughStack {
                expected: 2,
                got: self.len(),
            })
        }
    }

    fn binary_float(&mut self, func: impl Fn(f64, f64) -> f64) -> Result<(), Error> {
        if self.len() >= 2 {
            let x = self.pop_float();
            let y = *self.get_float(0);
            self.const_float(func(x, y));
            Ok(())
        } else {
            Err(Error::NotEnoughStack {
                expected: 2,
                got: self.len(),
            })
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Cast {
    Numeric,
    Bitwise,
}

#[derive(Clone, Copy, Debug)]
enum InputState {
    Empty,
    Integral,
    Decimal,
    Done,
}

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    Basic,
    Scientific,
    Programmer,
}
impl Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Mode::Basic => "basic",
            Mode::Scientific => "scientific",
            Mode::Programmer => "programmer",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Palette {
    First,
    Second,
}
impl Display for Palette {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Palette::First => "first (default)",
            Palette::Second => "second (2nd)",
        })
    }
}

#[derive(Debug, Clone)]
pub struct State {
    decimal_digits: usize,
    pub stack: Stack,
    input: InputState,
    angle: Angle,
    palette: Palette,
    memory: f64,
    base: Base,
}

impl State {
    fn new(raw_stack: Vec<u64>) -> Self {
        Self {
            stack: Stack {
                mode: Mode::Basic,
                raw: raw_stack,
            },
            input: InputState::Empty,
            decimal_digits: 0,
            angle: Angle::Degrees,
            palette: Palette::First,
            memory: 0.0,
            base: Base::Hexadecimal,
        }
    }

    fn handle_nans(&mut self) -> Result<(), Error> {
        let top = *self.stack.get_float(0);
        if top.is_nan() {
            Err(Error::InvalidResult { nan: true })
        } else if top.is_infinite() {
            Err(Error::InvalidResult { nan: false })
        } else {
            Ok(())
        }
    }
    fn process_digit(&mut self, digit: u8) -> Result<(), Error> {
        match self.input {
            InputState::Empty | InputState::Done => {
                self.input = InputState::Integral;
                match self.stack.mode() {
                    Mode::Basic | Mode::Scientific => *self.stack.get_float(0) = digit as f64,
                    Mode::Programmer => *self.stack.get_int(0) = digit as u64,
                }
                Ok(())
            }
            InputState::Integral => {
                match self.stack.mode() {
                    Mode::Basic | Mode::Scientific => {
                        self.stack.unary_float(|x| x * 10.0 + digit as f64)
                    }
                    Mode::Programmer => self
                        .stack
                        .unary_int(|x| x * self.base.value() + digit as u64),
                }
                Ok(())
            }
            InputState::Decimal => {
                self.decimal_digits += 1;
                // we can assume that the mode is basic / scientific
                self.stack.unary_float(|x| {
                    x + 10.0f64.powi(-(self.decimal_digits as i32)) * digit as f64
                });
                Ok(())
            }
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum DigitOp {
    Zero = "0",
    One = "1",
    Two = "2",
    Three = "3",
    Four = "4",
    Five = "5",
    Six = "6",
    Seven = "7",
    Eight = "8",
    Nine = "9",
    A = "a",
    B = "b",
    C = "c",
    D = "d",
    E = "e",
    F = "f",
}

impl DigitOp {
    fn classify(self) -> (u8, Base) {
        match self {
            DigitOp::Zero => (0, Base::Octal),
            DigitOp::One => (1, Base::Octal),
            DigitOp::Two => (2, Base::Octal),
            DigitOp::Three => (3, Base::Octal),
            DigitOp::Four => (4, Base::Octal),
            DigitOp::Five => (5, Base::Octal),
            DigitOp::Six => (6, Base::Octal),
            DigitOp::Seven => (7, Base::Octal),
            DigitOp::Eight => (8, Base::Decimal),
            DigitOp::Nine => (9, Base::Decimal),
            DigitOp::A => (10, Base::Hexadecimal),
            DigitOp::B => (11, Base::Hexadecimal),
            DigitOp::C => (12, Base::Hexadecimal),
            DigitOp::D => (13, Base::Hexadecimal),
            DigitOp::E => (14, Base::Hexadecimal),
            DigitOp::F => (15, Base::Hexadecimal),
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum NumericInputOp {
    Digit(DigitOp),
    ZeroZero = "00",
    FF = "ff",
    Decimal = "." | ",",
}

fn check_base(min_base: Base, mode: Mode, base: Base) -> Result<(), Error> {
    if let Mode::Basic | Mode::Scientific = mode {
        if let Base::Hexadecimal = min_base {
            return Err(Error::BadDigitBase {
                expected: Base::Hexadecimal,
            });
        }
    } else if base < min_base {
        return Err(Error::BadDigitBase { expected: min_base });
    }
    Ok(())
}

impl NumericInputOp {
    fn process_digit(self, state: &mut State) -> Result<(), Error> {
        match self {
            NumericInputOp::Digit(op) => {
                let (digit, min_base) = op.classify();
                let mode = state.stack.mode();
                let base = state.base;
                check_base(min_base, mode, base)?;
                state.process_digit(digit)?;
                state.handle_nans()
            }
            NumericInputOp::ZeroZero => {
                if let Mode::Programmer = state.stack.mode() {
                    state.process_digit(0)?;
                    state.process_digit(0)
                } else {
                    Err(Error::BadMode {
                        required: Mode::Programmer,
                    })
                }
            }
            NumericInputOp::FF => {
                let mode = state.stack.mode();
                let base = state.base;
                check_base(Base::Hexadecimal, mode, base)?;
                state.process_digit(15)?;
                state.process_digit(15)
            }
            NumericInputOp::Decimal => {
                match state.stack.mode() {
                    Mode::Basic | Mode::Scientific => match state.input {
                        InputState::Done | InputState::Empty => {
                            state.input = InputState::Decimal;
                            state.decimal_digits = 0
                        }
                        InputState::Integral => {
                            state.input = InputState::Decimal;
                            state.decimal_digits = 0
                        }
                        InputState::Decimal => return Err(Error::DuplicateDecimalPoint),
                    },
                    Mode::Programmer => return Err(Error::InvalidDecimalPoint),
                }
                Ok(())
            }
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum ModeOp {
    Basic = "Basic",
    Scientific = "Scientific",
    Programmer = "Programmer",
    ProgrammerPrime = "Programmer'",
}

impl ModeOp {
    fn transformation(self) -> (Mode, Cast) {
        match self {
            ModeOp::Basic => (Mode::Basic, Cast::Numeric),
            ModeOp::Scientific => (Mode::Scientific, Cast::Numeric),
            ModeOp::Programmer => (Mode::Programmer, Cast::Numeric),
            ModeOp::ProgrammerPrime => (Mode::Programmer, Cast::Bitwise),
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum RpnOp {
    Swap = "x↔︎y" | "x<->y",
    RotateDown = "R↓" | "Rv",
    RotateUp = "R↑" | "R^",
    Drop = "drop",
    Enter = "enter",
}

impl RpnOp {
    fn act(self, stack: &mut Stack) {
        match self {
            RpnOp::Swap => stack.swap(),
            RpnOp::RotateDown => stack.raw.rotate_right(1),
            RpnOp::RotateUp => stack.raw.rotate_left(1),
            RpnOp::Drop => {
                stack.pop_int();
            }
            RpnOp::Enter => {
                let x = *stack.get_int(0);
                stack.push_int(x);
            }
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum MemoryOp {
    Recall = "mr",
    Add = "m+",
    Subtract = "m-",
    Clear = "mc",
}

impl MemoryOp {
    fn act(self, stack: &mut Stack, mem: &mut f64) {
        match self {
            MemoryOp::Recall => *stack.get_float(0) = *mem,
            MemoryOp::Add => *mem += *stack.get_float(0),
            MemoryOp::Subtract => *mem -= *stack.get_float(0),
            MemoryOp::Clear => *mem = 0.0,
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum AngleOp {
    Rad = "Rad",
    Deg = "Deg",
}

impl AngleOp {
    fn angle(self) -> Angle {
        match self {
            AngleOp::Rad => Angle::Radians,
            AngleOp::Deg => Angle::Degrees,
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum BaseOp {
    Octal = "[8]",
    Decimal = "[10]",
    Hexadecimal = "[16]",
}

impl BaseOp {
    fn base(self) -> Base {
        match self {
            BaseOp::Octal => Base::Octal,
            BaseOp::Decimal => Base::Decimal,
            BaseOp::Hexadecimal => Base::Hexadecimal,
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum NumericUnaryOp {
    Negate = "⁺∕₋" | "+/-",
    Percent = "%",
}

impl NumericUnaryOp {
    fn eval(self, x: f64) -> f64 {
        match self {
            NumericUnaryOp::Negate => -x,
            NumericUnaryOp::Percent => x / 100.0,
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum ScientificConstOp {
    Rand = "Rand",
    E = "E",
    Pi = "π" | "pi",
}

impl ScientificConstOp {
    fn eval(self) -> f64 {
        match self {
            ScientificConstOp::Rand => random(),
            ScientificConstOp::E => E,
            ScientificConstOp::Pi => PI,
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum TrigOp {
    Sin = "sin",
    Cos = "cos",
    Tan = "tan",
}

impl TrigOp {
    fn eval(self, x: f64) -> f64 {
        match self {
            TrigOp::Sin => x.sin(),
            TrigOp::Cos => x.cos(),
            TrigOp::Tan => x.tan(),
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum InverseTrigOp {
    Arcsin = "sin⁻¹" | "sin^-1",
    Arccos = "cos⁻¹" | "cos^-1",
    Arctan = "tan⁻¹" | "tan^-1",
}

impl InverseTrigOp {
    fn eval(self, x: f64) -> f64 {
        match self {
            InverseTrigOp::Arcsin => x.asin(),
            InverseTrigOp::Arccos => x.acos(),
            InverseTrigOp::Arctan => x.atan(),
        }
    }
}
#[operation]
#[derive(Copy, Clone)]
enum OtherScientificUnaryOp {
    Exp10 = "10ˣ" | "10^x",
    Log10 = "log⏨" | "log10",
    Exp = "eˣ" | "e^x",
    Ln = "ln",
    Exp2 = "2ˣ" | "2^x",
    Log2 = "log₂" | "log2",
    Square = "x²" | "x^2",
    Cube = "x³" | "x^3",
    Sqrt = "²√x" | "2Vx",
    Cbrt = "³√x" | "3Vx",
    Inverse = "¹∕ₓ" | "1/x",
    Factorial = "x!",
    Sinh = "sinh",
    Cosh = "cosh",
    Tanh = "tanh",
    Arcsinh = "sinh⁻¹" | "sinh^-1",
    Arccosh = "cosh⁻¹" | "cosh^-1",
    Arctanh = "tanh⁻¹" | "tanh^-1",
}

impl OtherScientificUnaryOp {
    fn palette(self) -> Palette {
        if matches!(
            self,
            OtherScientificUnaryOp::Exp10
                | OtherScientificUnaryOp::Log10
                | OtherScientificUnaryOp::Exp
                | OtherScientificUnaryOp::Ln
                | OtherScientificUnaryOp::Sqrt
                | OtherScientificUnaryOp::Cbrt
                | OtherScientificUnaryOp::Inverse
                | OtherScientificUnaryOp::Factorial
        ) {
            Palette::First
        } else {
            Palette::Second
        }
    }
    fn eval(self, x: f64) -> f64 {
        match self {
            OtherScientificUnaryOp::Exp10 => 10.0f64.powf(x),
            OtherScientificUnaryOp::Log10 => x.log10(),
            OtherScientificUnaryOp::Exp => x.exp(),
            OtherScientificUnaryOp::Ln => x.ln(),
            OtherScientificUnaryOp::Exp2 => x.exp2(),
            OtherScientificUnaryOp::Log2 => x.log2(),
            OtherScientificUnaryOp::Square => x.powi(2),
            OtherScientificUnaryOp::Cube => x.powi(3),
            OtherScientificUnaryOp::Sqrt => x.sqrt(),
            OtherScientificUnaryOp::Cbrt => x.cbrt(),
            OtherScientificUnaryOp::Inverse => 1.0 / x,
            OtherScientificUnaryOp::Factorial => gamma_fn::gamma(x + 1.0),
            OtherScientificUnaryOp::Sinh => x.sinh(),
            OtherScientificUnaryOp::Cosh => x.cosh(),
            OtherScientificUnaryOp::Tanh => x.tanh(),
            OtherScientificUnaryOp::Arcsinh => x.asinh(),
            OtherScientificUnaryOp::Arccosh => x.acosh(),
            OtherScientificUnaryOp::Arctanh => x.atanh(),
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum ScientificUnaryOp {
    Trig(TrigOp),
    InverseTrig(InverseTrigOp),
    Other(OtherScientificUnaryOp),
}

impl ScientificUnaryOp {
    fn palette(self) -> Palette {
        match self {
            ScientificUnaryOp::Trig(_) => Palette::First,
            ScientificUnaryOp::InverseTrig(_) => Palette::Second,
            ScientificUnaryOp::Other(op) => op.palette(),
        }
    }
    fn eval(&self, x: f64, angle: Angle) -> f64 {
        match self {
            ScientificUnaryOp::Trig(trig) => {
                if let Angle::Degrees = angle {
                    trig.eval(x.to_radians())
                } else {
                    trig.eval(x)
                }
            }
            ScientificUnaryOp::InverseTrig(inv_trig) => {
                if let Angle::Degrees = angle {
                    inv_trig.eval(x).to_degrees()
                } else {
                    inv_trig.eval(x)
                }
            }
            ScientificUnaryOp::Other(other) => other.eval(x),
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum ProgrammerUnaryOp {
    RotateLeft = "RoL",
    RotateRight = "RoR",
    NudgeLeft = "<<",
    NudgeRight = ">>",
    TwosComplement = "2's",
    OnesComplement = "1's",
    ByteFlip = "byte-flip",
    WordFlip = "word-flip",
}

impl ProgrammerUnaryOp {
    fn eval(self, x: u64) -> u64 {
        match self {
            ProgrammerUnaryOp::RotateLeft => x.rotate_left(1),
            ProgrammerUnaryOp::RotateRight => x.rotate_right(1),
            ProgrammerUnaryOp::NudgeLeft => x << 1,
            ProgrammerUnaryOp::NudgeRight => x >> 1,
            ProgrammerUnaryOp::TwosComplement => x.wrapping_neg(),
            ProgrammerUnaryOp::OnesComplement => !x,
            ProgrammerUnaryOp::ByteFlip => x.swap_bytes(),
            ProgrammerUnaryOp::WordFlip => {
                (x & 0xffff) << 24
                    | (x & 0xffff0000) << 8
                    | (x & 0xffff00000000) >> 8
                    | (x & 0xffff000000000000) >> 24
            }
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum UniversalBinaryOp {
    Plus = "+",
    Minus = "-",
    Times = "×" | "*",
    Divide = "÷" | "/",
}

impl UniversalBinaryOp {
    fn eval_int(self, x: u64, y: u64) -> Option<u64> {
        match self {
            UniversalBinaryOp::Plus => Some(y.wrapping_add(x)),
            UniversalBinaryOp::Minus => Some(y.wrapping_sub(x)),
            UniversalBinaryOp::Times => Some(y.wrapping_mul(x)),
            // division by zero is defined as a noop in programmer mode
            // that makes some of this infrastructure annoying to deal with due to the
            // extra option involved
            UniversalBinaryOp::Divide => y.checked_div(x),
        }
    }

    fn eval_float(self, x: f64, y: f64) -> f64 {
        match self {
            UniversalBinaryOp::Plus => y + x,
            UniversalBinaryOp::Minus => y - x,
            UniversalBinaryOp::Times => y * x,
            UniversalBinaryOp::Divide => y / x,
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum ScientificBinaryOp {
    XToY = "xʸ" | "x^y",
    YToX = "yˣ" | "y^x",
    LogYX = "logᵧ" | "logy",
    XthRootY = "ˣ√y" | "xVy",
    EE = "EE",
}

impl ScientificBinaryOp {
    fn palette(self) -> Palette {
        if matches!(
            self,
            ScientificBinaryOp::YToX | ScientificBinaryOp::EE | ScientificBinaryOp::XthRootY
        ) {
            Palette::First
        } else {
            Palette::Second
        }
    }
    fn eval(self, x: f64, y: f64) -> f64 {
        match self {
            ScientificBinaryOp::XToY => {
                // calculator.app specifies 0^0 to be nan
                if y == 0.0 && x == 0.0 {
                    f64::NAN
                } else {
                    x.powf(y)
                }
            }
            ScientificBinaryOp::YToX => {
                if y == 0.0 && x == 0.0 {
                    f64::NAN
                } else {
                    y.powf(x)
                }
            }
            ScientificBinaryOp::LogYX => x.log(y),
            ScientificBinaryOp::XthRootY => {
                if y == 0.0 && x == 0.0 {
                    f64::NAN
                } else {
                    y.powf(1.0 / x)
                }
            }
            ScientificBinaryOp::EE => y * 10.0f64.powf(x),
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum ProgrammerBinaryOp {
    And = "AND",
    Or = "OR",
    Nor = "NOR",
    Xor = "XOR",
    XLeftShiftY = "X<<Y",
    XRightShiftY = "X>>Y",
}

impl ProgrammerBinaryOp {
    fn eval(self, x: u64, y: u64) -> Option<u64> {
        Some(match self {
            ProgrammerBinaryOp::And => x & y,
            ProgrammerBinaryOp::Or => x | y,
            ProgrammerBinaryOp::Nor => !(x | y),
            ProgrammerBinaryOp::Xor => x ^ y,
            ProgrammerBinaryOp::XLeftShiftY => x << y,
            ProgrammerBinaryOp::XRightShiftY => x >> y,
        })
    }
}

trait AffineUnit {
    fn to_self(&self) -> (f64, f64) {
        (self.to_self_proportional(), 0.0)
    }
    fn to_self_proportional(&self) -> f64 {
        self.to_self().0
    }
}

#[operation]
#[derive(Clone, Copy)]
enum AreaConversion {
    Acres = "Acres",
    Ares = "Ares",
    Decares = "Decares",
    Hectares = "Hectares",
    SquareCentimeters = "Square-Centimeters",
    SquareFeet = "Square-Feet",
    SquareKilometers = "Square-Kilometers",
    SquareMeters = "Square-Meters",
    SquareMiles = "Square-Miles",
    SquareMillimeters = "Square-Millimeters",
    SquareYard = "Square-Yards",
}

impl AffineUnit for AreaConversion {
    fn to_self_proportional(&self) -> f64 {
        match self {
            AreaConversion::Acres => 2.471_053_81e-4,
            AreaConversion::Ares => 1e-2,
            AreaConversion::Decares => 1e-3,
            AreaConversion::Hectares => 1e-4,
            AreaConversion::SquareCentimeters => 1e4,
            AreaConversion::SquareFeet => 10.763_911,
            AreaConversion::SquareKilometers => 1e-6,
            AreaConversion::SquareMeters => 1.0,
            AreaConversion::SquareMiles => 3.861_021_58e-7,
            AreaConversion::SquareMillimeters => 1e6,
            AreaConversion::SquareYard => 1.195_990,
        }
    }
}

#[operation]
#[derive(Clone, Copy)]
enum EnergyOrWorkConversion {
    Btus = "BTUs",
    Calories = "Calories",
    Ergs = "Ergs",
    FootPounds = "Foot-Pounds",
    Joules = "Joules",
    KilogramCalories = "Kilogram-Calories",
    KilogramMeters = "Kilogram-Meters",
    KilowattHours = "Kilowatt-Hours",
    NewtonMeters = "Newton-Meters",
}

impl AffineUnit for EnergyOrWorkConversion {
    fn to_self_proportional(&self) -> f64 {
        match self {
            EnergyOrWorkConversion::Btus => 9.478_2e-4,
            EnergyOrWorkConversion::Calories => 0.2390,
            EnergyOrWorkConversion::Ergs => 1e7,
            EnergyOrWorkConversion::FootPounds => 0.7376,
            EnergyOrWorkConversion::Joules => 1.0,
            EnergyOrWorkConversion::KilogramCalories => 2.388_458_97e-4,
            EnergyOrWorkConversion::KilogramMeters => 0.101971621, // this is weird
            EnergyOrWorkConversion::KilowattHours => 2.7778e-7,
            EnergyOrWorkConversion::NewtonMeters => 1.0,
        }
    }
}

#[operation]
#[derive(Clone, Copy)]
enum LengthConversion {
    Centimeters = "Centimeters",
    Feet = "Feet",
    Inches = "Inches",
    Kilometers = "Kilometers",
    Meters = "Meters",
    Miles = "Miles",
    Millimeters = "Millimeters",
    NauticalMiles = "Nautical-Miles",
    Yards = "Yards",
}

impl AffineUnit for LengthConversion {
    fn to_self_proportional(&self) -> f64 {
        match self {
            LengthConversion::Centimeters => 1e2,
            LengthConversion::Feet => 3.2808,
            LengthConversion::Inches => 39.370,
            LengthConversion::Kilometers => 1e-3,
            LengthConversion::Meters => 1.0,
            LengthConversion::Miles => 6.213_712e-4,
            LengthConversion::Millimeters => 1e3,
            LengthConversion::NauticalMiles => 5.399_6e-4,
            LengthConversion::Yards => 1.0936,
        }
    }
}

#[operation]
#[derive(Clone, Copy)]
enum PowerConversion {
    BtusPerMinute = "BTUs/Minute",
    FootPoundsPerMinute = "Foot-Pounds/Minute",
    FootPoundsPerSecond = "Foot-Pounds/Second",
    Horsepower = "Horsepower",
    Kilowatts = "Kilowatts",
    Watts = "Watts",
}

impl AffineUnit for PowerConversion {
    fn to_self_proportional(&self) -> f64 {
        match self {
            PowerConversion::BtusPerMinute => 5.686_902_721_9e-2,
            PowerConversion::FootPoundsPerMinute => 44.253_729,
            PowerConversion::FootPoundsPerSecond => 0.737_562_149_3,
            PowerConversion::Horsepower => 1.341_022_072e-3,
            PowerConversion::Kilowatts => 1e-3,
            PowerConversion::Watts => 1.0,
        }
    }
}

#[operation]
#[derive(Clone, Copy)]
enum PressureConversion {
    Atmospheres = "Atmospheres",
    Bars = "Bars",
    InchesOfMercury = "Inches-of-Mercury",
    MillimetersOfMercuryTorr = "Millimeters-of-Mercury(Torr)",
    Pascals = "Pascals",
    PoundsPerSquareFoot = "Pounds/Square-Foot",
    PoundsPerSquareInch = "Pounds/Square-Inch",
}

impl AffineUnit for PressureConversion {
    fn to_self_proportional(&self) -> f64 {
        match self {
            PressureConversion::Atmospheres => 9.869_23e-6,
            PressureConversion::Bars => 1e-5,
            PressureConversion::InchesOfMercury => 2.953e-4,
            PressureConversion::MillimetersOfMercuryTorr => 7.500_64e-4,
            PressureConversion::Pascals => 1.0,
            PressureConversion::PoundsPerSquareFoot => 2.088_5e-2,
            PressureConversion::PoundsPerSquareInch => 1.450_4e-4,
        }
    }
}

#[operation]
#[derive(Clone, Copy)]
enum SpeedConversion {
    FeetPerMinute = "Feet/Minute",
    FeetPerSecond = "Feet/Second",
    KilometersPerHour = "Kilometers/Hour",
    KilometersPerMinute = "Kilometers/Minute",
    Knots = "Knots",
    MetersPerSecond = "Meters/Second",
    MilesPerHour = "Miles/Hour",
    MilesPerMinute = "Miles/Minute",
}

impl AffineUnit for SpeedConversion {
    fn to_self_proportional(&self) -> f64 {
        match self {
            SpeedConversion::FeetPerMinute => 1.968_504e2,
            SpeedConversion::FeetPerSecond => 3.280_8,
            SpeedConversion::KilometersPerHour => 3.6,
            SpeedConversion::KilometersPerMinute => 6e-2,
            SpeedConversion::Knots => 1.943_8,
            SpeedConversion::MetersPerSecond => 1.0,
            SpeedConversion::MilesPerHour => 2.236_9,
            SpeedConversion::MilesPerMinute => 3.728_227e-2,
        }
    }
}

#[operation]
#[derive(Clone, Copy)]
enum TemperatureConversion {
    Celsius = "Celsius",
    Fahrenheit = "Fahrenheit",
    Kelvin = "Kelvin",
}

impl AffineUnit for TemperatureConversion {
    fn to_self(&self) -> (f64, f64) {
        match self {
            TemperatureConversion::Celsius => (1.0, -273.15),
            TemperatureConversion::Fahrenheit => (1.8, -459.67),
            TemperatureConversion::Kelvin => (1.0, 0.0),
        }
    }
}

#[operation]
#[derive(Clone, Copy)]
enum TimeConversion {
    Seconds = "Seconds",
    Minutes = "Minutes",
    Hours = "Hours",
    Days = "Days",
    Weeks = "Weeks",
    Years = "Years",
}

impl AffineUnit for TimeConversion {
    fn to_self_proportional(&self) -> f64 {
        match self {
            TimeConversion::Seconds => 1.0,
            TimeConversion::Minutes => 1.0 / 60.0,
            TimeConversion::Hours => 1.0 / 60.0 / 60.0,
            TimeConversion::Days => 1.0 / 60.0 / 60.0 / 24.0,
            TimeConversion::Weeks => 1.0 / 60.0 / 60.0 / 24.0 / 7.0,
            TimeConversion::Years => 1.0 / 60.0 / 60.0 / 24.0 / 365.0,
        }
    }
}

#[operation]
#[derive(Clone, Copy)]
enum VolumeConversion {
    CubicFeet = "Cubic-Feet",
    CubicMeters = "Cubic-Meters",
    FluidDramsUs = "Fluid-Drams(US)",
    FluidOuncesUs = "Fluid-Ounces(US)",
    GallonsUs = "Gallons(US)",
    Liters = "Liters",
    PintsUs = "Pints(US)",
    QuartsUs = "Quarts(US)",
}

impl AffineUnit for VolumeConversion {
    fn to_self_proportional(&self) -> f64 {
        match self {
            VolumeConversion::CubicFeet => 35.3,
            VolumeConversion::CubicMeters => 1.0,
            VolumeConversion::FluidDramsUs => 2.705_121_82e5,
            VolumeConversion::FluidOuncesUs => 3.381_402_27e4,
            VolumeConversion::GallonsUs => 264.0,
            VolumeConversion::Liters => 1e-3,
            VolumeConversion::PintsUs => 1.816e3,
            VolumeConversion::QuartsUs => 9.081e2,
        }
    }
}

#[operation]
#[derive(Clone, Copy)]
enum WeightsAndMassesConversion {
    Drams = "Drams",
    Grams = "Grams",
    Kilograms = "Kilograms",
    LongTons = "Long-Tons",
    Ounces = "Ounces",
    PoundsUs = "Pounds(US)",
    ShortTonsUs = "Short-Tons(US)",
    Slugs = "Slugs",
    Tonnes = "Tonnes",
}

impl AffineUnit for WeightsAndMassesConversion {
    fn to_self_proportional(&self) -> f64 {
        match self {
            WeightsAndMassesConversion::Drams => 564.3834,
            WeightsAndMassesConversion::Grams => 1e3,
            WeightsAndMassesConversion::Kilograms => 1.0,
            WeightsAndMassesConversion::LongTons => 9.842_065e4,
            WeightsAndMassesConversion::Ounces => 35.27396,
            WeightsAndMassesConversion::PoundsUs => 2.204_623,
            WeightsAndMassesConversion::ShortTonsUs => 1.102_311e3,
            WeightsAndMassesConversion::Slugs => 6.85e2,
            WeightsAndMassesConversion::Tonnes => 1e-3,
        }
    }
}

macro_rules! generate_conversions {
    ($(#[$meta:meta])*
    enum $name:ident {
        $($field:ident($from:ty, $to:ty)),*
        $(,)?
    }) => {
        $(#[$meta])*
        enum $name {
            $($field($from, $to),)*
        }

        impl FromStr for $name {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                if let Some((left, right)) = s.split_once('>') {
                    $(if let (Ok(from), Ok(to)) = (
                        <$from>::from_str(left),
                        <$to>::from_str(right),
                    ) {
                        return Ok(Self::$field(from, to));
                    })*
                    _ = left;
                    _ = right;
                    Err(())
                } else {
                    Err(())
                }
            }
        }

        impl ToString for $name {
            fn to_string(&self) -> String {
                match self {
                    $(
                        ConversionOp::$field(from, to) => {
                            format!("{}>{}", from.to_string(), to.to_string())
                        }
                    )*
                }
            }
        }

        impl AffineUnit for ConversionOp {
            fn to_self(&self) -> (f64, f64) {
                let (from_coefficients, to_coefficients) = match self {
                    $(
                        ConversionOp::$field(from, to) => (from.to_self(), to.to_self()),
                    )*
                };

                let (from_a, from_b) = from_coefficients;
                let (to_a, to_b) = to_coefficients;
                (to_a / from_a, to_b - to_a / from_a * from_b)
            }
        }
    };
}

generate_conversions! {
    #[derive(Clone, Copy)]
    enum ConversionOp {
        Area(AreaConversion, AreaConversion),
        EnergyOrWork(EnergyOrWorkConversion, EnergyOrWorkConversion),
        Length(LengthConversion, LengthConversion),
        Power(PowerConversion, PowerConversion),
        Pressure(PressureConversion, PressureConversion),
        Speed(SpeedConversion, SpeedConversion),
        Temperature(TemperatureConversion, TemperatureConversion),
        Time(TimeConversion, TimeConversion),
        Volume(VolumeConversion, VolumeConversion),
        WeightsAndMasses(WeightsAndMassesConversion, WeightsAndMassesConversion),
    }
}

#[operation]
#[derive(Copy, Clone)]
enum ManipulatorOp {
    Mode(ModeOp),
    Rpn(RpnOp),
    Memory(MemoryOp),
    Angle(AngleOp),
    SecondPalette = "2ⁿᵈ" | "2nd",
    Base(BaseOp),
    UniversalBinary(UniversalBinaryOp),
    BasicUnary(NumericUnaryOp),
    ScientificConst(ScientificConstOp),
    ScientificUnary(ScientificUnaryOp),
    ScientificBinary(ScientificBinaryOp),
    ProgrammerUnary(ProgrammerUnaryOp),
    ProgrammerBinary(ProgrammerBinaryOp),
    Conversion(ConversionOp),
}

impl ManipulatorOp {
    fn act(self, state: &mut State) -> Result<(), Error> {
        match self {
            ManipulatorOp::Mode(op) => {
                let (mode, cast) = op.transformation();
                state.stack.set_mode(mode, cast);
                Ok(())
            }
            ManipulatorOp::Rpn(op) => {
                op.act(&mut state.stack);
                Ok(())
            }
            ManipulatorOp::Memory(op) => {
                if let Mode::Scientific = state.stack.mode {
                    op.act(&mut state.stack, &mut state.memory);
                    if state.memory.is_finite() {
                        Ok(())
                    } else {
                        Err(Error::InvalidMemory {
                            nan: state.memory.is_nan(),
                        })
                    }
                } else {
                    Err(Error::BadMode {
                        required: Mode::Scientific,
                    })
                }
            }
            ManipulatorOp::Angle(op) => {
                if state.angle != op.angle() {
                    state.angle = op.angle();
                    Ok(())
                } else {
                    Err(Error::BadAngleOnlyToggle)
                }
            }
            ManipulatorOp::SecondPalette => {
                if let Mode::Scientific = state.stack.mode() {
                    match state.palette {
                        Palette::First => state.palette = Palette::Second,
                        Palette::Second => state.palette = Palette::First,
                    }
                    Ok(())
                } else {
                    Err(Error::BadMode {
                        required: Mode::Scientific,
                    })
                }
            }
            ManipulatorOp::Base(op) => {
                if let Mode::Programmer = state.stack.mode() {
                    state.base = op.base();
                    Ok(())
                } else {
                    Err(Error::BadMode {
                        required: Mode::Programmer,
                    })
                }
            }
            ManipulatorOp::BasicUnary(op) => {
                if let Mode::Scientific | Mode::Basic = state.stack.mode() {
                    state.stack.unary_float(|x| op.eval(x));
                    state.handle_nans()?;
                    Ok(())
                } else {
                    Err(Error::BadModeEither {
                        either: Mode::Basic,
                        or: Mode::Scientific,
                    })
                }
            }
            ManipulatorOp::ScientificConst(op) => {
                if let Mode::Scientific = state.stack.mode() {
                    state.stack.unary_float(|_| op.eval());
                    Ok(())
                } else {
                    Err(Error::BadMode {
                        required: Mode::Scientific,
                    })
                }
            }
            ManipulatorOp::ScientificUnary(op) => {
                if let Mode::Scientific = state.stack.mode() {
                    if state.palette == op.palette() {
                        state.stack.unary_float(|x| op.eval(x, state.angle));
                        state.handle_nans()?;
                        Ok(())
                    } else {
                        Err(Error::BadPalette {
                            required: op.palette(),
                        })
                    }
                } else {
                    Err(Error::BadMode {
                        required: Mode::Scientific,
                    })
                }
            }
            ManipulatorOp::ProgrammerUnary(op) => {
                if let Mode::Programmer = state.stack.mode() {
                    state.stack.unary_int(|x| op.eval(x));
                    // all programmer functions are total :)
                    Ok(())
                } else {
                    Err(Error::BadMode {
                        required: Mode::Programmer,
                    })
                }
            }
            ManipulatorOp::UniversalBinary(op) => {
                if let Mode::Programmer = state.stack.mode() {
                    state.stack.binary_int(|x, y| op.eval_int(x, y))
                } else {
                    state.stack.binary_float(|x, y| op.eval_float(x, y))?;
                    state.handle_nans()
                }
            }
            ManipulatorOp::ScientificBinary(op) => {
                if let Mode::Scientific = state.stack.mode() {
                    if state.palette == op.palette() {
                        state.stack.binary_float(|x, y| op.eval(x, y))?;
                        state.handle_nans()
                    } else {
                        Err(Error::BadPalette {
                            required: op.palette(),
                        })
                    }
                } else {
                    Err(Error::BadMode {
                        required: Mode::Scientific,
                    })
                }
            }
            ManipulatorOp::ProgrammerBinary(op) => {
                if let Mode::Programmer = state.stack.mode() {
                    // no nan checks required
                    state.stack.binary_int(|x, y| op.eval(x, y))?;
                    state.handle_nans()
                } else {
                    Err(Error::BadMode {
                        required: Mode::Programmer,
                    })
                }
            }
            ManipulatorOp::Conversion(op) => {
                if let Mode::Basic | Mode::Scientific = state.stack.mode() {
                    let (scale, offset) = op.to_self();
                    let x = state.stack.get_float(0);
                    *x = *x * scale + offset;
                    Ok(())
                } else {
                    Err(Error::BadModeEither {
                        either: Mode::Basic,
                        or: Mode::Scientific,
                    })
                }
            }
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum OutputOp {
    Ascii = "[ASCII]",
    Unicode = "[Unicode]",
    LargeType = "Large-Type",
}

impl OutputOp {
    fn get_string(&self, state: &mut State) -> Result<String, Error> {
        match self {
            OutputOp::Ascii => {
                let x = *state.stack.get_int(0);
                if let Mode::Programmer = state.stack.mode() {
                    Ok(((x & 0x7f) as u8 as char).to_string())
                } else {
                    Err(Error::BadMode {
                        required: Mode::Programmer,
                    })
                }
            }
            OutputOp::Unicode => {
                let x = *state.stack.get_int(0);
                if let Mode::Programmer = state.stack.mode() {
                    char::from_u32(x as u32 & 0x10ffff)
                        .map(|x| x.to_string())
                        .ok_or(Error::BadCodepoint)
                } else {
                    Err(Error::BadMode {
                        required: Mode::Programmer,
                    })
                }
            }
            OutputOp::LargeType => {
                if let Mode::Programmer = state.stack.mode() {
                    let x = *state.stack.get_int(0);
                    let string = match state.base {
                        Base::Octal => format!("{x:o}\n"),
                        Base::Decimal => format!("{x}\n"),
                        Base::Hexadecimal => format!("{x:x}\n"),
                    };
                    Ok(string)
                } else {
                    let x = *state.stack.get_float(0);
                    Ok(format!("{x}\n"))
                }
            }
        }
    }
}

#[operation]
#[derive(Copy, Clone)]
enum Op {
    NumericInput(NumericInputOp),
    Manipulator(ManipulatorOp),
    Output(OutputOp),
    C = "C",
    AC = "AC",
    Quit = "⌘Q" | "Q",
}

impl Op {
    fn act(self, state: &mut State) -> Result<(), Error> {
        match self {
            Op::NumericInput(op) => op.process_digit(state),
            Op::Manipulator(op) => {
                state.input = InputState::Done;
                op.act(state)
            }
            Op::Output(op) => {
                print!("{}", op.get_string(state)?);
                Ok(())
            }
            Op::C => {
                state.input = InputState::Empty;
                state.decimal_digits = 0;
                *state.stack.get_int(0) = 0;
                Ok(())
            }
            Op::AC => {
                state.stack.raw = vec![0];
                state.decimal_digits = 0;
                state.memory = 0.0;
                state.input = InputState::Empty;
                Ok(())
            }
            Op::Quit => Err(Error::Quit),
        }
    }
}

pub struct ProgramCompletion {
    pub state: State,
    pub code: i32,
    pub message: String,
}

pub fn exec_loop(
    program: &str,
    input: Vec<f64>,
    file: Option<&str>,
    verbose: bool,
) -> ProgramCompletion {
    let integers = input.iter().map(|x| x.to_bits()).collect();
    let raw = if input.is_empty() { vec![0] } else { integers };
    let mut state = State::new(raw);
    loop {
        match tick(program, state, file, verbose) {
            Ok(new_state) => state = new_state,
            Err(e) => break e,
        }
    }
}

pub fn exec_once(
    program: &str,
    input: Vec<f64>,
    file: Option<&str>,
    verbose: bool,
) -> Result<State, ProgramCompletion> {
    let integers = input.iter().map(|x| x.to_bits()).collect();
    let raw = if input.is_empty() { vec![0] } else { integers };
    let state = State::new(raw);
    tick(program, state, file, verbose)
}

pub fn tick(
    program: &str,
    mut state: State,
    file: Option<&str>,
    verbose: bool,
) -> Result<State, ProgramCompletion> {
    for (line_number, line) in program.lines().enumerate() {
        let mut word_end_chars = 0;
        let mut word_end_visual = 0;
        for chunk in line.split_inclusive(&[' ', '\t']) {
            let word = chunk.trim_end_matches(&[' ', '\t']);
            word_end_chars += chunk.chars().count();
            word_end_visual += UnicodeWidthStr::width(chunk);

            if word.is_empty() {
                continue;
            }

            if let Err(e) = Op::from_str(word)
                .map_err(|_| Error::UnknownWord)
                .and_then(|op| op.act(&mut state))
            {
                let msg = e.message();

                let return_code = if let Error::Quit = e { 0 } else { 1 };

                let word_width = UnicodeWidthStr::width(word);

                let word_start_chars = word_end_chars - chunk.chars().count();
                let loc = (line_number + 1).to_string();
                let pad = " ".repeat(loc.len());

                let word_start_visual = word_end_visual - UnicodeWidthStr::width(chunk);
                let pointer_line = " ".repeat(word_start_visual) + &"^".repeat(word_width);

                let error_report = if return_code == 0 {
                    "Quit".to_string()
                } else {
                    format!("Error: {msg}")
                };

                let file_name = file.unwrap_or("(stdin)");

                let state_suffix = if verbose {
                    format!("\n{pad} $ In state: {state:?}")
                } else {
                    String::new()
                };

                let visual_line_number = line_number + 1;
                let visual_word_start_chars = word_start_chars + 1;

                let msg = [
                    format!(
                        "{pad} > {file_name}:{}:{}{state_suffix}",
                        visual_line_number, visual_word_start_chars
                    ),
                    format!("{pad} |"),
                    format!("{loc} | {line}"),
                    format!("{pad} | {pointer_line}"),
                    format!("{pad} * {error_report}"),
                ]
                .join("\n");

                return Err(ProgramCompletion {
                    state,
                    code: return_code,
                    message: msg,
                });
            }
        }
    }
    Ok(state)
}
