use std::{
    f64::consts::{E, PI},
    fmt::Debug,
    io::{stdin, Read},
    mem::transmute,
    process::exit,
    str::FromStr,
};

use operation::operation;
use spfunc::gamma::gamma;

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

struct NewStack {
    mode: Mode,
    raw: Vec<u64>,
}

impl Debug for NewStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl NewStack {
    fn mode(&self) -> Mode {
        self.mode
    }
    fn set_mode(&mut self, mode: Mode, cast: Cast) {
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

    fn push(&mut self, n: u64) {
        self.raw.push(n)
    }

    fn pop(&mut self) -> u64 {
        let x = self
            .raw
            .pop()
            .expect("unreachable, the length should always be >= 1");
        if self.len() == 0 {
            self.push(0) // u64 and f64 zero have the same bit representation
        }
        x
    }

    fn popf(&mut self) -> f64 {
        f64::from_bits(self.pop())
    }

    fn len(&self) -> usize {
        self.raw.len()
    }

    fn swap(&mut self) {
        let last = self.len() - 1;
        self.raw.swap(last, last.saturating_sub(1))
    }

    fn peek(&mut self, from_top: usize) -> &mut u64 {
        let len = self.raw.len();
        &mut self.raw[len - 1 - from_top]
    }

    fn peekf(&mut self, from_top: usize) -> &mut f64 {
        let len = self.raw.len();
        let ptr = &mut self.raw[len - 1 - from_top];
        // the original pointer is constructed safely,
        // hence this transmute is sound
        unsafe { transmute::<&mut u64, &mut f64>(ptr) }
    }

    fn set(&mut self, n: u64) {
        *self.peek(0) = n;
    }

    fn setf(&mut self, x: f64) {
        *self.peekf(0) = x;
    }

    fn unary_int(&mut self, func: impl Fn(u64) -> u64) {
        let x = *self.peek(0);
        self.set(func(x))
    }

    fn unary_float(&mut self, func: impl Fn(f64) -> f64) {
        let x = *self.peekf(0);
        self.setf(func(x))
    }

    fn binary_int(&mut self, func: impl Fn(u64, u64) -> u64) -> Result<(), String> {
        if self.len() >= 2 {
            let y = self.pop();
            let x = *self.peek(0);
            self.set(func(x, y));
            Ok(())
        } else {
            Err("Not enough values on the stack".to_string())
        }
    }

    fn binary_float(&mut self, func: impl Fn(f64, f64) -> f64) -> Result<(), String> {
        if self.len() >= 2 {
            let y = self.popf();
            let x = *self.peekf(0);
            self.setf(func(x, y));
            Ok(())
        } else {
            Err("Not enough values on the stack".to_string())
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Cast {
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
enum Mode {
    Basic,
    Scientific,
    Programmer,
}

#[derive(Debug)]
struct State {
    decimal_digits: usize,
    stack: NewStack,
    input: InputState,
    angle: Angle,
    memory: f64,
    base: Base,
}

impl State {
    fn handle_nans(&mut self) -> Result<(), String> {
        let top = *self.stack.peekf(0);
        if top.is_nan() {
            Err("Operation produced NaN result".to_string())
        } else if top.is_infinite() {
            Err("Operation produced infinite result".to_string())
        } else {
            Ok(())
        }
    }
    fn process_digit(&mut self, digit: u8) -> Result<(), String> {
        match self.input {
            InputState::Empty | InputState::Done => {
                self.input = InputState::Integral;
                match self.stack.mode() {
                    Mode::Basic | Mode::Scientific => *self.stack.peekf(0) = digit as f64,
                    Mode::Programmer => *self.stack.peek(0) = digit as u64,
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

#[derive(Copy, Clone)]
#[operation]
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

#[derive(Copy, Clone)]
#[operation]
enum ByteOp {
    ZeroZero = "00",
    FF = "ff",
}

impl ByteOp {
    fn classify(self) -> u8 {
        match self {
            ByteOp::ZeroZero => 0,
            ByteOp::FF => 15,
        }
    }
}

#[derive(Copy, Clone)]
#[operation]
enum NumericInputOp {
    Digit(DigitOp),
    Byte(ByteOp),
    DecimalPeriod = ".",
    DecimalComma = ",",
}

fn check_base(digit: u8, min_base: Base, mode: Mode, base: Base) -> Result<(), String> {
    if let Mode::Basic | Mode::Scientific = mode {
        if let Base::Hexadecimal = min_base {
            return Err("Can't do hex input outside of programmer mode".to_string());
        }
    } else if base < min_base {
        return Err(format!("Base setting sucks for the given digit '{digit}'"));
    }
    Ok(())
}

impl NumericInputOp {
    fn process_digit(self, state: &mut State) -> Result<(), String> {
        match self {
            NumericInputOp::Digit(op) => {
                let (digit, min_base) = op.classify();
                let mode = state.stack.mode();
                let base = state.base;
                check_base(digit, min_base, mode, base)?;
                state.process_digit(digit)
            }
            NumericInputOp::Byte(op) => {
                let digit = op.classify();
                let mode = state.stack.mode();
                let base = state.base;
                check_base(digit, Base::Hexadecimal, mode, base)?;
                state.process_digit(digit)?;
                state.process_digit(digit)
            }
            NumericInputOp::DecimalComma | NumericInputOp::DecimalPeriod => {
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
                        InputState::Decimal => {
                            return Err("Already have a decimal point".to_string())
                        }
                    },
                    Mode::Programmer => {
                        return Err("Decimals invalid in programmer mode".to_string())
                    }
                }
                Ok(())
            }
        }
    }
}

#[derive(Copy, Clone)]
#[operation]
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

#[derive(Copy, Clone)]
#[operation]
enum RpnOp {
    Swap = "x<->y",
    RotateDown = "Rv",
    RotateUp = "R^",
    Drop = "drop",
    Enter = "enter",
}

impl RpnOp {
    fn act(self, stack: &mut NewStack) {
        match self {
            RpnOp::Swap => stack.swap(),
            RpnOp::RotateDown => stack.raw.rotate_right(1),
            RpnOp::RotateUp => stack.raw.rotate_left(1),
            RpnOp::Drop => {
                stack.pop();
            }
            RpnOp::Enter => {
                let x = *stack.peek(0);
                stack.push(x);
            }
        }
    }
}

#[derive(Copy, Clone)]
#[operation]
enum MemoryOp {
    Recall = "mr",
    Add = "m+",
    Subtract = "m-",
    Clear = "mc",
}

impl MemoryOp {
    fn act(self, stack: &mut NewStack, mem: &mut f64) {
        match self {
            MemoryOp::Recall => *stack.peekf(0) = *mem,
            MemoryOp::Add => *mem += *stack.peekf(0),
            MemoryOp::Subtract => *mem -= *stack.peekf(0),
            MemoryOp::Clear => *mem = 0.0,
        }
    }
}

#[derive(Copy, Clone)]
#[operation]
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

#[derive(Copy, Clone)]
#[operation]
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

#[derive(Copy, Clone)]
#[operation]
enum NumericUnaryOp {
    Negate = "+/-",
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

#[derive(Copy, Clone)]
#[operation]
enum ScientificConstOp {
    Rand = "Rand",
    E = "E",
    Pi = "pi",
}

impl ScientificConstOp {
    fn eval(self) -> f64 {
        match self {
            ScientificConstOp::Rand => rand::random(),
            ScientificConstOp::E => E,
            ScientificConstOp::Pi => PI,
        }
    }
}

#[derive(Copy, Clone)]
#[operation]
enum TrigOp {
    #[arg = |x| x.sin()]
    Sin = "sin",
    #[arg = |x| x.cos()]
    Cos = "cos",
    #[arg = |x| x.tan()]
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

#[derive(Copy, Clone)]
#[operation]
enum InverseTrigOp {
    Arcsin = "sin^-1",
    Arccos = "cos^-1",
    Arctan = "tan^-1",
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
#[derive(Copy, Clone)]
#[operation]
enum OtherScientificUnaryOp {
    Exp10 = "10^x",
    Log10 = "log10",
    Exp = "e^x",
    Ln = "ln",
    Exp2 = "2^x",
    Log2 = "log2",
    Square = "x^2",
    Cube = "x^3",
    Sqrt = "2vx",
    Cbrt = "3vx",
    Inverse = "1/x",
    Factorial = "x!",
    Sinh = "sinh",
    Cosh = "cosh",
    Tanh = "tanh",
    Arcsinh = "sinh^-1",
    Arccosh = "cosh^-1",
    Arctanh = "tanh^-1",
}

impl OtherScientificUnaryOp {
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
            OtherScientificUnaryOp::Factorial => gamma(x + 1.0),
            OtherScientificUnaryOp::Sinh => x.sinh(),
            OtherScientificUnaryOp::Cosh => x.cosh(),
            OtherScientificUnaryOp::Tanh => x.tanh(),
            OtherScientificUnaryOp::Arcsinh => x.asinh(),
            OtherScientificUnaryOp::Arccosh => x.acosh(),
            OtherScientificUnaryOp::Arctanh => x.atanh(),
        }
    }
}

#[derive(Copy, Clone)]
#[operation]
enum ScientificUnaryOp {
    Trig(TrigOp),
    InverseTrig(InverseTrigOp),
    Other(OtherScientificUnaryOp),
}

impl ScientificUnaryOp {
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

#[derive(Copy, Clone)]
#[operation]
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

#[derive(Copy, Clone)]
#[operation]
enum UniversalBinaryOp {
    Plus = "+",
    Minus = "-",
    Times = "*",
    Divide = "/",
}

impl UniversalBinaryOp {
    fn eval_int(self, x: u64, y: u64) -> u64 {
        match self {
            UniversalBinaryOp::Plus => x.wrapping_add(y),
            UniversalBinaryOp::Minus => x.wrapping_sub(y),
            UniversalBinaryOp::Times => x.wrapping_mul(y),
            // division by zero is defined as returning zero in programmer mode
            UniversalBinaryOp::Divide => x.checked_div(y).unwrap_or(0),
        }
    }

    fn eval_float(self, x: f64, y: f64) -> f64 {
        match self {
            UniversalBinaryOp::Plus => x + y,
            UniversalBinaryOp::Minus => x - y,
            UniversalBinaryOp::Times => x * y,
            UniversalBinaryOp::Divide => x / y,
        }
    }
}

#[derive(Copy, Clone)]
#[operation]
enum ScientificBinaryOp {
    XToY = "x^y",
    YToX = "y^x",
    LogYX = "logy",
    YthRootX = "yvx",
}

impl ScientificBinaryOp {
    fn eval(self, x: f64, y: f64) -> f64 {
        match self {
            ScientificBinaryOp::XToY => {
                // calculator.app specifies 0^0 to be nan
                if x == 0.0 && y == 0.0 {
                    f64::NAN
                } else {
                    x.powf(y)
                }
            }
            ScientificBinaryOp::YToX => {
                if x == 0.0 && y == 0.0 {
                    f64::NAN
                } else {
                    y.powf(x)
                }
            }
            ScientificBinaryOp::LogYX => x.log(y),
            ScientificBinaryOp::YthRootX => x.powf(1.0 / y),
        }
    }
}

#[derive(Copy, Clone)]
#[operation]
enum ProgrammerBinaryOp {
    And = "AND",
    Or = "OR",
    Nor = "NOR",
    Xor = "XOR",
    XLeftShiftY = "X<<Y",
    XRightShiftY = "X>>Y",
}

impl ProgrammerBinaryOp {
    fn eval(self, x: u64, y: u64) -> u64 {
        match self {
            ProgrammerBinaryOp::And => x & y,
            ProgrammerBinaryOp::Or => x | y,
            ProgrammerBinaryOp::Nor => !(x | y),
            ProgrammerBinaryOp::Xor => x ^ y,
            ProgrammerBinaryOp::XLeftShiftY => x << y,
            ProgrammerBinaryOp::XRightShiftY => x >> y,
        }
    }
}

#[derive(Copy, Clone)]
#[operation]
enum ManipulatorOp {
    Mode(ModeOp),
    Rpn(RpnOp),
    Memory(MemoryOp),
    Angle(AngleOp),
    Base(BaseOp),
    ScientificConst(ScientificConstOp),
    BasicUnary(NumericUnaryOp),
    ScientificUnary(ScientificUnaryOp),
    ProgrammerUnary(ProgrammerUnaryOp),
    UniversalBinary(UniversalBinaryOp),
    ScientificBinary(ScientificBinaryOp),
    EE = "EE",
    ProgrammerBinary(ProgrammerBinaryOp),
}

impl ManipulatorOp {
    // todo errors?
    fn act(self, state: &mut State) -> Result<(), String> {
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
                        Err("memory cell is NaN or infinite".to_string())
                    }
                } else {
                    Err("memory is only available in scientific mode idiot".to_string())
                }
            }
            ManipulatorOp::Angle(op) => {
                if state.angle != op.angle() {
                    state.angle = op.angle();
                    Ok(())
                } else {
                    Err("Angle can't be set to what it already is".to_string())
                }
            }
            ManipulatorOp::Base(op) => {
                if let Mode::Programmer = state.stack.mode() {
                    state.base = op.base();
                    Ok(())
                } else {
                    Err("Can't set base outside programmer mode".to_string())
                }
            }
            ManipulatorOp::BasicUnary(op) => {
                if let Mode::Scientific | Mode::Basic = state.stack.mode() {
                    state.stack.unary_float(|x| op.eval(x));
                    state.handle_nans()?;
                    Ok(())
                } else {
                    Err("Basic unary operation not available in current mode".to_string())
                }
            }
            ManipulatorOp::ScientificConst(op) => {
                if let Mode::Scientific = state.stack.mode() {
                    state.stack.unary_float(|_| op.eval());
                    Ok(())
                } else {
                    Err("Scientific constant not available in current mode".to_string())
                }
            }
            ManipulatorOp::ScientificUnary(op) => {
                if let Mode::Scientific = state.stack.mode() {
                    state.stack.unary_float(|x| op.eval(x, state.angle));
                    state.handle_nans()?;
                    Ok(())
                } else {
                    Err("Scientific unary not available in current mode".to_string())
                }
            }
            ManipulatorOp::ProgrammerUnary(op) => {
                if let Mode::Programmer = state.stack.mode() {
                    state.stack.unary_int(|x| op.eval(x));
                    // all programmer functions are total :)
                    Ok(())
                } else {
                    Err("Programmer unary not available in current mode".to_string())
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
                    state.stack.binary_float(|x, y| op.eval(x, y))?;
                    state.handle_nans()
                } else {
                    Err("Scientific binny not available in current mode".to_string())
                }
            }
            ManipulatorOp::ProgrammerBinary(op) => {
                if let Mode::Programmer = state.stack.mode() {
                    // no nan checks required
                    state.stack.binary_int(|x, y| op.eval(x, y))?;
                    state.handle_nans()
                } else {
                    Err("Prog binary not available in current mode".to_string())
                }
            }
            ManipulatorOp::EE => todo!(),
        }
    }
}

#[derive(Copy, Clone)]
#[operation]
enum Op {
    NumericInput(NumericInputOp),
    Manipulator(ManipulatorOp),
}

impl Op {
    // todo errors?
    fn act(self, state: &mut State) -> Result<(), String> {
        match self {
            Op::NumericInput(op) => op.process_digit(state), // todo something about nans
            Op::Manipulator(op) => {
                state.input = InputState::Done;
                op.act(state)
            }
        }
    }
}

fn main() {
    let mut state = State {
        stack: NewStack {
            mode: Mode::Basic,
            raw: vec![0],
        },
        input: InputState::Empty,
        decimal_digits: 0,
        angle: Angle::Degrees,
        memory: 0.0,
        base: Base::Hexadecimal,
    };

    let mut program = String::new();
    if let Err(e) = stdin().read_to_string(&mut program) {
        panic!("Error reading program: {e}\n")
    }

    for word in program.split_ascii_whitespace() {
        if let Err(e) = Op::from_str(word)
            .map_err(|_| format!("Unknown word {word}"))
            .and_then(|op| op.act(&mut state))
        {
            // `word` is always a valid slice within `program` so safety invariants are held
            let offset = unsafe { word.as_ptr().offset_from(program.as_ptr()) } as usize;
            let line_number = program
                .as_bytes()
                .iter()
                .take(offset)
                .filter(|&&c| c == b'\n')
                .count()
                + 1;
            let line = program.lines().nth(line_number - 1).unwrap();
            let col_start = unsafe { line.as_ptr().offset_from(program.as_ptr()) } as usize;
            let col_offset = offset - col_start;
            let col_offset_len = col_offset + word.len();

            let pos = format!("{line_number}:{col_offset}");
            let fake_pos: String = (0..pos.len()).map(|_| ' ').collect();

            let pointer_line: String = (0..line.len())
                .map(|i| {
                    if col_offset <= i && i < col_offset_len {
                        '^'
                    } else {
                        ' '
                    }
                })
                .collect();

            eprintln!("In state: {state:?}");
            eprintln!("{pos} | {line}");
            eprintln!("{fake_pos} | {pointer_line}");
            eprintln!("{fake_pos} | Error: {e}");
            exit(1);
        }
    }

    dbg!(state);
}
