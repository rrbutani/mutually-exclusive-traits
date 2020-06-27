// #![feature(generic_associated_types)]
#![feature(const_generics)]
#![feature(const_fn)]
#![feature(const_if_match)]
#![feature(specialization)]
// #![feature(negative_impls)]
// #![feature(optin_builtin_traits)]

// #![feature(overlapping_marker_traits)]
#![feature(marker_trait_attr)]
#![allow(incomplete_features)]

use std::fmt::{Arguments, Display};
use std::ops::Add;

use num_traits::{One, Pow, Unsigned, CheckedMul};

#[derive(Debug, Clone)]
pub struct UnOpData {
    op: String,
    arg: Box<Repr>,
}

#[derive(Debug, Clone)]
pub struct BinOpData {
    op: String,
    lhs: Box<Repr>,
    rhs: Box<Repr>,
}

#[derive(Debug, Clone)]
pub struct LitData(String);

pub trait ReprKind {
    type Data;
    fn into_repr(data: Self::Data) -> Repr;
}

macro_rules! kinds {
    ($($kind:ident :: $data_ident:ident: $data:ty => $variant:path),+) => {$(
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct $kind;

        impl ReprKind for $kind {
            type Data = $data;
            fn into_repr(data: Self::Data) -> Repr {
                $variant(data)
            }
        }
    )*};
}

kinds! {
    LitM :: data: LitData => Repr::Lit,
    UnOpM :: data: UnOpData => Repr::UnOp,
    BinOpM :: data: BinOpData => Repr::BinOp
}

#[derive(Debug, Clone)]
pub enum Repr {
    UnOp(UnOpData),
    BinOp(BinOpData),
    Lit(LitData),
}

trait Printable {
    // Args is TODO!
    fn to_repr(&self, args: ()) -> Repr;
}

// trait UnaryOperation {
//     fn arg(&self) -> &impl Printable;
//     fn op(&self) -> &impl Display;
// }

// trait BinaryOperation {
//     fn lhs(&self) -> &impl Printable;
//     fn rhs(&self) -> &impl Printable;
//     fn op(&self) -> &impl Display;
// }

trait UnaryOperation {
    fn arg(&self) -> &dyn Printable;
    fn op(&self) -> &dyn Display;
}

trait BinaryOperation {
    fn lhs(&self) -> &dyn Printable;
    fn rhs(&self) -> &dyn Printable;
    fn op(&self) -> &dyn Display;
}

trait Literal: Display { }

// If const generics were there:
/*
trait ValidOpTraitImpl { type Valid: const bool; }

default impl<T: Literal> ValidOpTraitImpl for T { type Valid = true; }
default impl<T: UnaryOperation> ValidOpTraitImpl for T { type Valid = true; }
default impl<T: BinaryOperation> ValidOpTraitImpl for T { type Valid = true; }

default impl<T: Literal + UnaryOperation> ValidOpTraitImpl for T { type Valid = false; }
default impl<T: UnaryOperation + BinaryOperation> ValidOpTraitImpl for T { type Valid = false; }
default impl<T: BinaryOperation + Literal> ValidOpTraitImpl for T { type Valid = false; }

impl<T: Literal + UnaryOperation + BinaryOperation> ValidOpTraitImpl for T { type Valid = false; }
*/

mod valid {
    pub trait Sealed { }

    pub struct Yes;
    pub struct No;

    impl Sealed for Yes { }
    impl Sealed for No { }
}

/*
trait ValidOpTraitImpl { type Valid: valid::Sealed; }

default impl<T: Literal> ValidOpTraitImpl for T { type Valid = valid::Yes; }
default impl<T: UnaryOperation> ValidOpTraitImpl for T { type Valid = valid::Yes; }
default impl<T: BinaryOperation> ValidOpTraitImpl for T { type Valid = valid::Yes; }

default impl<T: Literal + UnaryOperation> ValidOpTraitImpl for T { type Valid = valid::No; }
default impl<T: UnaryOperation + BinaryOperation> ValidOpTraitImpl for T { type Valid = valid::No; }
default impl<T: BinaryOperation + Literal> ValidOpTraitImpl for T { type Valid = valid::No; }

impl<T: Literal + UnaryOperation + BinaryOperation> ValidOpTraitImpl for T { type Valid = valid::No; }
*/

// impl<T: ValidOpTraitImpl<Valid = valid::Yes> + Literal> Printable for T {
//     fn to_repr(&self, args: ()) -> Repr {
//         LitM::into_repr(LitData(format!("{}", self)))
//     }
// }

// impl<T: ValidOpTraitImpl<Valid = valid::Yes> + UnaryOperation> Printable for T {
//     fn to_repr(&self, args: ()) -> Repr {
//         UnOpM::into_repr(UnOpData {
//             op: format!("{}", self.op()),
//             arg: Box::new(self.arg().to_repr()),
//         })
//     }
// }

// impl<T: ValidOpTraitImpl<Valid = valid::Yes> + BinaryOperation> Printable for T {
//     fn to_repr(&self, args: ()) -> Repr {
//         BinOpM::into_repr(BinOpData {
//             op: format!("{}", self.op()),
//             lhs: Box::new(self.lhs().to_repr()),
//             rgs: Box::new(self.rhs().to_repr()),
//         })
//     }
// }

/*
trait ValidUnary: UnaryOperation { type Valid: valid::Sealed; }
default impl<T: UnaryOperation> ValidUnary for T { type Valid = valid::Yes; }

default impl<T: UnaryOperation + BinaryOperation> ValidUnary for T { type Valid = valid::No; }
default impl<T: UnaryOperation + Literal> ValidUnary for T { type Valid = valid::No; }

default impl<T: UnaryOperation + BinaryOperation + Literal> ValidUnary for T { type Valid = valid::No; }
*/

/*
trait ValidUnary { }

default impl<T: UnaryOperation> ValidUnary for T { }
default impl<T: BinaryOperation> !ValidUnary for T { }
*/

/**** L1 ****/
// Unary or (Lit + Unary)
trait ValidUnaryL1 { type Valid: valid::Sealed; }
default impl<T: UnaryOperation> ValidUnaryL1 for T { type Valid = valid::Yes; }
default impl<T: UnaryOperation + BinaryOperation> ValidUnaryL1 for T { type Valid = valid::No; }
default impl<T: UnaryOperation + BinaryOperation + Literal> ValidUnaryL1 for T { type Valid = valid::No; }

// Binary or (Un + Binary)
trait ValidBinaryL1 { type Valid: valid::Sealed; }
default impl<T: BinaryOperation> ValidBinaryL1 for T { type Valid = valid::Yes; }
default impl<T: BinaryOperation + Literal> ValidBinaryL1 for T { type Valid = valid::No; }
default impl<T: BinaryOperation + Literal + UnaryOperation> ValidBinaryL1 for T { type Valid = valid::No; }

// Literal or (Bin + Literal)
trait ValidLiteralL1 { type Valid: valid::Sealed; }
impl<T: Literal> ValidLiteralL1 for T { default type Valid = valid::Yes; }
impl<T: Literal + UnaryOperation> ValidLiteralL1 for T { default type Valid = valid::No; }
impl<T: Literal + UnaryOperation + BinaryOperation> ValidLiteralL1 for T { default type Valid = valid::No; }


/**** L2 ****/
// Just Unary.
trait ValidUnaryL2 { type Valid: valid::Sealed; }
default impl<T: ValidUnaryL1<Valid = valid::Yes> + UnaryOperation> ValidUnaryL2 for T { type Valid = valid::Yes; }
default impl<T: ValidUnaryL1<Valid = valid::Yes> + UnaryOperation + Literal> ValidUnaryL2 for T { type Valid = valid::No; }

trait ValidUnary: ValidUnaryL2<Valid = valid::Yes> + UnaryOperation { }
impl<T: ValidUnaryL2<Valid = valid::Yes> + UnaryOperation> ValidUnary for T { }

// Just Binary.
trait ValidBinaryL2 { type Valid: valid::Sealed; }
default impl<T: ValidBinaryL1<Valid = valid::Yes> + BinaryOperation> ValidBinaryL2 for T { type Valid = valid::Yes; }
default impl<T: ValidBinaryL1<Valid = valid::Yes> + BinaryOperation + UnaryOperation> ValidBinaryL2 for T { type Valid = valid::No; }

trait ValidBinary: ValidBinaryL2<Valid = valid::Yes> + BinaryOperation { }
impl<T: ValidBinaryL2<Valid = valid::Yes> + BinaryOperation> ValidBinary for T { }

// Just Literal.
trait ValidLiteralL2 { type Valid: valid::Sealed; }
impl<T: ValidLiteralL1<Valid = valid::Yes> + Literal> ValidLiteralL2 for T { default type Valid = valid::Yes; }
impl<T: ValidLiteralL1<Valid = valid::Yes> + Literal + BinaryOperation> ValidLiteralL2 for T { default type Valid = valid::No; }

trait ValidLiteral: ValidLiteralL2<Valid = valid::Yes> + Literal { }
impl<T: ValidLiteralL2<Valid = valid::Yes> + Literal> ValidLiteral for T { }

/**** Fin ****/

// trait One { }
// trait Two { }
// trait Three { }

// struct A;
// struct B;
// struct C;

// impl One for A { }
// impl Two for A { } impl Two for B { }
// impl Three for A { } impl Three for B { } impl Three for C { }

// trait Assoc { type Other; }

/*// Literal | Unary | Binary
trait LitUnBinBound { fn as_bin(&self) -> &dyn BinaryOperation; }
// Literal | Unary
trait LitUnBound: LitUnBinBound { fn as_un(&self) -> &dyn UnaryOperation; }
// Literal
trait LitBound: LitUnBound { fn as_lit(&self) -> &dyn Literal; }
*/

// auto trait One { }

// impl<L: ValidLiteral> !One for L { }
// impl<L: ValidUnary> !One for L { }

trait AsLiteral { fn as_lit(&self) -> &dyn Literal { unreachable!() } }
default impl<T> AsLiteral for T { }
impl<T: ValidLiteral> AsLiteral for T { fn as_lit(&self) -> &dyn Literal { self }}

trait AsUnary { fn as_un(&self) -> &dyn UnaryOperation { unreachable!() } }
default impl<T> AsUnary for T { }
impl<T: ValidUnary> AsUnary for T { fn as_un(&self) -> &dyn UnaryOperation { self }}

trait AsBinary { fn as_bin(&self) -> &dyn BinaryOperation { unreachable!() } }
default impl<T> AsBinary for T { }
impl<T: ValidBinary> AsBinary for T { fn as_bin(&self) -> &dyn BinaryOperation { self }}

// Literal | Unary | Binary
#[marker] trait LitUnBinBound: AsBinary { }
// Literal | Unary
#[marker] trait LitUnBound: AsUnary + LitUnBinBound { }
// Literal
#[marker] trait LitBound: AsLiteral + LitUnBound { }

impl<L: ValidLiteral + AsBinary> LitUnBinBound for L { }
impl<U: ValidUnary + AsBinary> LitUnBinBound for U { }
impl<B: ValidBinary + AsBinary> LitUnBinBound for B { }

impl<L: LitUnBinBound + ValidLiteral + AsUnary> LitUnBound for L { }
impl<U: LitUnBinBound + ValidUnary + AsUnary> LitUnBound for U { }

impl<L: LitUnBound + ValidLiteral + AsLiteral> LitBound for L { }

// a use //

impl<T: LitUnBinBound> Printable for T {
    default fn to_repr(&self, args: ()) -> Repr {
        let bin = self.as_bin();

        BinOpM::into_repr(BinOpData {
            op: format!("{}", bin.op()),
            lhs: Box::new(bin.lhs().to_repr(args)),
            rhs: Box::new(bin.rhs().to_repr(args)),
        })
    }
}

impl<T: LitUnBound> Printable for T {
    default fn to_repr(&self, args: ()) -> Repr {
        let un = self.as_un();

        UnOpM::into_repr(UnOpData {
            op: format!("{}", un.op()),
            arg: Box::new(un.arg().to_repr(args)),
        })
    }
}

impl<T: LitBound> Printable for T {
    fn to_repr(&self, args: ()) -> Repr {
        LitM::into_repr(LitData(format!("{}", self.as_lit())))
    }
}

// fin //

trait Evaluatable<Res>: Printable {
    fn eval(&self) -> Res;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lit<L: Display, const BITS: usize> {
    val: L,
}

impl<L: Display, const BITS: usize> Lit<L, BITS>
where
    L: Unsigned,
    // L: Pow<usize, Output = L>,
    L: Eq + PartialOrd,
    L: One,
    L: Add<L, Output = L>,
    L: Clone,
    L: CheckedMul,
{
    pub fn new(val: L) -> Result<Self, ()> {
        let two: L = L::one() + L::one();
        let limit = num_traits::pow::checked_pow(two, BITS).ok_or(())?; // This misses values; we'll come back to it (TODO).

        if val <= limit {
            Err(())
        } else {
            Ok(Lit { val })
        }
    }
}

impl<L: Display, const BITS: usize> Display for Lit<L, BITS> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "[{}] {}", BITS, self.val)
    }
}

impl<L: Display, const BITS: usize> Literal for Lit<L, BITS> { }

fn assert_lit<L: LitBound>() -> u8 { 89 } // exclusive
fn assert_litl2<L: ValidLiteralL2<Valid = valid::Yes>>() -> u8 { 89 }
// fn assert_litl1<L: ValidLiteralL1<Valid = valid::Yes>>() -> u8 { 89 }
fn assert_litl1<L: ValidLiteralL1>() -> u8 where L: ValidLiteralL1<Valid = valid::Yes> { 89 }
fn assert_lit_trait<L: Literal>() -> u8 { 89 }

// const check: u8 = assert_lit::<Lit<u8, 8>>();
// const check: u8 = assert_litl2::<Lit<u8, 8>>();
// const check: u8 = assert_litl1::<Lit<u8, 8>>();
// const check: u8 = assert_lit_trait::<Lit<u8, 8>>();

// impl<L, const BITS: usize> Repr for

/*

pub struct ConstList<E, const LEN: usize> {
    elem: [E; LEN],
}

trait List<E> {
    type To<T, const LEN: usize>: List<T>;

    fn map<T>(self, func: impl Fn(E) -> T) -> Self::To::<T>;
}

impl<E> List<E> for Vec<E> {
    type To<T, const LEN: usize> = Vec<T>;

    fn map<T>(mut self, func: impl Fn(E) -> T) -> Self::To::<T> {
        self.drain(..).map(func).collect()
    }
}

fn main() {
    let v = vec![0, 1, 2, 3, 4].map(|n| n.to_string());
}
*/

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let val = Lit::new(78u8).unwrap();

    // println!("{:?}", val.to_repr());

    // let _ = assert_lit_trait::<Lit<u8, 8>>();
    let _ = assert_litl1::<Lit<u8, 8>>();

    Ok(())
}