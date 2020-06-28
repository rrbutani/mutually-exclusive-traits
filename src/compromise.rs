#![feature(marker_trait_attr)]
#![feature(optin_builtin_traits)]
#![feature(negative_impls)]

#![feature(specialization)]

#![feature(const_generics)]
#![allow(incomplete_features)]

use std::fmt::Display;
use std::ops::Add;

use num_traits::{One, Unsigned, CheckedMul};

//////////////////////////////////////////////////////////////

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

//////////////////////////////////////////////////////////////

trait Printable {
    // Args is TODO!
    fn to_repr(&self, args: ()) -> Repr;
}

trait UnaryOp {
    fn arg(&self) -> &dyn Printable;
    fn op(&self) -> &dyn Display;
}

trait BinaryOp {
    fn lhs(&self) -> &dyn Printable;
    fn rhs(&self) -> &dyn Printable;
    fn op(&self) -> &dyn Display;
}

trait Literal: Display { }

//////////////////////////////////////////////////////////////

mod valid {
    pub trait Bool: private::Sealed { }
    mod private { pub trait Sealed { } }

    impl<S: private::Sealed> Bool for S { }

    pub struct Yes;
    pub struct No;

    impl private::Sealed for Yes { }
    impl private::Sealed for No { }
}

use valid::{Bool, Yes, No};

//////////////////////////////////////////////////////////////

#[marker] auto trait ValidLiteralInner { }
impl<Invalid: UnaryOp> !ValidLiteralInner for Invalid { }
impl<Invalid: BinaryOp> !ValidLiteralInner for Invalid { }

#[marker] auto trait ValidUnaryInner { }
impl<Invalid: Literal> !ValidUnaryInner for Invalid { }
impl<Invalid: BinaryOp> !ValidUnaryInner for Invalid { }

#[marker] auto trait ValidBinaryInner { }
impl<Invalid: Literal> !ValidBinaryInner for Invalid { }
impl<Invalid: UnaryOp> !ValidBinaryInner for Invalid { }

//////////////////////////////////////////////////////////////

trait ValidLiteral: Literal + ValidLiteralInner { }
impl<L: Literal + ValidLiteralInner> ValidLiteral for L { }

trait ValidUnary: UnaryOp + ValidUnaryInner { }
impl<U: UnaryOp + ValidUnaryInner> ValidUnary for U { }

trait ValidBinary: BinaryOp + ValidBinaryInner { }
impl<B: BinaryOp + ValidBinaryInner> ValidBinary for B { }

//////////////////////////////////////////////////////////////

trait AsLiteral { fn as_lit(&self) -> &dyn Literal; }
default impl<T> AsLiteral for T { fn as_lit(&self) -> &dyn Literal { unreachable!() }}
impl<T: ValidLiteral> AsLiteral for T { fn as_lit(&self) -> &dyn Literal { self }}

trait AsUnary { fn as_un(&self) -> &dyn UnaryOp; }
impl<T> AsUnary for T { default fn as_un(&self) -> &dyn UnaryOp { unreachable!() }}
impl<T: ValidUnary> AsUnary for T { fn as_un(&self) -> &dyn UnaryOp { self }}

trait AsBinary { fn as_bin(&self) -> &dyn BinaryOp; }
impl<T> AsBinary for T { default fn as_bin(&self) -> &dyn BinaryOp { unreachable!() }}
impl<T: ValidBinary> AsBinary for T { fn as_bin(&self) -> &dyn BinaryOp { self }}


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

//////////////////////////////////////////////////////////////

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

//////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lit<L: Display, const BITS: usize> {
    val: L,
}

impl<L: Display, const BITS: usize> Lit<L, BITS>
where
    L: Unsigned,
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

impl<L: Display, const BITS: usize> ValidLiteralInner for Lit<L, BITS> { }

impl<L: Display, const BITS: usize> Display for Lit<L, BITS> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "[{}] {}", BITS, self.val)
    }
}

impl<L: Display, const BITS: usize> Literal for Lit<L, BITS> { }

fn assert_lit<L: ValidLiteral>() -> u8 { 89 } // exclusive
// fn assert_litl2<L: ValidLiteralL2>() -> u8 { 89 }
// fn assert_litl1<L: ValidLiteralL1<Valid = valid::Yes>>() -> u8 { 89 }
// fn assert_litl1<L: ValidLiteralL1>() -> u8 where L: ValidLiteralL1<Valid = valid::Yes> { 89 }
// fn assert_litl1<L: ValidLiteralL1>() -> u8 { 89 }
fn assert_lit_trait<L: Literal>() -> u8 { 89 }

// fn assert_printable<L: Printable>() -> u8 { 89 }
// fn assert_printable<L: Printable>() -> u8 { 89 }

//////////////////////////////////////////////////////////////

struct Cheater;

impl Display for Cheater {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Literal for Cheater { }
impl UnaryOp for Cheater { fn op(&self) -> &dyn std::fmt::Display { todo!() }
fn arg(&self) -> &dyn Printable { todo!() }
}
// impl ValidLiteralInner for Cheater { }

//////////////////////////////////////////////////////////////


fn main() {
    let _  = assert_lit_trait::<Lit<u8, 6>>();
    let _  = assert_lit::<Lit<u8, 6>>();

    // let _  = assert_lit::<Cheater>();

    let val = Lit::<_, 6>::new(78u8).unwrap();

    // let _ = assert_printable::<Lit<u8, 8>>();
    println!("{:?}", val.to_repr(()));
}
