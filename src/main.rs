#![recursion_limit="100000"]

#![feature(marker_trait_attr)]
#![feature(optin_builtin_traits)]
#![feature(negative_impls)]

#![feature(specialization)]

#![feature(const_generics)]
#![feature(const_fn)]
#![feature(never_type)]
#![feature(type_name_of_val)]
#![feature(try_blocks)]
#![allow(incomplete_features)]

use std::fmt::{self, Display};
use std::ops::{Add, Sub, Mul};

use num_traits::{One, Unsigned, CheckedMul};

////////////////////////////////////////////////////////////////////////////////////////

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
    Lit(LitData),
    UnOp(UnOpData),
    BinOp(BinOpData),
}

impl Display for Repr {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Repr::*;
        match self {
            Lit(LitData(lit)) => write!(fmt, "{}", lit),
            UnOp(UnOpData { op, arg }) => write!(fmt, "{}({})", op, arg),
            BinOp(BinOpData { op, lhs, rhs }) => write!(fmt, "({}) {} ({})", lhs, op, rhs),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////

pub trait Printable {
    // Args is TODO!
    fn to_repr(&self, args: ()) -> Repr;
}

pub trait UnaryOp {
    fn arg(&self) -> &dyn Printable;
    fn op(&self) -> &dyn Display;
}

pub trait BinaryOp {
    fn lhs(&self) -> &dyn Printable;
    fn rhs(&self) -> &dyn Printable;
    fn op(&self) -> &dyn Display;
}

pub trait Literal: Display { }

////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////

#[marker] auto trait ValidLiteralInner { }
impl<Invalid: UnaryOp> !ValidLiteralInner for Invalid { }
impl<Invalid: BinaryOp> !ValidLiteralInner for Invalid { }

#[marker] auto trait ValidUnaryInner { }
impl<Invalid: Literal> !ValidUnaryInner for Invalid { }
impl<Invalid: BinaryOp> !ValidUnaryInner for Invalid { }

#[marker] auto trait ValidBinaryInner { }
impl<Invalid: Literal> !ValidBinaryInner for Invalid { }
impl<Invalid: UnaryOp> !ValidBinaryInner for Invalid { }

////////////////////////////////////////////////////////////////////////////////////////

trait ValidLiteral: Literal + ValidLiteralInner { }
impl<L: Literal + ValidLiteralInner> ValidLiteral for L { }

trait ValidUnary: UnaryOp + ValidUnaryInner { }
impl<U: UnaryOp + ValidUnaryInner> ValidUnary for U { }

trait ValidBinary: BinaryOp + ValidBinaryInner { }
impl<B: BinaryOp + ValidBinaryInner> ValidBinary for B { }

////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////

// TODO
// pub struct Bits<T: Unsigned, const BITS: usize> { .. }

////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BasicLit<L: Display>(L);
impl<L: Display> BasicLit<L> { #[inline] pub const fn new(val: L) -> Self { Self(val) } }
impl<L: Display> Literal for BasicLit<L> { }
impl<L: Display> ValidLiteralInner for BasicLit<L> { }
impl<L: Display> Display for BasicLit<L> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result { write!(fmt, "{}", self.0) }
}

impl<L: Display + Clone> Evaluable for BasicLit<L> {
    type Computed = L;

    #[inline]
    fn evaluate(&self) -> Self::Computed { self.0.clone() }
}

////////////////////////////////////////////////////////////////////////////////////////

pub trait Evaluable {
    type Computed;

    // #[inline]
    fn evaluate(&self) -> Self::Computed;
}

////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Ls<T>(T);

impl<T> From<T> for Ls<T> {
    fn from(t: T) -> Self { Ls(t) }
}

////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AddOp<Lhs: Printable + Evaluable, Rhs: Printable + Evaluable>
where
    <Lhs as Evaluable>::Computed: Add<<Rhs as Evaluable>::Computed>,
{
    lhs: Lhs,
    rhs: Rhs,
}

impl<Lhs: Printable + Evaluable, Rhs: Printable + Evaluable> AddOp<Lhs, Rhs>
where
    <Lhs as Evaluable>::Computed: Add<<Rhs as Evaluable>::Computed>,
{
    #[inline] const fn new_unchecked(lhs: Lhs, rhs: Rhs) -> Self { Self { lhs, rhs } }
}

impl<Lhs: Printable + Evaluable, Rhs: Printable + Evaluable> BinaryOp for AddOp<Lhs, Rhs>
where
    <Lhs as Evaluable>::Computed: Add<<Rhs as Evaluable>::Computed>,
{
    fn lhs(&self) -> &dyn Printable { &self.lhs }
    fn rhs(&self) -> &dyn Printable { &self.rhs }
    fn op(&self) -> &dyn Display { &"+" }
}

impl<Lhs: Printable + Evaluable, Rhs: Printable + Evaluable> ValidBinaryInner for AddOp<Lhs, Rhs>
where
    <Lhs as Evaluable>::Computed: Add<<Rhs as Evaluable>::Computed>,
{ }

impl<Lhs: Printable + Evaluable, Rhs: Printable + Evaluable> Evaluable for AddOp<Lhs, Rhs>
where
    <Lhs as Evaluable>::Computed: Add<<Rhs as Evaluable>::Computed>,
{
    type Computed = <<Lhs as Evaluable>::Computed as Add<<Rhs as Evaluable>::Computed>>::Output;

    #[inline]
    fn evaluate(&self) -> Self::Computed { self.lhs.evaluate() + self.rhs.evaluate() }
}

// Dual of `std::ops::Add` that we own.
pub trait AddR<Rhs> { type Output; fn add_r(self, rhs: Rhs) -> Self::Output; }

impl<Lhs: Printable + Evaluable, Rhs: Printable + Evaluable> AddR<Rhs> for Lhs
where
    <Lhs as Evaluable>::Computed: Add<<Rhs as Evaluable>::Computed>,
{
    type Output = AddOp<Lhs, Rhs>;
    #[inline] fn add_r(self, rhs: Rhs) -> AddOp<Lhs, Rhs> { AddOp::new_unchecked(self, rhs) }
}

// The rest is just sugar that lets us use `std::ops::Add`.
// (add expr) + _
impl<IL: Printable + Evaluable, IR: Printable + Evaluable, Rhs: Printable + Evaluable> Add<Rhs> for AddOp<IL, IR>
where
    <IL as Evaluable>::Computed: Add<<IR as Evaluable>::Computed>,
    <AddOp<IL, IR> as Evaluable>::Computed: Add<<Rhs as Evaluable>::Computed>,
{
    type Output = AddOp<AddOp<IL, IR>, Rhs>;
    #[inline] fn add(self, rhs: Rhs) -> Self::Output { AddOp::new_unchecked(self, rhs) }
}

// (literal) + _
impl<LitInner: Display + Clone, Rhs: Printable + Evaluable> Add<Rhs> for BasicLit<LitInner>
where
    LitInner: Add<<Rhs as Evaluable>::Computed>,
{
    type Output = AddOp<BasicLit<LitInner>, Rhs>;
    #[inline] fn add(self, rhs: Rhs) -> Self::Output { AddOp::new_unchecked(self, rhs) }
}

// (marked _) + _
impl<Lhs: Printable + Evaluable, Rhs: Printable + Evaluable> Add<Rhs> for Ls<Lhs>
where
    <Lhs as Evaluable>::Computed: Add<<Rhs as Evaluable>::Computed>,
{
    type Output = AddOp<Lhs, Rhs>;
    #[inline]
    fn add(self, rhs: Rhs) -> AddOp<Lhs, Rhs> {
        AddOp::new_unchecked(self.0, rhs)
    }
}

////////////////////////////////////////////////////////////////////////////////////////

fn assert_lit<L: ValidLiteral>() -> u8 { 89 } // exclusive
// fn assert_litl2<L: ValidLiteralL2>() -> u8 { 89 }
// fn assert_litl1<L: ValidLiteralL1<Valid = valid::Yes>>() -> u8 { 89 }
// fn assert_litl1<L: ValidLiteralL1>() -> u8 where L: ValidLiteralL1<Valid = valid::Yes> { 89 }
// fn assert_litl1<L: ValidLiteralL1>() -> u8 { 89 }
fn assert_lit_trait<L: Literal>() -> u8 { 89 }

fn eval<E: Evaluable>(e: E) -> E::Computed { e.evaluate() }

// fn assert_printable<L: Printable>() -> u8 { 89 }
// fn assert_printable<L: Printable>() -> u8 { 89 }

////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////

use Ls as L;
use BasicLit as Lt;

fn main() {
    // let _  = assert_lit_trait::<Lit<u8, 6>>();
    // let _  = assert_lit::<Lit<u8, 6>>();

    // let _  = assert_lit::<Cheater>();

    // let val = Lit::<u128, 6>::new(78u128).unwrap();
    let val = BasicLit::new(78u128);
    // let _ = eval(val);
    // let _ = assert_lit_trait::<BasicLit<u128>>();

    let val = L(val) + val;
    let val = val + val;
    let val = val + val;
    let val = val + val;
    let val = BasicLit::new(12) * val;
    let val = BasicLit::new(120000000000) - val * val;

    // let val = val + Lit::<_, 6>::new_unchecked(123u128) + val + val + val + val + val + BasicLit::new(123) + Lt(89) + Lt(23);
    // let val = val + Lit::<_, 6>::new_unchecked(123u128) + val + val + val + val + val + BasicLit::new(123) + Lt(89) + Lt(23);
    // let val = val + Lit::<_, 6>::new_unchecked(123u128) + val + val + val + val + val + BasicLit::new(123) + Lt(89) + Lt(23);
    // let val = val + Lit::<_, 6>::new_unchecked(123u128) + val + val + val + val + val + BasicLit::new(123) + Lt(89) + Lt(23);


    // fn recurse<O: Evaluable, E: Evaluable, F: Evaluable>(n: usize, e: E, f: F) where E: Add<F, Output = O>, O: Add<F>, <O as std::ops::Add<F>>::Output: Evaluable, <O as std::ops::Add<F>>::Output: std::ops::Add<F>  {
    //     if n == 0 {
    //         return;
    //     } else {
    //         recurse(n - 1, e + f, f)
    //     }
    // }

    // let val = val + val;
    // let val = val + val;
    // let val = val + val;
    // let val = val + val;
    // let val = val + val;

    // let _ = assert_printable::<Lit<u8, 8>>();
    // println!("{:#?}", val.to_repr(()));
    println!("{:#}", val.to_repr(()));
    println!("{:#?}", val.evaluate());
    // println!("{}", core::any::type_name_of_val(&val));
}
