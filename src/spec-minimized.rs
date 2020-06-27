// #![feature(generic_associated_types)]
// #![feature(const_generics)]
// #![feature(const_fn)]
// #![feature(const_if_match)]
#![feature(specialization)]
// #![feature(marker_trait_attr)]
#![allow(incomplete_features)]

trait UnaryOperation { }
// trait BinaryOperation { }
struct Yes; struct No;

trait Literal { }
struct Foo; impl Literal for Foo { }

/**** L1 ****/
// Unary or (Lit + Unary)
// trait ValidUnaryL1 { type Valid/*: valid::Sealed*/; }
// default impl<T: UnaryOperation> ValidUnaryL1 for T { type Valid = valid::Yes; }
// default impl<T: UnaryOperation + BinaryOperation> ValidUnaryL1 for T { type Valid = valid::No; }
// default impl<T: UnaryOperation + BinaryOperation + Literal> ValidUnaryL1 for T { type Valid = valid::No; }

// // Binary or (Un + Binary)
// trait ValidBinaryL1 { type Valid/*: valid::Sealed*/; }
// default impl<T: BinaryOperation> ValidBinaryL1 for T { type Valid = valid::Yes; }
// default impl<T: BinaryOperation + Literal> ValidBinaryL1 for T { type Valid = valid::No; }
// default impl<T: BinaryOperation + Literal + UnaryOperation> ValidBinaryL1 for T { type Valid = valid::No; }

// Literal or (Bin + Literal)
trait ValidLiteralL1 { type Valid; }
impl<T: Literal> ValidLiteralL1 for T { default type Valid = Yes; }
impl<T: Literal + UnaryOperation> ValidLiteralL1 for T { type Valid = No; }
// impl<T: Literal + UnaryOperation + BinaryOperation> ValidLiteralL1 for T { type Valid = No; }


/*///// L2 //////
// Just Unary.
trait ValidUnaryL2 { type Valid/*: valid::Sealed*/; }
default impl<T: ValidUnaryL1<Valid = valid::Yes> + UnaryOperation> ValidUnaryL2 for T { type Valid = valid::Yes; }
default impl<T: ValidUnaryL1<Valid = valid::Yes> + UnaryOperation + Literal> ValidUnaryL2 for T { type Valid = valid::No; }

trait ValidUnary: ValidUnaryL2<Valid = valid::Yes> + UnaryOperation { }
impl<T: ValidUnaryL2<Valid = valid::Yes> + UnaryOperation> ValidUnary for T { }

// Just Binary.
trait ValidBinaryL2 { type Valid/*: valid::Sealed*/; }
default impl<T: ValidBinaryL1<Valid = valid::Yes> + BinaryOperation> ValidBinaryL2 for T { type Valid = valid::Yes; }
default impl<T: ValidBinaryL1<Valid = valid::Yes> + BinaryOperation + UnaryOperation> ValidBinaryL2 for T { type Valid = valid::No; }

trait ValidBinary: ValidBinaryL2<Valid = valid::Yes> + BinaryOperation { }
impl<T: ValidBinaryL2<Valid = valid::Yes> + BinaryOperation> ValidBinary for T { }

// Just Literal.
trait ValidLiteralL2 { type Valid/*: valid::Sealed*/; }
impl<T: ValidLiteralL1<Valid = valid::Yes> + Literal> ValidLiteralL2 for T { default type Valid = valid::Yes; }
impl<T: ValidLiteralL1<Valid = valid::Yes> + Literal + BinaryOperation> ValidLiteralL2 for T { default type Valid = valid::No; }

trait ValidLiteral: ValidLiteralL2<Valid = valid::Yes> + Literal { }
impl<T: ValidLiteralL2<Valid = valid::Yes> + Literal> ValidLiteral for T { }
*/
fn assert_litl1<L: ValidLiteralL1<Valid = Yes>>(_l: L) { }

struct Cat;

fn main() {
    assert_litl1(Foo);
    assert_litl1(Cat);
}