// See: https://play.rust-lang.org/?version=nightly&mode=debug&edition=2018&gist=b9014ecaf065b73d813ea04aa0445035

#![feature(specialization)]
#![allow(incomplete_features)]

struct Yes; struct No;

trait UnaryOperation { }
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

fn assert_litl1<L: ValidLiteralL1<Valid = Yes>>(_l: L) { }

struct Cat;

fn main() {
    assert_litl1(Foo);
    assert_litl1(Cat);
}