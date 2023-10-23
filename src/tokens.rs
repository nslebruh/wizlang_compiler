use crate::main3::{ParseBuffer, Error};

macro_rules! impl_token {
    ($ident:ident, $expr:expr) => {
        impl Peek for $ident {
          fn peek(input: &ParseBuffer) -> bool {
            if let Some(x) = input.get(input.index..$expr.len() + input.index) {
              if x == $expr.chars().collect::<Vec<char>>().as_slice() {
                true
              } else {
                false
              }
            } else {
              false
            }
          }
        }

        impl Parse for $ident {
          fn parse(input: &ParseBuffer) -> Result<Self, Error> {
            if $ident::Peek(input) {
              let token = input.get(input.index..$expr.len() + input.index);
              
            } else {
              return Err(Error {msg: format!("error getting {}", $ident), Span {bol: 0, lnum: 0, cnum: 0, len: 0}})
            }
          }
        }

        impl Token for $ident {}
    };
}


#[derive(Debug, Clone)]
pub struct Span {
  bol: usize,
  lnum: usize,
  cnum: usize,
  len: usize,
}

pub trait Peek {
  fn peek(input: &ParseBuffer) -> bool where Self: Sized;
}

pub trait Parse {
  fn parse(input: &ParseBuffer) -> Result<Self, Error> where Self: Sized;
}
pub trait Token: Peek + Parse {}

pub struct Imbue {
  span: Span,
}

pub struct Declare {
  span: Span
}

pub struct Ident {
  span: Span,
  value: String
}

pub struct Semi {
  span: Span
}

pub struct Dot {
  span: Span
}

pub struct Return {
  span: Span
}

pub struct Comma {
  span: Span
}

pub struct Equals {
  span: Span
}

pub struct Plus {
  span: Span
}

pub struct Minus {
  span: Span
}

pub struct Times {
  span: Span
}

pub struct Divided {
  span: Span
}

pub struct NotEquals {
  span: Span
}

pub struct Gt {
  span: Span
}

pub struct Lt {
  span: Span
}

pub struct Ge {
  span: Span
}

pub struct Le {
  span: Span
}

pub struct And {
  span: Span
}

pub struct Or {
  span: Span
}