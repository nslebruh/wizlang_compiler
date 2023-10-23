use std::ops::Deref;

use crate::tokens::{Span, Token, Parse, Peek};

#[derive(Debug, Hash, Clone)]
pub enum Type {
  String,
  Int,
  Float,
  Bool,
  Custom(String)
}

pub struct Error {
  msg: String,
  span: Span
}

pub struct ParseBuffer {
  inner: String,
  index: usize,
  bol: usize,
  lnum: usize,
}

impl Deref for ParseBuffer {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl ParseBuffer {
  pub fn new(tokens: String) -> Self {
    if tokens.is_ascii() {
      Self {
        inner: tokens,
        index: 0,
        bol: 0,
        lnum: 0
      }
    } else {
      panic!("string not ascii")
    }
    
  }

  pub fn test(&mut self) {
  }
  
  pub fn parse<T: Parse>(&self) -> Result<T, Error> {
    T::parse(self)
  }

  pub fn peek<T: Peek>(&self) -> bool {
      T::peek(self)
  }
}