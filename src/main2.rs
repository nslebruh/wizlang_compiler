use std::fmt::Display;
use std::ops::{RangeBounds, Deref, DerefMut, AddAssign, Add};
use std::slice::SliceIndex;
use std::collections::{VecDeque, HashMap};

#[derive(Debug, Hash, Clone)]
pub enum Type {
  String,
  Int,
  Float,
  Bool,
  Custom(String)
}

#[derive(Debug, Clone)]
pub enum Value {
  String(String),
  Int(i64),
  Float(f64),
  Bool(bool),
  Ident(Identifier),
  Operator(Operator)
}

impl Value {
  pub fn get_ident(self) -> Result<Identifier, String> {
    match self {
      Self::Ident(x) => Ok(x),
      x => Err(format!("expected an identifier and found {:?}", x))
    }
  }
}

#[derive(Debug, Clone)]
pub struct Identifier(String);

#[derive(Debug, Clone)]
pub struct Span {
  bol: usize,
  lnum: usize,
  cnum: usize,
  len: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ln {}, col {} to {}", self.lnum, self.cnum - self.bol, (self.cnum + (self.len - 1)) - self.bol)
    }
}

impl Span {
  pub fn new(bol: usize, lnum: usize, cnum: usize,
  len: usize) -> Self {
    Self {
      bol,
      lnum,
      cnum,
      len
    }
  }
}

impl Add for Span {

    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {bol: self.bol, lnum: self.lnum, cnum: self.cnum, len: self.len + rhs.len}
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TokenType {
  FullStop,
  As,
  With,
  Semi,
  Return,
  Literal,
  Function,
  Ident,
  Assign,
  ConstAssign,
  Comma,
  Be,
  Equal,
  To,
  Not,
  And,
  Greater,
  Than,
  Less,
  If,
  Else,
  Then,
  Invoke,
  Field,
  Or,
  Conclude,
  Becometh,
  Struct,
  Unequal,
  Operator
}

#[derive(Debug, Clone)]
pub struct Token {
  ty: TokenType,
  span: Span,
  value: Option<Value>
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token {{\n\t\tty: {:?},\n\t\tspan: {},\n\t\tvalue: {:?}\n\t}}", self.ty, self.span, self.value)
    }
}

impl Token {
  pub fn new(ty: TokenType, span: Span, value: Option<Value>) -> Self {
    Self {
      ty,
      span,
      value
    }
  }
}

pub struct ParseBuffer {
  inner: Vec<Token>,
  types: Vec<TokenType>,
  pub index: usize
}

impl Deref for ParseBuffer {
    type Target = Vec<Token>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for ParseBuffer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Display for ParseBuffer {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut output = String::new();
      for n in &self.inner {
        output.push('\t');
        output.push_str(n.to_string().as_str());
        output.push_str(",\n");
      }
      write!(f, "Tokens: [\n{}]", output)
  }
}

impl ParseBuffer {
  pub fn new() -> Self {
    Self {
      inner: Vec::new(),
      types: Vec::new(),
      index: 0
    }
  }
  pub fn get_next(&mut self) -> Option<(Token, TokenType)> {
    if self.inner.get(self.index).is_some() {
      self.index += 1;
      Some((self.inner[self.index - 1].clone(), self.types[self.index - 1].clone())) 
    } else {
      None
    }
  }

  pub fn peek(&self) -> Option<&Token> {
    self.inner.get(self.index)
  }

  pub fn peek2(&self) -> Option<&Token> {
    self.inner.get(self.index + 1)
  }

  pub fn peek3(&self) -> Option<&Token> {
    self.inner.get(self.index + 2)
  }

  pub fn peek_range<I: SliceIndex<[Token]>>(&self, range: I) -> Option<&I::Output> {
    self.inner.get(range)
  }

  pub fn peek_range_type<I: SliceIndex<[TokenType]>>(&self, range: I) -> Option<&I::Output> {
    self.types.get(range)
  }

  pub fn peek_type(&self) -> Option<&TokenType> {
    self.types.get(self.index)
  }

  pub fn peek2_type(&self) -> Option<&TokenType> {
    self.types.get(self.index + 1)
  }

  pub fn peek3_type(&self) -> Option<&TokenType> {
    self.types.get(self.index + 2)
  }
  pub fn len(&self) -> usize {
    self.inner.len()
  }
  
  pub fn is_next(&self) -> bool {
    self.types.get(self.index).is_some()
  }
}

pub struct Lexer {
  string: String,
  pub buffer: ParseBuffer
}

impl Lexer {
  pub fn new(string: impl Into<String>) -> Result<Self, String> {
    Self {
      string: string.into(),
      buffer: ParseBuffer::new()
    }.lex()
  }

  fn lex(mut self) -> Result<Self, String> {
    let mut buf = String::new();
    let mut idx = 0;
    let str: Vec<char> = self.string.chars().collect();

    let mut bol: usize = 0; 
    let mut lnum: usize = 1;

    while idx < str.len() {
      let cnum = idx + 1;
      let mut len: usize = 1;
      let c = str[idx];
      if c.is_ascii_alphabetic() {
        buf.push(c);
        idx += 1;
        while str.get(idx).is_some() {
          if str[idx].is_ascii_alphanumeric() {
              buf.push(str[idx]);
              idx += 1;
              len += 1;
          } else {
              break
          }
        }
        match buf.as_str() {
          "declare" | "Declare" => {
            self.buffer.push(Token::new(TokenType::ConstAssign, Span::new(bol, lnum, cnum, len), None));
          },
          "imbue" | "Imbue" => {
            self.buffer.push(Token::new(TokenType::Assign, Span::new(bol, lnum, cnum, len), None));
          },
          "bestow" | "Bestow" => {
            self.buffer.push(Token::new(TokenType::Return, Span::new(bol, lnum, cnum, len), None))
          },
          "conjure" | "Conjure" => {
            self.buffer.push(Token::new(TokenType::Function, Span::new(bol, lnum, cnum, len), None))
          },
          "plus" => {
            self.buffer.push(Token::new(TokenType::Operator, Span::new(bol, lnum, cnum, len), Some(Value::Operator(Operator::Add))))
          },
          "minus" => {
            self.buffer.push(Token::new(TokenType::Operator, Span::new(bol, lnum, cnum, len), Some(Value::Operator(Operator::Sub))))
          },
          "times" => {
            self.buffer.push(Token::new(TokenType::Operator, Span::new(bol, lnum, cnum, len), Some(Value::Operator(Operator::Mul))))
          },
          "over" => {
            self.buffer.push(Token::new(TokenType::Operator, Span::new(bol, lnum, cnum, len), Some(Value::Operator(Operator::Div))))
          },
          "be" => {
            self.buffer.push(Token::new(TokenType::Be, Span::new(bol, lnum, cnum, len), None))
          },
          "equal" => {
            self.buffer.push(Token::new(TokenType::Equal, Span::new(bol, lnum, cnum, len), None))
          },
          "to" => {
            self.buffer.push(Token::new(TokenType::To, Span::new(bol, lnum, cnum, len), None))
          },
          "or" => {
            self.buffer.push(Token::new(TokenType::Or, Span::new(bol, lnum, cnum, len), None))
          },
          "not" => {
            self.buffer.push(Token::new(TokenType::Not, Span::new(bol, lnum, cnum, len), None))
          },
          "and" => {
            self.buffer.push(Token::new(TokenType::And, Span::new(bol, lnum, cnum, len), None))
          },
          "greater" => {
            self.buffer.push(Token::new(TokenType::Greater, Span::new(bol, lnum, cnum, len), None))
          },
          "than" => {
            self.buffer.push(Token::new(TokenType::Than, Span::new(bol, lnum, cnum, len), None))
          },
          "less" => {
            self.buffer.push(Token::new(TokenType::Less, Span::new(bol, lnum, cnum, len), None))
          },
          "should" | "Should" => {
            self.buffer.push(Token::new(TokenType::If, Span::new(bol, lnum, cnum, len), None))
          },
          "otherwise" | "Otherwise" => {
            self.buffer.push(Token::new(TokenType::Else, Span::new(bol, lnum, cnum, len), None))
          },
          "then" => {
            self.buffer.push(Token::new(TokenType::Then, Span::new(bol, lnum, cnum, len), None))
          },
          "invoke" | "Invoke" => {
            self.buffer.push(Token::new(TokenType::Invoke, Span::new(bol, lnum, cnum, len), None))
          },
          "conclude" | "Conclude" => {
            self.buffer.push(Token::new(TokenType::Conclude, Span::new(bol, lnum, cnum, len), None))
          },
          "becometh" => {
            self.buffer.push(Token::new(TokenType::Becometh, Span::new(bol, lnum, cnum, len), None))
          },
          "envision" | "Envision" => {
            self.buffer.push(Token::new(TokenType::Struct, Span::new(bol, lnum, cnum, len), None))
          },
          "unequal" => {
            self.buffer.push(Token::new(TokenType::Unequal, Span::new(bol, lnum, cnum, len), None))
          },
          "truth" => {
            self.buffer.push(Token::new(TokenType::Literal, Span::new(bol, lnum, cnum, len), Some(Value::Bool(true))))
          },
          "untruth" => {
            self.buffer.push(Token::new(TokenType::Literal, Span::new(bol, lnum, cnum, len), Some(Value::Bool(false))))
          },
          "as" => {
            self.buffer.push(Token::new(TokenType::As, Span::new(bol, lnum, cnum, len), None))
          },
          "with" => {
            self.buffer.push(Token::new(TokenType::With, Span::new(bol, lnum, cnum, len), None))
          }
          _ => {
            self.buffer.push(Token::new(TokenType::Ident, Span::new(bol, lnum, cnum, len), Some(Value::Ident(Identifier(buf.clone())))));
            
          }
        }
        buf.clear()
      } else if c.is_ascii_digit() {
        buf.push(c);
        idx += 1;
        let mut fullstops = 0;
        while str.get(idx).is_some() {
          if str[idx].is_ascii_digit() || str[idx] == '.' {
            if str[idx] == '.' {
              fullstops += 1;
              if fullstops == 2 {
                break
              }
            }
            buf.push(str[idx]);
            idx += 1;
            len += 1;
          } else {
            break
          }
        }
        if buf.ends_with('.') {
          buf.pop();
          idx -= 1;
          len -= 1;
          
        }
        self.buffer.push(Token::new(TokenType::Literal, Span::new(bol, lnum, cnum, len), Some(if buf.contains('.') {Value::Float(buf.parse::<f64>().unwrap())} else {Value::Int(buf.parse::<i64>().unwrap())})));
        buf.clear()
      } else if c == '\n' {
        idx += 1;
        lnum += 1;
        bol = idx;
        continue
      } else if c == ' ' {
        idx += 1;
        continue
      } else if c == '\'' {
        buf.push(c);
        idx += 1;
        if str.get(idx) == Some(&'s') {
          buf.push(str[idx]);
          len += 1;
          idx += 1;
          self.buffer.push(Token::new(TokenType::Field, Span::new(bol, lnum, cnum, len), None));
          buf.clear()
        } else {
          return Err(format!("expected s at {} and found {}", idx, str[idx]))
        }
      } else if c == '"' {
        idx += 1;
        while str.get(idx).is_some() && str.get(idx) != Some(&'"') {
            buf.push(str[idx]);
            idx += 1;
            len += 1;
        }
        idx += 1;
        self.buffer.push(Token::new(TokenType::Literal, Span::new(bol, lnum, cnum, len), Some(Value::String(buf.clone()))));
        buf.clear();
      } else if c == ',' {
        self.buffer.push(Token::new(TokenType::Comma, Span::new(bol, lnum, cnum, len), None));
        idx += 1;
      } else if c == '.' {
        self.buffer.push(Token::new(TokenType::FullStop, Span::new(bol, lnum, cnum, len), None));
        idx += 1;
      } else if c == ';' {
        self.buffer.push(Token::new(TokenType::Semi, Span::new(bol, lnum, cnum, len), None));
        idx += 1;
      } else {
        return Err(format!("invalid syntax at {}: {}", cnum, c))
      }
    }
    for n in self.buffer.clone() {
      self.buffer.types.push(n.ty)
    }
    Ok(self)
  } 
}

pub struct Parser {
  pub buffer: ParseBuffer,
  pub idents: HashMap<String, Type>,
  pub statements: Vec<Statement>,
}

impl Parser {
  pub fn new(buffer: ParseBuffer) -> Self {
    Self {
      buffer,
      idents: HashMap::new(),
      statements: Vec::new()
    }
  }

  pub fn operator_pass(&mut self) -> Result<(), String> {
    if !self.buffer.types.contains(&TokenType::Be) {
      return Ok(())
    }
    let mut new_buffer: Vec<Token> = Vec::new();
    let mut idx = 0;
    println!("len: {}", self.buffer.len());
    while idx < self.buffer.len() {
      println!("idx: {}", idx);
      let (token, token_type) = self.buffer.get_next().unwrap();
      if token_type == TokenType::Be {
        match self.buffer.get_next() {
          Some((equal_token, TokenType::Equal)) => {
            match self.buffer.get_next() {
              Some((to_token, TokenType::To)) => {
                new_buffer.push(Token::new(TokenType::Operator, token.span + equal_token.span + to_token.span, Some(Value::Operator(Operator::Eq))));
                idx += 3;
              },
              Some((token, token_type)) => return Err(format!("expected to at {} and found {:?}", token.span, token_type)),
              None => return Err("unexpected end of input".to_string())
            }
          },
          Some((unequal_token, TokenType::Unequal)) => {
            match self.buffer.get_next() {
              Some((to_token, TokenType::To)) => {
                new_buffer.push(Token::new(TokenType::Operator, token.span + unequal_token.span + to_token.span, Some(Value::Operator(Operator::Ne))));
                idx += 3;
              },
              Some((token, token_type)) => return Err(format!("expected to at {} and found {:?}", token.span, token_type)),
              None => return Err("unexpected end of input".to_string())
            }
          },
          Some((greater_token, TokenType::Greater)) => {
            match self.buffer.get_next() {
              Some((than_token, TokenType::Than)) => {
                match self.buffer.peek_type() {
                  Some(TokenType::Or) => {
                    let or_token = self.buffer.get_next().unwrap();
                    match self.buffer.get_next() {
                      Some((equal_token, TokenType::Equal)) => {
                        match self.buffer.get_next() {
                          Some((to_token, TokenType::To)) => {
                            new_buffer.push(Token::new(TokenType::Operator, token.span + greater_token.span + than_token.span + or_token.0.span + equal_token.span + to_token.span, Some(Value::Operator(Operator::Ge))));
                            idx += 6;
                          },
                          Some((token, token_type)) => return Err(format!("expected to at {} and found {:?}", token.span, token_type)),
                          None => return Err("unexpected end of input".to_string())
                        }
                      },
                      Some((token, token_type)) => return Err(format!("expected equal at {} and found {:?}", token.span, token_type)),
                      None => return Err("unexpected end of input".to_string())
                    }
                  },
                  Some(_) => {
                    new_buffer.push(Token::new(TokenType::Operator, token.span + greater_token.span + than_token.span, Some(Value::Operator(Operator::Gt))));
                    idx += 3;
                  },
                  None => return Err("unexpected end of input".to_string())
                }
              }
              Some((token, token_type)) => return Err(format!("expected than at {} and found {:?}", token.span, token_type)),
              None => return Err("unexpected end of input".to_string())
            }
          },
          Some((less_token, TokenType::Less)) => {
            match self.buffer.get_next() {
              Some((than_token, TokenType::Than)) => {
               match self.buffer.peek_type() {
                Some(TokenType::Or) => {
                  let or_token = self.buffer.get_next().unwrap();
                  match self.buffer.get_next() {
                    Some((equal_token, TokenType::Equal)) => {
                      match self.buffer.get_next() {
                        Some((to_token, TokenType::To)) => {
                          new_buffer.push(Token::new(TokenType::Operator, token.span + less_token.span + than_token.span + or_token.0.span + equal_token.span + to_token.span, Some(Value::Operator(Operator::Le))));
                            idx += 6;
                        },
                        Some((token, token_type)) => return Err(format!("expected to at {} and found {:?}", token.span, token_type)),
                        None => return Err("unexpected end of input".to_string())
                      }
                    },
                    Some((token, token_type)) => return Err(format!("expected equal at {} and found {:?}", token.span, token_type)),
                    None => return Err("unexpected end of input".to_string())
                  }
                },
                Some(_) => {
                  new_buffer.push(Token::new(TokenType::Operator, token.span + less_token.span + than_token.span, Some(Value::Operator(Operator::Lt))));
                    idx += 3;
                }
                None => return Err("unexpected end of input".to_string())
               } 
              },
              Some((token, token_type)) => return Err(format!("expected than at {} and found {:?}", token.span, token_type)),
              None => return Err("unexpected end of input".to_string())
            }
          },
          Some((token, token_type)) => {
            return Err(format!("expected greater, less, equal or unequal at {} and found {:?}", token.span, token_type))
          },
          None => {}
        }
      } else {
        new_buffer.push(token);
        idx += 1;
      }
    }
    self.buffer.inner = new_buffer;
    self.buffer.types.clear();
    self.buffer.index = 0;
    for n in &self.buffer.inner {
      self.buffer.types.push(n.ty)
    }
    Ok(())
  }

  fn fix_operator_priority(&mut self) {
    let mut idx = 0;
    while idx < self.statements.len() {

    }
  }

  pub fn parse(&mut self) -> Result<(), String> {
    self.operator_pass()?;
    println!("new: {:?}", self.buffer.inner);
    let mut idx = 0;
    while idx < self.buffer.len() {
      let mut stmt = ParseBuffer::new();

      while self.buffer.types[idx] != TokenType::FullStop {
        println!("parse idx: {}", idx);
        let token = self.buffer.get_next().unwrap();
        stmt.push(token.0);
        stmt.types.push(token.1);
        idx += 1;
      }
      self.buffer.get_next();
      idx += 1;
      if stmt.len() < 2 {
        return Err("unexpected end of input".to_string())
      }

      let statement = self.parse_statement(stmt)?;
      self.statements.push(statement)
    }
    Ok(())
  }

  fn parse_statement(&mut self, mut buffer: ParseBuffer) -> Result<Statement, String> {
    match buffer[0].ty {
      TokenType::Assign => {
        let ident: Identifier;
        buffer.get_next();
        if buffer.is_next() {
          if buffer.peek_type() == Some(&TokenType::Ident) {
            ident = match buffer.get_next().unwrap().0.value {
              Some(Value::Ident(i)) => i,
              x => return Err(format!("error in finding ident value of ident type. found {:?}", x))
            }
          } else {
            let token = buffer.get_next().unwrap();
            return Err(format!("expected an identifier at {} and found {:?}", token.0.span, token.1))
          }
        } else {
          return Err("unexpected end of input".to_string())
        }
        if buffer.is_next() {
          if buffer.peek_type() == Some(&TokenType::With) {
            buffer.get_next();
          } else {
            let x = buffer.get_next().unwrap();
            return Err(format!("expected with at {} and found {:?}", x.0.span, x.1))
          }
        } else {
          return Err("unexpected end of input".to_string())
        }
        let expr = self.parse_expression(&mut buffer)?;
        Ok(Statement::Assignment(Assignment(ident, expr)))
      },
      TokenType::ConstAssign => {
        let ident: Identifier;
        buffer.get_next();
        if buffer.is_next() {
          if buffer.peek_type() == Some(&TokenType::Ident) {
            ident = match buffer.get_next().unwrap().0.value {
              Some(Value::Ident(i)) => i,
              x => return Err(format!("error in finding ident value of ident type. found {:?}", x))
            }
          } else {
            let token = buffer.get_next().unwrap();
            return Err(format!("expected an identifier at {} and found {:?}", token.0.span, token.1))
          }
        } else {
          return Err("unexpected end of input".to_string())
        }
        if buffer.is_next() {
          if buffer.peek_type() == Some(&TokenType::As) {
            buffer.get_next();
          } else {
            let x = buffer.get_next().unwrap();
            return Err(format!("expected with at {} and found {:?}", x.0.span, x.1))
          }
        } else {
          return Err("unexpected end of input".to_string())
        }
        let expr = self.parse_expression(&mut buffer)?;
        Ok(Statement::ConstAssignment(ConstAssignment(ident, expr)))
      }
      //TokenType::Function => {
      //
      //},
      //TokenType::If => {
      //
      //},
      //TokenType::Else => {
      //
      //},
      _ => {
        Err(format!("expected a statement at {} and found {:?}", buffer[0].span, buffer[0].ty))
      }
    }
  }

  fn parse_expression(&mut self, buffer: &mut ParseBuffer) -> Result<Expression, String> {
    // check if first token is ident, lit or invoke
    let mut expr: Option<Expression> = None;
    if buffer.is_next() {
      match buffer.get_next() {
        Some((token, TokenType::Literal)) => { 
          expr = Some(Expression::Literal(token.value.unwrap()))
        },
        Some((token, TokenType::Ident)) => {
          expr = Some(Expression::Identifier(token.value.unwrap().get_ident().unwrap()))
        },                                                                                                                      
        Some((_, TokenType::Invoke)) => {
          panic!("havent done this yet fucknut")
        },
        Some((token, token_type)) => {
          return Err(format!("expected a literal, identifier of function call at {} and found {:?}", token.span, token_type))
        },
        None => return Err("unexpected end of input".to_string())
      };
      while buffer.is_next() {
        if buffer.peek_type().unwrap() == &TokenType::Field {
          buffer.get_next();
          match buffer.get_next() {
            Some((token,TokenType::Ident)) => {
              let old_expr = expr.unwrap();
              expr = Some(Expression::Field(Box::new(old_expr), token.value.unwrap().get_ident().unwrap()))
            },
            Some((token, token_type)) => return Err(format!("expected an identifier at {} and found {:?}", token.span, token_type)),
            None => return Err("unexpected end of input".to_string())
          }
        } else {
          let operator: Operator = match buffer.get_next().unwrap() {
            (token, TokenType::Operator) => {
               match token.value.unwrap() {
                Value::Operator(x) => x,
                _ => panic!("failed to get operator from operator type")
               }
            },
            (_, TokenType::Or) => {
              Operator::Or
            },
            (_, TokenType::And) => {
              Operator::And
            },
            (_, TokenType::Comma) => {
              break;
            }
            (token, token_type) => return Err(format!("expected an operator at {} and found {:?}", token.span, token_type))
          };
          let value = match buffer.get_next() {
            Some((token, TokenType::Ident)) => {
              Expression::Identifier(token.value.unwrap().get_ident().unwrap())
            },
            Some((token, TokenType::Literal)) => {
              match token.value.unwrap() {
                Value::Ident(_) => panic!("failed to get a literal from value"),
                x => Expression::Literal(x),
              }
            },
            Some((token, token_type)) => return Err(format!("expected an identifier or literal at {} and found {:?}", token.span, token_type)),
            None => return Err("unexpected end of input".to_string())
          };
          let old_expr = expr.unwrap();
          match old_expr.clone() {
            Expression::Literal(_) | Expression::Identifier(_) | Expression::Field(_, _) | Expression::FunctionCall(_, _) => {
              expr = Some(Expression::op(old_expr, operator, value))
            },
            Expression::Operation(old_expr1, old_operator, old_expr2) => {
              match (old_operator.get_priority(), operator.get_priority()) {
                (OperatorPriority::AddSub, OperatorPriority::AddSub) | (OperatorPriority::AddSub, OperatorPriority::AndOr) | (OperatorPriority::AddSub, OperatorPriority::Comparitive) => {
                  expr = Some(Expression::op(old_expr, operator, value))
                },
                (OperatorPriority::AddSub, OperatorPriority::MulDiv) => {
                  expr = Some(Expression::op(*old_expr1, old_operator, Expression::op(*old_expr2, operator, value)))
                },
                (OperatorPriority::MulDiv, _) => {
                  expr = Some(Expression::op(old_expr, operator, value))
                },
                (OperatorPriority::Comparitive, OperatorPriority::Comparitive) => {
                  return Err("do not chain comparison operators".to_string())
                },
                (OperatorPriority::Comparitive, OperatorPriority::AndOr) => {
                  expr = Some(Expression::op(old_expr, operator, value))
                },
                (OperatorPriority::Comparitive, _) => {
                  expr = Some(Expression::op(*old_expr1, old_operator, Expression::op(*old_expr2, operator, value)))
                },
                
                (OperatorPriority::AndOr, OperatorPriority::AndOr) => {
                  expr = Some(Expression::op(old_expr, operator, value))
                },
                (OperatorPriority::AndOr, _) => {
                  expr = Some(Expression::op(*old_expr1, old_operator, Expression::op(*old_expr2, operator, value)))
                }
              }
            }
          }
        }
      }
      expr.ok_or("err".to_string())
    } else {
      Err("unexpected end of input".to_string())
    }
  }
}

#[derive(Debug, Clone)]
pub enum Statement {
  Assignment(Assignment),
  ConstAssignment(ConstAssignment),
  FunctionAssignment(Identifier, Vec<(Identifier, Type)>, Vec<Statement>),
  If(Expression, Vec<Statement>),
  Else(Option<Expression>, Vec<Statement>),
  Return(Expression)
}

impl Statement {
  pub fn get_expression(&self) -> Option<Expression> {
    match self {
      Self::Else(expr, _) => expr.clone(),
      Self::Assignment(x) => Some(x.1.clone()),
      Self::ConstAssignment(x) => Some(x.1.clone()),
      Self::Return(expr) => Some(expr.clone()),
      Self::If(expr, _) => Some(expr.clone()),
      Self::FunctionAssignment(_, _, _) => None
    }
  }


  pub fn fix_priority(&mut self) {
    if let Statement::FunctionAssignment(_, _, stmts) = self {
      for stmt in stmts {
        stmt.fix_priority()
      }
    }
    if let Some(expr) = self.get_expression() {

    }
  }
}

#[derive(Debug, Clone)]
pub struct Assignment(Identifier, Expression);

#[derive(Debug, Clone)]
pub struct ConstAssignment(Identifier, Expression);

#[derive(Debug, Clone)]
pub enum Expression {
  FunctionCall(Identifier, Vec<Expression>),
  Operation(Box<Expression>, Operator, Box<Expression>),
  Literal(Value),
  Identifier(Identifier),
  Field(Box<Expression>, Identifier)
}

impl Expression {
  pub fn get_operator(&self) -> Option<Operator> {
    match self {
      Expression::Operation(_, op, _) => Some(op.clone()),
      _ => None
    }
  }

  pub fn op(expr1: Expression, op: Operator, expr2: Expression) -> Self {
    Expression::Operation(Box::new(expr1), op, Box::new(expr2))
  }
}

#[derive(Debug, Clone)]
pub enum Operator {
  Add,
  Sub,
  Mul,
  Div,
  Eq,
  Ne,
  Gt,
  Ge,
  Lt,
  Le,
  And,
  Or,
}

impl Operator {
  fn get_priority(&self) -> OperatorPriority {
    match self {
        Operator::Add | Operator::Sub => OperatorPriority::AddSub,
        Operator::Mul | Operator::Div=> OperatorPriority::MulDiv,
        Operator::Eq | Operator::Ne | Operator::Gt | Operator::Ge | Operator::Lt | Operator::Le => OperatorPriority::Comparitive,
        Operator::And |
        Operator::Or => OperatorPriority::AndOr,
    }
  }
}

pub enum OperatorPriority {
  AddSub,
  MulDiv,
  Comparitive,
  AndOr,
}