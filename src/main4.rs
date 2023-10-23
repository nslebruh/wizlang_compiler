use std::{ops::{Add, Deref, DerefMut}, fmt::Display, collections::HashMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
        Self {bol: self.bol, lnum: self.lnum, cnum: self.cnum, len: self.len + rhs.len + 1}
    }
}

#[derive(Debug, Clone)]
pub struct ParseBuffer {
  inner: Vec<Token>,
  index: usize
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

impl ParseBuffer {
  pub fn new(inner: Vec<Token>) -> Self {
    Self {
      inner,
      index: 0
    }
  }

  pub fn is_next(&self) -> bool {
    self.get(self.index).is_some()
  }

  pub fn parse(&mut self) -> Option<Token> {
    let token = self.get(self.index).cloned();
    self.index += 1;
    token
  }

  pub fn parse_unchecked(&mut self) -> Token {
    let token = self.parse();
    if let Some(x) = token {
      x
    } else {
      panic!("end of tokens bozo")
    }
  }

  pub fn peek(&self) -> Option<&Token> {
    self.get(self.index)
  }

  pub fn peek2(&self) -> Option<&Token> {
    self.get(self.index + 1)
  }

  pub fn peek3(&self) -> Option<&Token> {
    self.get(self.index + 2)
  }

  pub fn peek_n(&self, n: usize) -> Option<&Token> {
    self.get(self.index + n)
  }

  pub fn peek_type(&self) -> Option<TokenType> {
    self.get(self.index).map(|token| token._type.clone())
  }

  pub fn peek2_type(&self) -> Option<TokenType> {
    self.get(self.index + 1).map(|token| token._type.clone())
  }

  pub fn peek3_type(&self) -> Option<TokenType> {
    self.get(self.index + 2).map(|token| token._type.clone())
  }

  pub fn peek_n_type(&self, n: usize) -> Option<TokenType> {
    self.get(self.index + n).map(|token| token._type.clone())
  }

  pub fn contains(&self, tt: TokenType) -> bool {
    for n in &self.inner {
      if n._type == tt {
        return true
      }
    }
    false
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
  String(String),
  Integer(i64),
  Float(f64),
  Boolean(bool)
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(x) => write!(f, "{x}"),
            Literal::Integer(x) => write!(f, "{x}"),
            Literal::Float(x) => write!(f, "{x}"),
            Literal::Boolean(x) => write!(f, "{}", match x {
              true => "truth",
              false => "untruth"
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
  Literal(Literal),
  Identifier(String),
  Function,
  Assignment,
  ConstAssignment,
  Semi,
  FullStop,
  Return,
  Comma,
  If,
  Else,
  FunctionCall,
  Field,
  Conclude,
  Becometh,
  Struct,
  Greater,
  Less,
  Equal,
  Unequal,
  And,
  Or,
  Operator(Operator)
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            TokenType::Literal(x) => x.to_string(),
            TokenType::Identifier(x) => x.clone(),
            TokenType::Function => String::from("Conjure"),
            TokenType::Assignment => String::from("Imbue"),
            TokenType::ConstAssignment => String::from("Declare"),
            TokenType::Semi => String::from(";"),
            TokenType::FullStop => String::from("."),
            TokenType::Return => String::from("Bestow"),
            TokenType::Comma => String::from(","),
            TokenType::If => String::from("Should"),
            TokenType::Else => String::from("Otherwise"),
            TokenType::FunctionCall => String::from("Invoke"),
            TokenType::Field => String::from("'s"),
            TokenType::Conclude => String::from("Conclude"),
            TokenType::Becometh => String::from("becometh"),
            TokenType::Struct => String::from("Envision"),
            TokenType::Greater => String::from("greater"),
            TokenType::Less => String::from("less"),
            TokenType::Equal => String::from("equal"),
            TokenType::Unequal => String::from("unequal"),
            TokenType::And => String::from("and"),
            TokenType::Or => String::from("or"),
            TokenType::Operator(x) => x.to_string(),
        };
        write!(f, "{}", string)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
  _type: TokenType,
  span: Span
}

impl Token {
  pub fn new(_type: TokenType, span: Span) -> Self {
    Self {
      _type,
      span
    }
  }
}

impl From<Vec<Token>> for ParseBuffer {
    fn from(value: Vec<Token>) -> Self {
        ParseBuffer::new(value)
    }
}

pub fn lexer(input: impl Into<String>) -> Result<ParseBuffer, String> {
  let source: String = input.into();
  let source_vec = source.chars().collect::<Vec<_>>();
  let mut output: Vec<Token> = Vec::new();
  let mut lnum = 1;
  let mut bol = 0;
  
  let mut idx = 0;
  let mut buf = String::new();

  let mut is_comment: bool = false;
  let mut multiline: bool = false;

  while idx < source_vec.len() {
    if is_comment || multiline {
      if multiline {
        if source_vec.get(idx..idx + 3) == Some(&['-', '-', '-']) {
          multiline = false
        }
      } else if is_comment {
        if source_vec[idx] == '\n' {
          is_comment = false;
        }
      } else {
        panic!("how2")
      }
      idx += 1;
    } else {
      let cnum = idx + 1;
      let mut len: usize = 1;
      let c = source_vec[idx];
      if c.is_ascii_alphabetic() {
        buf.push(c);
          idx += 1;
          while source_vec.get(idx).is_some() {
            if source_vec[idx].is_ascii_alphanumeric() {
                buf.push(source_vec[idx]);
                idx += 1;
                len += 1;
            } else {
                break
            }
          }
          match buf.as_str() {
            "Declare" | "declare" => {
              output.push(Token::new(TokenType::ConstAssignment, Span::new(bol, lnum, cnum, len)));
            },
            "Imbue" | "imbue" => {
              output.push(Token::new(TokenType::Assignment, Span::new(bol, lnum, cnum, len)));
            },
            "Conjure" | "conjure" => {
              output.push(Token::new(TokenType::Function, Span::new(bol, lnum, cnum, len)));
            },
            "Bestow" | "bestow" => {
              output.push(Token::new(TokenType::Return, Span::new(bol, lnum, cnum, len)));
            },
            "Should" | "should" => {
              output.push(Token::new(TokenType::If, Span::new(bol, lnum, cnum, len)));
            },
            "Otherwise" | "otherwise" => {
              output.push(Token::new(TokenType::Else, Span::new(bol, lnum, cnum, len)));
            },
            "Invoke" | "invoke" => {
              output.push(Token::new(TokenType::FunctionCall, Span::new(bol, lnum, cnum, len)));
            },
            "Conclude" | "conclude" => {
              output.push(Token::new(TokenType::Conclude, Span::new(bol, lnum, cnum, len)));
            },
            "Becometh" | "becometh" => {
              output.push(Token::new(TokenType::Becometh, Span::new(bol, lnum, cnum, len)));
            },
            "Envision" | "envision" => {
              output.push(Token::new(TokenType::Struct, Span::new(bol, lnum, cnum, len)));
            },
            "greater" => {
              output.push(Token::new(TokenType::Greater, Span::new(bol, lnum, cnum, len)));
            },
            "less" => {
              output.push(Token::new(TokenType::Less, Span::new(bol, lnum, cnum, len)));
            },
            "equal" => {
              output.push(Token::new(TokenType::Equal, Span::new(bol, lnum, cnum, len)));
            },
            "unequal" => {
              output.push(Token::new(TokenType::Unequal, Span::new(bol, lnum, cnum, len)));
            },
            "truth" => {
              output.push(Token::new(TokenType::Literal(Literal::Boolean(true)), Span::new(bol, lnum, cnum, len)));
            },
            "untruth" => {
              output.push(Token::new(TokenType::Literal(Literal::Boolean(false)), Span::new(bol, lnum, cnum, len)));
            },
            "plus" => {
              output.push(Token::new(TokenType::Operator(Operator::Add), Span::new(bol, lnum, cnum, len)));
            },
            "minus" => {
              output.push(Token::new(TokenType::Operator(Operator::Sub), Span::new(bol, lnum, cnum, len)));
            },
            "times" => {
              output.push(Token::new(TokenType::Operator(Operator::Mul), Span::new(bol, lnum, cnum, len)));
            },
            "over" => {
              output.push(Token::new(TokenType::Operator(Operator::Div), Span::new(bol, lnum, cnum, len)));
            },
            "and" => {
              output.push(Token::new(TokenType::And, Span::new(bol, lnum, cnum, len)));
            },
            "or" => {
              output.push(Token::new(TokenType::Or, Span::new(bol, lnum, cnum, len)));
            },
            _ => {
              output.push(Token::new(TokenType::Identifier(buf.clone()), Span::new(bol, lnum, cnum, len)));
            }
          };
          buf.clear()
      } else if c.is_ascii_digit() {
        buf.push(c);
        idx += 1;
        let mut fullstops = 0;
        while source_vec.get(idx).is_some() {
          if source_vec[idx].is_ascii_digit() || source_vec[idx] == '.' {
            if source_vec[idx] == '.' {
              fullstops += 1;
              if fullstops == 2 {
                break
              }
            }
            buf.push(source_vec[idx]);
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
        output.push(Token::new(match buf.contains('.') {
          true => TokenType::Literal(Literal::Float(buf.parse::<f64>().unwrap())),
          false => TokenType::Literal(Literal::Integer(buf.parse::<i64>().unwrap())),
        }, Span::new(bol, lnum, cnum, len)));
        buf.clear()
      } else if c == ' ' {
        idx += 1;
        continue
      } else if c == '\n' {
        idx += 1;
        lnum += 1;
        bol = idx;
        if is_comment {
          is_comment = false
        }
      } else if c == '\'' {
        buf.push(c);
          idx += 1;
          if source_vec.get(idx) == Some(&'s') {
            buf.push(source_vec[idx]);
            len += 1;
            idx += 1;
            output.push(Token::new(TokenType::Field, Span::new(bol, lnum, cnum, len)));
            buf.clear()
          } else {
            return Err(format!("expected s at {} and found {}", idx, source_vec[idx]))
          }
      } else if c == '"' {
        idx += 1;
          while source_vec.get(idx).is_some() && source_vec.get(idx) != Some(&'"') {
              buf.push(source_vec[idx]);
              idx += 1;
              len += 1;
          }
          idx += 1;
          output.push(Token::new(TokenType::Literal(Literal::String(buf.clone())), Span::new(bol, lnum, cnum, len)));
          buf.clear();
      } else if c == ';' {
        output.push(Token::new(TokenType::Semi, Span::new(bol, lnum, cnum, len)));
        idx += 1;
      } else if c == '.' {
        output.push(Token::new(TokenType::FullStop, Span::new(bol, lnum, cnum, len)));
        idx += 1;
      } else if c == ',' {
        output.push(Token::new(TokenType::Comma, Span::new(bol, lnum, cnum, len)));
        idx += 1;
      } else if c == '-' {
        idx += 1;
        if source_vec.get(idx) == Some(&'-') {
          idx += 1;
          is_comment = true;
          if source_vec.get(idx) == Some(&'-') {
            idx += 1;
            multiline = true;
          }
        } else {
          return Err(format!("invalid syntax: expected another / after {c} at {}", Span::new(bol, lnum, cnum, len)))
        }
      } else {
        return Err(format!("invalid syntax at {}: {}", Span::new(bol, lnum, cnum, len), c))
      }
    }
  }
  Ok(ParseBuffer::new(output))
}

pub fn operator_pass(mut buffer: ParseBuffer) -> Result<ParseBuffer, String> {
  if buffer.contains(TokenType::And) || buffer.contains(TokenType::Or) || buffer.contains(TokenType::Identifier(String::from("be"))) {
    let mut new_buffer: Vec<Token> = Vec::new();
    while buffer.is_next() {
      match buffer.parse() {
        Some(Token {_type: TokenType::And, span}) => {
          new_buffer.push(Token::new(TokenType::Operator(Operator::And), span))
        },
        Some(Token {_type: TokenType::Or, span}) => {
          new_buffer.push(Token::new(TokenType::Operator(Operator::Or), span))
        },
        Some(Token {_type: TokenType::Identifier(x), span: be_span}) if x == *"be" => {
          match buffer.parse() {
            Some(Token {_type: TokenType::Equal, span: fop_span}) => {
              match buffer.parse() {
                Some(Token {_type: TokenType::Identifier(x), span: to_span}) if x == *"to" => {
                  new_buffer.push(Token::new(TokenType::Operator(Operator::Eq), be_span + fop_span + to_span))
                },
                Some(token) => return Err(format!("expected to at {} and found {}", token.span, token._type)), 
                None => return Err("unexpected end of input".to_string())
              }
            },
            Some(Token {_type: TokenType::Unequal, span: equal_span}) => {
              match buffer.parse() {
                Some(Token {_type: TokenType::Identifier(x), span: to_span}) if x == *"to" => {
                  new_buffer.push(Token::new(TokenType::Operator(Operator::Neq), be_span + equal_span + to_span))
                },
                Some(token) => return Err(format!("expected to at {} and found {}", token.span, token._type)), 
                None => return Err("unexpected end of input".to_string())
              }
            }
            Some(Token {_type: TokenType::Greater, span: greater_span}) => {
              match buffer.parse() {
                Some(Token {_type: TokenType::Identifier(x), span: than_span}) if x == *"than" => {
                  match buffer.peek() {
                    Some(Token {_type: TokenType::Or, ..}) => {
                      let or_token = buffer.parse().unwrap();
                      match buffer.parse() {
                        Some(Token {_type: TokenType::Equal, span: equal_span}) => {
                          match buffer.parse() {
                            Some(Token {_type: TokenType::Identifier(x), span: to_span}) if x == *"to" => {
                              new_buffer.push(Token::new(TokenType::Operator(Operator::Ge), be_span + greater_span + than_span + or_token.span + equal_span + to_span))
                            },
                            Some(token) => return Err(format!("expected to at {} and found {}", token.span, token._type)), 
                            None => return Err("unexpected end of input".to_string())
                          }
                        },
                        Some(token) => return Err(format!("expected equal at {} and found {}", token.span, token._type)),
                        None => return Err("unexpected end of input".to_string())
                      }
                    },
                    Some(_) => {
                      new_buffer.push(Token::new(TokenType::Operator(Operator::Gt), be_span + greater_span + than_span));
                    },
                    None => break
                  }
                },
                Some(token) => return Err(format!("expected than at {} and found {}", token.span, token._type)),
                None => return Err("unexpected end of input".to_string())
              }
            },
            Some(Token {_type: TokenType::Less, span: less_span}) => {
              match buffer.parse() {
                Some(Token {_type: TokenType::Identifier(x), span: than_span}) if x == *"than" => {
                  match buffer.peek() {
                    Some(Token {_type: TokenType::Or, ..}) => {
                      let or_token = buffer.parse().unwrap();
                      match buffer.parse() {
                        Some(Token {_type: TokenType::Equal, span: equal_span}) => {
                          match buffer.parse() {
                            Some(Token {_type: TokenType::Identifier(x), span: to_span}) if x == *"to" => {
                              new_buffer.push(Token::new(TokenType::Operator(Operator::Le), be_span + less_span + than_span + or_token.span + equal_span + to_span))
                            },
                            Some(token) => return Err(format!("expected to at {} and found {}", token.span, token._type)), 
                            None => return Err("unexpected end of input".to_string())
                          }
                        },
                        Some(token) => return Err(format!("expected equal at {} and found {}", token.span, token._type)),
                        None => return Err("unexpected end of input".to_string())
                      }
                    },
                    Some(_) => {
                      new_buffer.push(Token::new(TokenType::Operator(Operator::Lt), be_span + less_span + than_span));
                    },
                    None => break
                  }
                },
                Some(token) => return Err(format!("expected than at {} and found {}", token.span, token._type)),
                None => return Err("unexpected end of input".to_string())
              } 
            }, 
            Some(Token {_type, span}) => return Err(format!("expected equal, greater or less at {} and found {}", span, x)),
            None => return Err("unexpected end of input".to_string())
          }
        },
        Some(token) => {
          new_buffer.push(token)
        }
        None => break
      }
    }
    Ok(ParseBuffer::new(new_buffer))
  } else {
    Ok(buffer)
  }
}

#[derive(Debug, Clone)]
pub struct Parser {
  buffer: ParseBuffer,
  idents: HashMap<String, Span>,
  output: Vec<Statement>
}

impl Display for Parser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();
        for n in &self.output {
          str.push_str(n.to_string().as_str());
          str.push('\n');
        }
        write!(f, "{}", str)
    }
}

impl Parser {
  pub fn new(buffer: ParseBuffer) -> Self {
    Self {
      buffer,
      idents: HashMap::new(),
      output: Vec::new()
    }
  }

  pub fn parse(&mut self) -> Result<(), String> {
    let mut stmt_buffer: Vec<Token> = Vec::new();
    while self.buffer.is_next() {
      match self.buffer.parse() {
        Some(Token {_type: TokenType::FullStop, ..}) => {
          self.output.push(Statement::parse(&mut stmt_buffer.clone().into(), &mut self.idents)?);
          stmt_buffer.clear()
        }
        Some(token) => stmt_buffer.push(token),
        None => break
      }
    }
    Ok(())
  }
}

#[derive(Debug, Clone)]
pub enum Statement {
  Assignment(String, Expression),
  ConstAssignment(String, Expression),
  Return(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
          Statement::Assignment(x, y) => write!(f, "Imbue {} with {}.", x, y),
          Statement::ConstAssignment(x, y) => write!(f, "Declare {} as {}.", x, y),
          Statement::Return(x) => write!(f, "bestow {}", x),
        }
    }
}

impl Statement {
  pub fn parse(buffer: &mut ParseBuffer, idents: &mut HashMap<String, Span>) -> Result<Statement, String> {
    match buffer.parse() {
      Some(Token {_type: TokenType::Assignment, ..}) => {
        match buffer.parse() {
          Some(Token {_type: TokenType::Identifier(ident), ..}) => {
            match buffer.parse() {
              Some(Token {_type: TokenType::Identifier(x), ..}) if x == *"with" => {
                let expr = Expression::parse(buffer, 0)?;
                Ok(Statement::Assignment(ident, expr))
              },
              Some(token) => Err(format!("expected with at {} and found {}", token.span, token._type)),
              None => Err("unexpected end of input".to_string())
            }
          },
          Some(token) => Err(format!("expected an identifier at {} and found {}", token.span, token._type)),
          None => Err("unexpected end of input".to_string())
        }
      },
      Some(Token {_type: TokenType::ConstAssignment, ..}) => {
        match buffer.parse() {
          Some(Token {_type: TokenType::Identifier(ident), ..}) => {
            match buffer.parse() {
              Some(Token {_type: TokenType::Identifier(x), ..}) if x == *"as" => {
                let expr = Expression::parse(buffer, 0)?;
                Ok(Statement::ConstAssignment(ident, expr))
              },
              Some(token) => Err(format!("expected with at {} and found {}", token.span, token._type)),
              None => Err("unexpected end of input".to_string())
            }
          },
          Some(token) => Err(format!("expected an identifier at {} and found {}", token.span, token._type)),
          None => Err("unexpected end of input".to_string())
        }
      },
      Some(Token {_type: TokenType::Return, ..}) => {
        let expr = Expression::parse(buffer, 0)?;
        Ok(Statement::Return(expr))
      },
      Some(token) => Err(format!("expected imbue, declare or bestow at {} and found {}", token.span, token._type)),
      None => Err("unexpected end of input".to_string())
    }
  }
}

#[derive(Debug, Clone)]
pub enum Expression {
  Literal(Literal),
  Field(Box<Expression>, String),
  Identifier(String),
  Operation(Box<Expression>, Operator, Box<Expression>),
  FunctionCall(String, Vec<Expression>)
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
          Expression::Literal(x) => write!(f, "{x}"),
          Expression::Field(x, y) => write!(f, "{}'s {}", x, y),
          Expression::Identifier(x) => write!(f, "{x}"),
          Expression::Operation(x, y, z) => write!(f, "({} {} {})", x, y, z),
          Expression::FunctionCall(x, y) => {
            let mut args = String::new();
            for n in y {
              args.push_str(n.to_string().as_str());
              args.push_str(", ")
            }
            write!(f, "invoke {}, which takes {}", x, args)
          }
        }
    }
}

impl Expression {
  pub fn first_expr(&self) -> Expression {
    match self {
      Expression::Operation(x, _, _) => *x.to_owned(),
      _ => panic!("nuh uh")
    }
  }

  pub fn last_expr(&self) -> Expression {
    match self {
      Expression::Operation(_, _, x) => *x.to_owned(),
      _ => panic!("nuh uh")
    }
  }

  pub fn op(&self) -> Operator {
    match self {
      Expression::Operation(_, x, _) => x.to_owned(),
      _ => panic!("nuh uh")
    }
  }

  pub fn parse(buffer: &mut ParseBuffer, nested: u8) -> Result<Expression, String> {
    let mut expr = match buffer.parse() {
      Some(Token {_type: TokenType::Literal(x), ..}) => Expression::Literal(x),
      Some(Token {_type: TokenType::Identifier(x), ..}) => Expression::Identifier(x),
      Some(Token {_type: TokenType::FunctionCall, ..}) => {
        todo!()
      },
      Some(token) => return Err(format!("expected an identifier, literal or invoke at {} and found {}", token.span, token._type)),
      None => return Err("unexpected end of input".to_string())
    };
    while buffer.is_next() {
      if buffer.peek_type() == Some(TokenType::Field) {
        buffer.parse();
        match buffer.parse() {
          Some(Token {_type: TokenType::Identifier(x), ..}) => {
            let old_expr = expr.clone();
            expr = Expression::Field(Box::new(old_expr), x)
          }
          Some(token) => return Err(format!("expected an identifier at {} and found {}", token.span, token._type)),
          None => return Err("unexpected end of input".to_string())
        }
      } else {
        match buffer.peek_type() {
          Some(TokenType::Operator(operator)) => {
            buffer.parse();
            let next: Expression = match buffer.parse() {
              Some(Token {_type: TokenType::Literal(x), ..}) => Expression::Literal(x),
              Some(Token {_type: TokenType::Identifier(x), ..}) => Expression::Identifier(x),
              Some(token) => return Err(format!("expected an identifier or literal at {} and found {}", token.span, token._type)),
              None => return Err("unexpected end of input".to_string())
            };
            let old_expr = expr.clone();
            if let Expression::Operation(x, y, z) = old_expr.clone() {
              match (y.priority(), operator.priority()) {
                (OperatorPriority::AddSub, OperatorPriority::MulDiv) => {
                  expr = Expression::Operation(x, y, Box::new(Expression::Operation(z, operator, Box::new(next))))
                },
                (OperatorPriority::AddSub | OperatorPriority::MulDiv, _) => {
                  expr = Expression::Operation(Box::new(old_expr), operator, Box::new(next))
                },
                (OperatorPriority::Eq, OperatorPriority::Eq) => {
                  return Err("do not chain comparison operators".to_string())
                },
                (OperatorPriority::Eq | OperatorPriority::AndOr, OperatorPriority::AndOr) => {
                  expr = Expression::Operation(Box::new(old_expr), operator, Box::new(next))
                },
                (OperatorPriority::Eq | OperatorPriority::AndOr, _) => {
                  expr = Expression::Operation(x, y, Box::new(Expression::Operation(z, operator, Box::new(next))))
                }
              }
            } else {
              expr = Expression::Operation(Box::new(old_expr), operator, Box::new(next))
            }
          }
          Some(_) => break,
          None => return Err("unexpected end of input".to_string())
        }
      }
    }
    Ok(expr)
  }
}

fn create_operation(old: Expression, operator: Operator, new: Expression) -> Result<Expression, String> {
  let output: Expression = match &old {
    Expression::Operation(x, y, z) => {
      let mut op = y.to_owned();
      let mut expr: Expression = *z.to_owned();
      while op.priority() == OperatorPriority::AndOr {
          expr = expr.last_expr();
          op = expr.op();
      }
      match (op.priority(), operator.priority()) {
        (OperatorPriority::AndOr, OperatorPriority::AndOr) => todo!(),
        (OperatorPriority::AndOr, OperatorPriority::AddSub) => todo!(),
        (OperatorPriority::AndOr, OperatorPriority::MulDiv) => todo!(),
        (OperatorPriority::AndOr, OperatorPriority::Eq) => todo!(),
        (OperatorPriority::AddSub, OperatorPriority::AndOr) => todo!(),
        (OperatorPriority::AddSub, OperatorPriority::AddSub) => todo!(),
        (OperatorPriority::AddSub, OperatorPriority::MulDiv) => todo!(),
        (OperatorPriority::AddSub, OperatorPriority::Eq) => todo!(),
        (OperatorPriority::MulDiv, OperatorPriority::AndOr) => todo!(),
        (OperatorPriority::MulDiv, OperatorPriority::AddSub) => todo!(),
        (OperatorPriority::MulDiv, OperatorPriority::MulDiv) => todo!(),
        (OperatorPriority::MulDiv, OperatorPriority::Eq) => todo!(),
        (OperatorPriority::Eq, OperatorPriority::AndOr) => todo!(),
        (OperatorPriority::Eq, OperatorPriority::AddSub) => todo!(),
        (OperatorPriority::Eq, OperatorPriority::MulDiv) => todo!(),
        (OperatorPriority::Eq, OperatorPriority::Eq) => todo!(),
    }
      Expression::Operation(Box::new(old), operator, Box::new(new))
    },
    _ => {
      Expression::Operation(Box::new(old), operator, Box::new(new))
    }
  };
  Ok(output)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
  Eq,
  Neq,
  Gt,
  Ge,
  Lt,
  Le,
  And,
  Or,
  Add,
  Sub,
  Mul,
  Div
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Operator::Eq => String::from("be equal to"),
            Operator::Neq => String::from("be unequal to"),
            Operator::Gt => String::from("be greater than"),
            Operator::Ge => String::from("be greater than or equal to"),
            Operator::Lt => String::from("be less than"),
            Operator::Le => String::from("be less than or equal to"),
            Operator::And => String::from("and"),
            Operator::Or => String::from("or"),
            Operator::Add => String::from("plus"),
            Operator::Sub => String::from("minus"),
            Operator::Mul => String::from("times"),
            Operator::Div => String::from("over"),
        };
        write!(f, "{}", string)
    }
}

impl Operator {
  fn priority(&self) -> OperatorPriority {
    use Operator as o;
    use OperatorPriority as op;
    match self {
      o::Add | o::Sub => op::AddSub,
      o::Mul | o::Div => op::MulDiv,
      o::And | o::Or => op::AndOr,
      _ => op::Eq
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum OperatorPriority {
  AndOr,
  AddSub,
  MulDiv,
  Eq
}