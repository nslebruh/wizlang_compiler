#[macro_use]
mod macros;
mod main2;
mod main3;
mod main4;
mod tokens;

use std::{ops::Deref, fmt::Display};

use clap::Parser;

use crate::{main2::Lexer, main4::Parser as Parser2};

trait ToRust {
    fn to_rust(&self) -> String;
}

trait ToJavascript {
    fn to_javascript(&self) -> String;
}


#[derive(Parser, Debug)]
struct Cli {
    path: std::path::PathBuf
}

#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash, PartialOrd, Ord)]
struct Span {
    start: usize,
    end: usize
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Lit {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool)
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(x) => write!(f, "{}", x),
            Lit::Float(x) => write!(f, "{}", x),
            Lit::String(x) => write!(f, "{}", x),
            Lit::Bool(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd)]
enum TokenType {
    Return,
    Lit(Lit),
    Semi,
    Function,
    Ident(Ident),
    Assign(bool),
    Plus,
    Minus,
    Times,
    Divided,
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
    Comma,
    Then,
    Invoke,
    Field,
    Or,
    Conclude,
    Becometh,
    Struct,
    Unequal
}

#[derive(Debug, Clone)]
struct Token {
    span: Span,
    ty: TokenType,
}

impl Token {
    pub fn new(start: usize, end: usize, ty: TokenType) -> Self {
        Self {span: Span {start, end}, ty}
    }
}

fn string_to_tokens(content: String) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut buf = String::new();
    let mut start: usize;
    let mut idx = 0;
    let str = content.chars().collect::<Vec<char>>();
    while idx < str.len() {
        start = idx + 1;
        let c = str[idx];
        if c.is_ascii_alphabetic() {
            buf.push(c);
            idx += 1;
            while str.get(idx).is_some() {
                if str[idx].is_ascii_alphanumeric() {
                    buf.push(str[idx]);
                    idx += 1;
                } else {
                    break
                }
            }
            println!("buf: {}", buf);
            if buf == "bestow" {
                tokens.push(Token::new(start, idx, TokenType::Return));
                buf.clear();
            } else if buf == "imbue" || buf == "Imbue" {
                tokens.push(Token::new(start, idx, TokenType::Assign(true)));
                buf.clear();
            } else if buf == "declare" || buf == "Declare" {
                tokens.push(Token::new(start, idx, TokenType::Assign(false)));
                buf.clear();
            } else if buf == "conjure" {
                tokens.push(Token::new(start, idx, TokenType::Function));
                buf.clear();
            } else if buf == "plus" {
                tokens.push(Token::new(start, idx, TokenType::Plus));
                buf.clear();
            } else if buf == "minus" {
                tokens.push(Token::new(start, idx, TokenType::Minus));
                buf.clear();
            } else if buf == "times" {
                tokens.push(Token::new(start, idx, TokenType::Times));
                buf.clear();
            } else if buf == "over" {
                tokens.push(Token::new(start, idx, TokenType::Divided));
                buf.clear();
            } else if buf == "be" {
                tokens.push(Token::new(start, idx, TokenType::Be));
                buf.clear();
            } else if buf == "equal" {
                tokens.push(Token::new(start, idx, TokenType::Equal));
                buf.clear();
            } else if buf == "to" {
                tokens.push(Token::new(start, idx, TokenType::To));
                buf.clear();
            } else if buf == "not" {
                tokens.push(Token::new(start, idx, TokenType::Not));
                buf.clear();
            } else if buf == "and" {
                tokens.push(Token::new(start, idx, TokenType::And));
                buf.clear();
            } else if buf == "greater" {
                tokens.push(Token::new(start, idx, TokenType::Greater));
                buf.clear();
            } else if buf == "less" {
                tokens.push(Token::new(start, idx, TokenType::Less));
                buf.clear();
            } else if buf == "than" {
                tokens.push(Token::new(start, idx, TokenType::Than));
                buf.clear();
            } else if buf == "should" || buf == "Should" {
                tokens.push(Token::new(start, idx, TokenType::If));
                buf.clear();
            } else if buf == "otherwise" || buf == "Otherwise" {
                tokens.push(Token::new(start, idx, TokenType::Else));
                buf.clear();
            } else if buf == "then" {
                tokens.push(Token::new(start, idx, TokenType::Then));
                buf.clear();
            } else if buf == "invoke" || buf == "Invoke" {
                tokens.push(Token::new(start, idx, TokenType::Invoke));
                buf.clear();
            } else if buf == "or" {
                tokens.push(Token::new(start, idx, TokenType::Or));
                buf.clear();
            } else if buf == "conclude" || buf == "Conclude" {
                tokens.push(Token::new(start, idx, TokenType::Conclude));
                buf.clear();
            } else if buf == "becometh" {
                tokens.push(Token::new(start, idx, TokenType::Becometh));
                buf.clear();
            } else if buf == "envision" || buf == "Envision" {
                tokens.push(Token::new(start, idx, TokenType::Struct));
                buf.clear();
            } else if buf == "unequal" {
                tokens.push(Token::new(start, idx, TokenType::Unequal));
                buf.clear();
            } else {
                tokens.push(Token::new(start, idx, TokenType::Ident(Ident(buf.clone()))));
                buf.clear();
            }
        } else if c.is_ascii_digit() {
            buf.push(c);
            idx += 1;
            let mut decimal = false;
            while str.get(idx).is_some() {
                if str[idx].is_ascii_digit() || (str[idx] == '.' && !decimal) {
                    if str[idx] == '.' {
                        if !decimal {
                            decimal = true;
                        } else {
                            buf.push(str[idx]);
                            println!("buf: {}", buf);
                            break
                        }
                    } 
                    buf.push(str[idx]);
                    idx += 1;
                } else {
                    println!("buf: {}", buf);
                    break
                }
            }
            if buf.ends_with('.') {
                buf.remove(buf.len() - 1);
                println!("removed buf: {}", buf);
                idx -= 1;
                tokens.push(Token::new(start, idx, TokenType::Lit(if buf.contains('.') {Lit::Float(buf.parse::<f64>().unwrap())} else {Lit::Int(buf.parse::<i64>().unwrap())})));   
                buf.clear();  
            } else {
                tokens.push(Token::new(start, idx, TokenType::Lit(if buf.contains('.') {Lit::Float(buf.parse::<f64>().unwrap())} else {Lit::Int(buf.parse::<i64>().unwrap())})));
                buf.clear();
            }

        } else if c.is_ascii_whitespace() {
            idx += 1;
            continue
        } else if c == '.' {
            idx += 1;
            tokens.push(Token::new(start, idx, TokenType::Semi));
        } else if c == '\'' {
            if str.get(idx + 1) == Some(&'s') {
                idx += 2;
                tokens.push(Token::new(start, idx, TokenType::Field));
            } else {
               return Err(format!("expected a s and found {:?}", str.get(idx + 1))) 
            }
        } else if c == '"' {
            idx += 1;
            while str.get(idx).is_some() && str.get(idx) != Some(&'"') {
                buf.push(str[idx]);
                idx += 1;
            }
            idx += 1;
            tokens.push(Token::new(start, idx, TokenType::Lit(Lit::String(buf.clone()))));
            buf.clear();
        } else if c == ',' {
            idx += 1;
            tokens.push(Token::new(start, idx, TokenType::Comma));
        } else {
            return Err(format!("invalid syntax at {}: {}", idx, c))
        }
    }
    Ok(tokens)
}


#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone)]
struct Ident(String);

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Ident {
    pub fn new(val: impl Into<String>) -> Self {
        Self(val.into())
    }
}

#[derive(Debug, Clone)]
enum Type {
    Number,
    Float,
    Bool,
    String,
    Custom(String)
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "i64"),
            Type::Float => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "String"),
            Type::Custom(x) => write!(f, "{x}"),
        }
    }
}

#[derive(Debug, Clone)]
struct Invoke {
    ident: Ident,
    args: Vec<Expr>
}

impl Display for Invoke {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut args = String::new();
        for n in self.args.clone() {
            args.push_str(n.to_string().as_str());
            args.push_str(", ");
        }
        write!(f, "{}({})", self.ident, args)
    }
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
        }
    }
}

impl ToJavascript for Operator {
    fn to_javascript(&self) -> String {
        self.to_string()
    }
}

impl ToRust for Operator {
    fn to_rust(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, Clone)]
enum EqOperator {
    Eq,
    Ne,
    Ge,
    Gt,
    Lt,
    Le,
    And,
    Or,
}

impl Display for EqOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EqOperator::Eq => write!(f, "=="),
            EqOperator::Ne => write!(f, "!="),
            EqOperator::Ge => write!(f, ">="),
            EqOperator::Gt => write!(f, ">"),
            EqOperator::Lt => write!(f, "<"),
            EqOperator::Le => write!(f, "<="),
            EqOperator::And => write!(f, "&&"),
            EqOperator::Or => write!(f, "||"),
        }
    }
}

impl ToJavascript for EqOperator {
    fn to_javascript(&self) -> String {
        match self {
            EqOperator::Eq => String::from("==="),
            EqOperator::Ne => String::from("!=="),
            x => x.to_string()
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Lit(Lit),
    Ident(Ident),
    Invoke(Invoke),
    Field(Box<Expr>, Box<Expr>),
    EqOp(Box<Expr>, EqOperator, Box<Expr>),
    Op(Box<Expr>, Operator, Box<Expr>)
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Lit(x) => write!(f, "{}", x),
            Expr::Ident(x) => write!(f, "{}", x),
            Expr::Invoke(x) => write!(f, "{}", x),
            Expr::Field(x, y) => write!(f, "{}.{}", x, y),
            Expr::EqOp(x, y, z) => write!(f, "{} {} {}", x, y, z),
            Expr::Op(x, y, z) => write!(f, "{} {} {}", x, y, z),
        }
    }
}

impl ToRust for Expr {
    fn to_rust(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, Clone)]
struct Assignment {
    mutable: bool,
    ident: Ident,
    expr: Expr,
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.mutable {
            write!(f, "let mut {} = {};", self.ident, self.expr)
        } else {
            write!(f, "let {} = {};", self.ident, self.expr)
        }
    }
}

impl ToJavascript for Assignment {
    fn to_javascript(&self) -> String {
        format!("let {} = {};", self.ident, self.expr)
    }
}

#[derive(Debug, Clone)]
struct IfElse {
    predicate: Expr,
    stmts: Vec<Stmt>,
    _else: Option<Box<IfElse>>
}

impl Display for IfElse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut stmts = String::new();
        for n in self.stmts.clone() {
            stmts.push_str(n.to_string().as_str())
        }
        if let Some(x) = self._else.clone() {
            write!(f, "if {} {{{}}} else {}", self.predicate, stmts, x)
        } else {
            write!(f, "if {} {{{}}}", self.predicate, stmts)
        }
    }
}

impl ToJavascript for IfElse {
    fn to_javascript(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, Clone)]
struct Function {
    ident: Ident,
    args: Option<Vec<(Ident, Type)>>,
    stmts: Option<Vec<Stmt>>
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut args = String::new();
        if let Some(x) = self.args.clone() {
            for n in x {
                args.push_str(format!("{}: {},", n.0, n.1).as_str())
            }
        }
        let stmts = String::new();
        if let Some(x) = self.stmts.clone() {
            for n in x {
                args.push_str(n.to_string().as_str())
            }
        }
        write!(f, "fn {}({}){{{}}}", self.ident, args, stmts)
    }
}

impl ToJavascript for Function {
    fn to_javascript(&self) -> String {
        let mut args = String::new();
        if let Some(x) = self.args.clone() {
            for n in x {
                args.push_str(format!("{}: {},", n.0, n.1).as_str())
            }
        }
        let stmts = String::new();
        if let Some(x) = self.stmts.clone() {
            for n in x {
                args.push_str(n.to_string().as_str())
            }
        }
        format!("function {}({}) {{{}}}", self.ident, args, stmts)
    }
}

#[derive(Debug, Clone)]
struct FunctionCall {
    expr: Expr,
    args: Vec<Expr>
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut args = String::new();
        for n in self.args.clone() {
            args.push_str(n.to_string().as_str());
            args.push(',')
        }
        write!(f, "{}({});", self.expr, args)
    }
}

impl ToJavascript for FunctionCall {
    fn to_javascript(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, Clone)]
struct Becometh {
    ident: Ident,
    expr: Expr
}

impl Display for Becometh {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {};", self.ident, self.expr)
    }
}

impl ToJavascript for Becometh {
    fn to_javascript(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, Clone)]
enum Stmt {
    Assignment(Assignment),
    IfElse(IfElse),
    Function(Function),
    FunctionCall(FunctionCall),
    Becometh(Becometh),
    Return(Expr),
    Conclude,
    No
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Assignment(x) => write!(f, "{}", x),
            Stmt::IfElse(x) => write!(f, "{}", x),
            Stmt::Function(x) => write!(f, "{}", x),
            Stmt::FunctionCall(x) => write!(f, "{}", x),
            Stmt::Becometh(x) => write!(f, "{}", x),
            Stmt::Return(x) => write!(f, "return {}", x),
            Stmt::Conclude => write!(f, ""),
            Stmt::No => unreachable!(),
        }
    }
}

impl ToJavascript for Stmt {
    fn to_javascript(&self) -> String {
        match self {
            Stmt::No => unreachable!(),
            Stmt::Assignment(x) => x.to_javascript(),
            Stmt::IfElse(x) => x.to_javascript(),
            Stmt::Function(x) => x.to_javascript(),
            Stmt::FunctionCall(x) => x.to_javascript(),
            Stmt::Becometh(x) => x.to_javascript(),
            Stmt::Return(x) => format!("return {};", x),
            Stmt::Conclude => "".to_string()
            
        }
    }
}

fn parse_expr3(tokens: &[Token], idx: &mut usize) -> Result<Expr, String> {
    let rest: Vec<Token> = if tokens.len() == 2 {
        vec![tokens.get(1).unwrap().to_owned()]
    } else {
        tokens.get(*idx..tokens.len() - 1).unwrap().to_owned()
    };
    println!("len of rest: {}, rest: {:?}", rest.len(), rest);
    let mut output: Option<Expr> = None;
    let mut index: usize = 0;
    if rest[index].ty == TokenType::Invoke {
        if let TokenType::Ident(ident) = &rest[index + 1].ty {
            index += 2;
            if rest[index].ty == TokenType::Comma {
                
            } else {
                output = Some(Expr::Invoke(Invoke {ident: ident.to_owned(), args: Vec::new()}))
            }
        } else {
            return Err(format!("expected an identifier at {} and found {:?}", rest[index + 1].span.start, rest[index + 1].ty))
        }
    }
    *idx += index;
    output.ok_or(String::from("unexpected end of input"))
}

fn parse_expr2(tokens: &[Token], idx: &mut usize) -> Result<Expr, String> {
    let rest: Vec<Token> = if tokens.len() == 2 {
        vec![tokens.get(1).unwrap().to_owned()]
    } else {
        tokens.get(*idx..tokens.len() - 1).unwrap().to_owned()
    };
    println!("len of rest: {}, rest: {:?}", rest.len(), rest);
    let mut output: Option<Expr> = None;
    let mut index = 0;
    match &rest[index].ty {
        TokenType::Ident(x) => {
            output = Some(Expr::Ident(x.clone()))
        },
        TokenType::Lit(x) => {
            output = Some(Expr::Lit(x.clone()))
        },
        _ => return Err(format!("expected ident or lit at {} and found {:?}", rest[index].span.start, rest[index].ty))
    }
    index += 1;
    if index == rest.len() {
        *idx += index;
        return Ok(output.unwrap())
    }
    while let Some(rest2) = rest.get(index..index + 2) {
        if !(if_or!(rest2[0].ty, TokenType::Field, TokenType::Plus, TokenType::Minus, TokenType::Minus, TokenType::Divided, TokenType::And, TokenType::Or, TokenType::Be)) {
            return Err(format!("expected an operator at {} and found {:?}", rest2[0].span.start, rest2[0].ty))
        }
        if rest2[0].ty == TokenType::Be {
            if let Some(rest3) = rest.get(index..index + 7) {
                if rest3[1].ty == TokenType::Greater && rest3[2].ty == TokenType::Than {
                    match (rest3[3].ty.clone(), rest3[4].ty.clone(), rest[5].ty.clone()) {
                        (TokenType::Ident(x), _, _) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Gt, Box::new(Expr::Ident(x))));
                            index += 4;
                        },
                        (TokenType::Lit(x), _, _) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Gt, Box::new(Expr::Lit(x))));
                            index += 4;
                        },
                        (TokenType::Or, TokenType::Equal, TokenType::To) => {
                            match rest3[6].ty.clone() {
                                TokenType::Lit(x) => {
                                    let expr = output.unwrap();
                                    output = Some(Expr::EqOp(Box::new(expr), EqOperator::Ge, Box::new(Expr::Lit(x))));
                                    index += 7;
                                },
                                TokenType::Ident(x) => {
                                    let expr = output.unwrap();
                                    output = Some(Expr::EqOp(Box::new(expr), EqOperator::Ge, Box::new(Expr::Ident(x))));
                                    index += 7;
                                },
                                _ => return Err(format!("expected a literal or identifier at {} and found {:?}", rest3[6].span.start, rest3[6].ty))
                            }
                        },
                        (_, _, _) => {
                            return Err(format!("expected a literal, ident or \"or equal to\" at {} and found {:?}", rest3[3].span.start, rest3[3].ty))
                        }
                    }
                } else if rest3[1].ty == TokenType::Less && rest3[2].ty == TokenType::Than {
                    match (rest3[3].ty.clone(), rest3[4].ty.clone(), rest[5].ty.clone()) {
                        (TokenType::Ident(x), _, _) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Lt, Box::new(Expr::Ident(x))));
                            index += 4;
                        },
                        (TokenType::Lit(x), _, _) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Lt, Box::new(Expr::Lit(x))));
                            index += 4;
                        },
                        (TokenType::Or, TokenType::Equal, TokenType::To) => {
                            match rest3[6].ty.clone() {
                                TokenType::Lit(x) => {
                                    let expr = output.unwrap();
                                    output = Some(Expr::EqOp(Box::new(expr), EqOperator::Le, Box::new(Expr::Lit(x))));
                                    index += 7;
                                },
                                TokenType::Ident(x) => {
                                    let expr = output.unwrap();
                                    output = Some(Expr::EqOp(Box::new(expr), EqOperator::Le, Box::new(Expr::Ident(x))));
                                    index += 7;
                                },
                                _ => return Err(format!("expected a literal or identifier at {} and found {:?}", rest3[6].span.start, rest3[6].ty))
                            }
                        },
                        (_, _, _) => {
                            return Err(format!("expected a literal, ident or \"or equal to\" at {} and found {:?}", rest3[3].span.start, rest3[3].ty))
                        }
                    }
                } else if rest3[1].ty == TokenType::Equal && rest3[2].ty == TokenType::To {
                    match rest3[3].ty.clone() {
                        TokenType::Lit(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Eq, Box::new(Expr::Lit(x))));
                            index += 4;
                        },
                        TokenType::Ident(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Eq, Box::new(Expr::Ident(x))));
                            index += 4;
                        },
                        _ => {
                            return Err(format!("expected a literal or identifier at {} and found {:?}", rest3[3].span.start, rest3[3].ty))
                        }
                    }
                } else if rest3[1].ty == TokenType::Unequal && rest3[2].ty == TokenType::To {
                    match rest3[3].ty.clone() {
                        TokenType::Lit(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Ne, Box::new(Expr::Lit(x))));
                            index += 4;
                        },
                        TokenType::Ident(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Ne, Box::new(Expr::Ident(x))));
                            index += 4;
                        },
                        _ => {
                            return Err(format!("expected a literal or identifier at {} and found {:?}", rest3[3].span.start, rest3[3].ty))
                        }
                    }
                } else {
                    return Err(format!("expected less than, greater than, equal to at {} and found {:?}", rest3[1].span.start, rest3[1].ty))
                }
            } else if let Some(rest3) = rest.get(index..index + 4) {
                if rest3[1].ty == TokenType::Greater && rest3[2].ty == TokenType::Than {
                    match rest3[3].ty.clone() {
                        TokenType::Lit(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Gt, Box::new(Expr::Lit(x))));
                            index += 4
                        },
                        TokenType::Ident(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Gt, Box::new(Expr::Ident(x))));
                            index += 4
                        },
                        _ => {
                            return Err(format!("expected a literal or an identifier at {} and found {:?}", rest3[3].span.start, rest3[3].ty))
                        }
                    }
                } else if rest3[1].ty == TokenType::Less && rest3[2].ty == TokenType::Than {
                    match rest3[3].ty.clone() {
                        TokenType::Lit(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Lt, Box::new(Expr::Lit(x))));
                            index += 4
                        },
                        TokenType::Ident(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Lt, Box::new(Expr::Ident(x))));
                            index += 4
                        },
                        _ => {
                            return Err(format!("expected a literal or an identifier at {} and found {:?}", rest3[3].span.start, rest3[3].ty))
                        }
                    }
                } else if rest3[1].ty == TokenType::Equal && rest3[2].ty == TokenType::To {
                    match rest3[3].ty.clone() {
                        TokenType::Lit(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Eq, Box::new(Expr::Lit(x))));
                            index += 4
                        },
                        TokenType::Ident(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Eq, Box::new(Expr::Ident(x))));
                            index += 4
                        },
                        _ => {
                            return Err(format!("expected a literal or an identifier at {} and found {:?}", rest3[3].span.start, rest3[3].ty))
                        }
                    }
                } else if rest3[1].ty == TokenType::Unequal && rest3[2].ty == TokenType::To {
                    match rest3[3].ty.clone() {
                        TokenType::Lit(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Ne, Box::new(Expr::Lit(x))));
                            index += 4;
                        },
                        TokenType::Ident(x) => {
                            let expr = output.unwrap();
                            output = Some(Expr::EqOp(Box::new(expr), EqOperator::Ne, Box::new(Expr::Ident(x))));
                            index += 4;
                        },
                        _ => {
                            return Err(format!("expected a literal or identifier at {} and found {:?}", rest3[3].span.start, rest3[3].ty))
                        }
                    }
                } else {
                    return Err(format!("expected less than, greater than, equal to at {} and found {:?}", rest3[1].span.start, rest3[1].ty))
                }
            } else {
                return Err(String::from("unexpected end of input"))
            }
        } else {
            match &rest2[1].ty {
                TokenType::Lit(_) => {},
                TokenType::Ident(_) => {},
                _ => return Err(format!("expected an identifier or a literal at {} and found {:?}", rest2[1].span.start, rest2[1].ty))
            }
            let expr = output.unwrap();
            if rest2[0].ty == TokenType::Field {
                if let Expr::EqOp(x, y, z) = expr {
                    output = Some(Expr::EqOp(x, y, Box::new(Expr::Field(z, Box::new(match rest2[1].ty.clone() {
                        TokenType::Lit(x) => Expr::Lit(x),
                        TokenType::Ident(x) => Expr::Ident(x),
                        _ => return Err(format!("expected an identifier or a literal at {} and found {:?}", rest2[1].span.start, rest2[1].ty))
                    })))));
                } else {
                    output = Some(Expr::Field(Box::new(expr), Box::new(match rest2[1].ty.clone() {
                        TokenType::Lit(x) => Expr::Lit(x),
                        TokenType::Ident(x) => Expr::Ident(x),
                        _ => return Err(format!("expected an identifier or a literal at {} and found {:?}", rest2[1].span.start, rest2[1].ty))
                    })))  
                }
            } else if rest2[0].ty == TokenType::And || rest2[0].ty == TokenType::Or {
                output = Some(Expr::EqOp(Box::new(expr), match rest2[0].ty.clone() {
                    TokenType::And => EqOperator::And,
                    TokenType::Or => EqOperator::Or,
                    _ => return Err(format!("expected and or or at {} and found {:?}", rest2[0].span.start, rest2[0].ty))
                }, Box::new(match rest2[1].ty.clone() {
                    TokenType::Lit(x) => Expr::Lit(x),
                    TokenType::Ident(x) => Expr::Ident(x),
                    _ => return Err(format!("expected an identifier or a literal at {} and found {:?}", rest2[1].span.start, rest2[1].ty))
                })))
            } else if let Expr::EqOp(x, y, z) = expr {
                output = Some(Expr::EqOp(x, y, Box::new(Expr::Op(z, match rest2[0].ty.clone() {
                    TokenType::Plus => Operator::Add,
                    TokenType::Minus => Operator::Sub,
                    TokenType::Times => Operator::Mul,
                    TokenType::Divided => Operator::Div,
                    _ => return Err(format!("expected an operator at {} and found {:?}", rest2[0].span.start, rest2[0].ty))
                }, Box::new(match rest2[1].ty.clone() {
                    TokenType::Lit(x) => Expr::Lit(x),
                    TokenType::Ident(x) => Expr::Ident(x),
                    _ => return Err(format!("expected an identifier or a literal at {} and found {:?}", rest2[1].span.start, rest2[1].ty))
                })))))
            } else {
                output = Some(Expr::Op(Box::new(expr), match rest2[0].ty.clone() {
                    TokenType::Plus => Operator::Add,
                    TokenType::Minus => Operator::Sub,
                    TokenType::Times => Operator::Mul,
                    TokenType::Divided => Operator::Div,
                    _ => return Err(format!("expected an operator at {} and found {:?}", rest2[0].span.start, rest2[0].ty))
                }, Box::new(match rest2[1].ty.clone() {
                    TokenType::Lit(x) => Expr::Lit(x),
                    TokenType::Ident(x) => Expr::Ident(x),
                    _ => return Err(format!("expected an identifier or a literal at {} and found {:?}", rest2[1].span.start, rest2[1].ty))
                })));
            }
            index += 2;
        }
    }
    *idx += index;
    output.ok_or(String::from("unexpected end of input"))
}

fn get_stmt(tokens: &[Token], idx: &mut usize, depth: u32) -> Result<Stmt, String> {
    // println!("tokens len: {}, tokens: {:?}", tokens.len(), tokens);
    let mut unchecked: Vec<Token> = Vec::new();
    let mut output = Stmt::No;
    while tokens.get(*idx).is_some() {
        println!("token: {:?}", tokens[*idx]);
        if tokens[*idx].ty == TokenType::Semi {
            unchecked.push(tokens[*idx].clone());
            *idx += 1;
            break
        } else {
            unchecked.push(tokens[*idx].clone());
            *idx += 1;
        }
    }
    println!("unchecked: {:?}", unchecked);
    let mut x = 0;
    match &unchecked[x].ty {
        TokenType::Return => {
            x += 1;
            let expr = parse_expr2(&unchecked, &mut x)?;
            if unchecked[x].ty == TokenType::Semi {
                output = Stmt::Return(expr);
            } else {
                return Err(format!("expected a . at {} and found {:?}", unchecked[x].span.start, unchecked[x].ty))
            }
        },
        TokenType::Assign(mutt) => {
            x += 1;
            let ident: Ident;
            if let Some(y) = unchecked.get(x) {
                if let TokenType::Ident(iden) = y.ty.clone() {
                    ident = iden;
                    x += 1;
                    
                } else {
                    return Err(format!("expected a identifier at {} and found {:?}", y.span.start, y.ty))
                }
            } else {
                return Err(String::from("unexpected end of input"))
            }
            if let Some(y) = unchecked.get(x) {
                if y.ty == TokenType::Ident(Ident::new("as")) || y.ty == TokenType::Ident(Ident::new("with")) {
                    x += 1;
                } else {
                    return Err(format!("expected as or with at {} and found {:?}", y.span.start, y.ty))
                }
            }
            let expr = parse_expr2(&unchecked, &mut x)?;
            if unchecked[x].ty == TokenType::Semi {
                output = Stmt::Assignment(Assignment { ident, expr, mutable: *mutt });
            } else {
                return Err(format!("expected a . at {} and found {:?}", unchecked[x].span.start, unchecked[x].ty))
            }
        },
        TokenType::If => {
            return Err(String::from("have not done this yet"))
        },
        TokenType::Function => {
            x += 1;
            let ident: Ident;
            if let TokenType::Ident(y) = unchecked[x].ty.clone() {
                ident = y;
            } else {
                return Err(format!("expected an indentifier at {} and found {:?}", unchecked[x].span.start, unchecked[x].ty))
            }
            //return Err(String::from("have not done this yet"))
        },
        TokenType::Ident(ident) => {
            x += 1;
            if unchecked[x].ty == TokenType::Becometh {
                x += 1;
                let expr = parse_expr2(&unchecked, &mut x)?;
                return Ok(Stmt::Becometh(Becometh {ident: ident.to_owned(), expr}))
            } else {
                return Err(format!("expected becometh at {} and found {:?}", unchecked[x].span.start, unchecked[x].ty))
            }
        },
        TokenType::Invoke => {
            return Err(String::from("have not done this yet"))
        },
        TokenType::Conclude => {
            x += 1;
            if unchecked[x].ty == TokenType::Semi {
                output = Stmt::Conclude;
            } else {
                return Err(format!("expected a . at {} and found {:?}", unchecked[x].span.start, unchecked[x].ty))
            }
        }
        //TokenType::Struct => {
        //    x += 1;
        //    if unchecked[x].ty == TokenType::Ident(Ident::new("a")) || unchecked[x].ty == TokenType::Ident(Ident::new("an")) {
        //        x += 1;
        //        let an = unchecked[x].to_owned();
        //        if let TokenType::Ident(x) = unchecked[x].ty.clone() {
        //            if x.starts_with(['a', 'e', 'i', 'o', 'u']) {
        //                if an.ty == 
        //            }
        //        } else {
        //            return Err(format!("expected identifier at {} and found {:?}", unchecked[x].span.start, unchecked[x].ty))
        //        }
        //    }
        //    
//
        //},
        _ => return Err(format!("expected a statement at {} and found {:?}", unchecked[x].span.start, unchecked[x].ty))
    };
    println!("output: {:?}", output);
    Ok(output)
}

fn parse_tokens(tokens: Vec<Token>, depth: u32) -> Result<Vec<Stmt>, String> {
    let mut output: Vec<Stmt> = Vec::new();
    let mut idents: Vec<String> = Vec::new();
    let mut unchecked: Vec<Token> = Vec::new();
    let mut idx = 0;

    while idx < tokens.len() {
        while tokens[idx].ty != TokenType::Semi {
            unchecked.push(tokens[idx].clone());
            idx += 1;
        }
        unchecked.push(tokens[idx].clone());
        idx += 1;
        let mut x = 0;
        match &unchecked[x].ty {
            TokenType::Assign(_mut) => {
                let ident: Ident;
                let semi: Token;
                x += 1;
                loop {
                    if let TokenType::Ident(y) = &unchecked[x].ty {
                        if y.0 != *"thy" && y.0 != *"the" {
                            idents.push(y.0.clone());
                            ident = y.clone();
                            x += 1;
                            break
                        }
                        else {
                            x += 1;
                        }
                    } else {
                        return Err(format!("expected an ident at {} and found {:?}", unchecked[x].span.start, unchecked[x].ty))
                    }
                }
                if unchecked[x].ty == TokenType::Ident(Ident(String::from("as"))) || unchecked[x].ty == TokenType::Ident(Ident(String::from("with"))) {
                    x += 1;
                }
                let expr: Expr = parse_expr2(&unchecked, &mut x)?;
                println!("expr: {:?}", expr);
                if unchecked[unchecked.len() - 1].ty == TokenType::Semi {
                    semi = unchecked[unchecked.len() - 1].clone();
                } else {
                    return Err(format!("expected a . at {}", unchecked[unchecked.len() - 1].span.start))
                }
                output.push(Stmt::Assignment(Assignment {ident, expr, mutable: *_mut}));
                idx += x;
                unchecked.clear();
            },
            TokenType::Function => {
                if depth > 0 {
                    return Err("erm aktually nested functions are bad practice ☝🤓".to_string())
                }
                let ident: Ident;
                let mut args: Option<Vec<(Ident, Type)>> = None;
                let mut stmts: Option<Vec<Stmt>> = None;
                x += 1;
                if let TokenType::Ident(y) = unchecked[x].ty.clone() {
                    ident = y;
                    x += 1;
                    if unchecked[x].ty == TokenType::Comma {
                        x += 1;
                    }
                    if unchecked[x].ty == TokenType::Ident(Ident(String::from("which"))) {
                        if unchecked[x + 1].ty == TokenType::Ident(Ident(String::from("bestows"))) {
                            x += 2;
                            while (unchecked[x].ty != TokenType::Comma || unchecked[x].ty != TokenType::And) && (unchecked[x + 1].ty == TokenType::Ident(Ident(String::from("takes"))) || unchecked[x + 1].ty == TokenType::Ident(Ident(String::from("requires"))) || unchecked[x + 1].ty != TokenType::Ident(Ident(String::from("thusly")))) {

                            }
                        } else if unchecked[x + 1].ty == TokenType::Ident(Ident(String::from("takes"))) || unchecked[x + 1].ty == TokenType::Ident(Ident(String::from("requires"))) {
                            x += 2;
                            while (unchecked[x].ty != TokenType::Comma || unchecked[x].ty != TokenType::And) && (unchecked[x + 1].ty == TokenType::Ident(Ident(String::from("bestows"))) || unchecked[x + 1].ty != TokenType::Ident(Ident(String::from("thusly")))) {
                            }
                            x += 1;
                            if unchecked[x].ty == TokenType::Ident(Ident(String::from("bestows"))) {

                            } else if unchecked[x].ty == TokenType::Ident(Ident(String::from("thusly"))) {
                                if unchecked[x + 1].ty == TokenType::Ident(Ident(String::from("containing"))) {
                                    x += 2;
                                } else {
                                    return Err(format!("expected containing at {} and found {:?}", unchecked[x + 1].span.start, unchecked[x + 1].ty))
                                }
                            } else {
                                return Err("how8".to_string())
                            }

                        } else {
                            return Err(format!("expected either bestows, takes or requires at {} and found {:?}", unchecked[x + 1].span.start, unchecked[x + 1].ty))
                        }
                    }
                } else {
                    return Err(format!("expected an ident at {} and found {:?}", unchecked[x].span.start, unchecked[x].ty))
                }
            },
            TokenType::Invoke => {

            },
            TokenType::Ident(x) => {

            },
            TokenType::If => {
                let mut x = 0;
                let should = unchecked[x].clone();
                x += 1;
                let expr = parse_expr2(&unchecked, &mut x)?;
            },
            TokenType::Return => {
                let mut x = 0;
                x += 1;
                let expr = parse_expr2(&unchecked, &mut x)?;
                println!("expr: {:?}, x: {:?}", expr, unchecked[x]);
                x += 1;
                if unchecked[x].ty == TokenType::Semi {
                    x += 1;
                    output.push(Stmt::Return(expr));
                    unchecked.clear();
                    idx += x;
                }
            },
            _ => return Err(format!("invalid syntax at {}: {:?}", unchecked[0].span.start, unchecked[0].ty))
        }
    }
    Ok(output)
}

fn main() -> Result<(), String> {
    println!("Hello, world!");
    let args = Cli::parse();
    let content = std::fs::read_to_string(&args.path).unwrap_or_else(|_| panic!("could not find {}", &args.path.to_str().unwrap()));
    use main4::{lexer, operator_pass};
    let tokens = lexer(content);
    let new_tokens = operator_pass(tokens.unwrap()).unwrap();
    println!("{:?}", new_tokens);
    let mut parser = Parser2::new(new_tokens);
    parser.parse()?;
    println!("parser: {}", parser);
    //let lexer = Lexer::new(content)?;
    //println!("{}", lexer.buffer);
    //let mut parser = main2::Parser::new(lexer.buffer);
    //parser.parse()?;
    //println!("{:?}", parser.statements);

    //let tokens = string_to_tokens(content).expect("should be valid tokens");
    //for n in &tokens {
    //    println!("{:?}", n.ty);
    //}
    //let mut stmts: Vec<Stmt> = Vec::new();
    //let mut idx = 0;
    //while idx < tokens.len() {
    //  stmts.push(get_stmt(&tokens, &mut idx, 0)?)
    //}
    //println!("{:#?}", stmts);
    //for n in &stmts {
    //    println!("javascript: {}", n.to_javascript());
    //    println!("rust: {}",  n);
    //}



    //let asm = tokens_to_asm(tokens).expect("should be asm");
    //println!("asm: {}", asm);
    //let stem = args.path.file_stem().expect("should be able to get file stem").to_str().expect("should be able to convert osStr to str");
    //let mut asm_file = std::fs::File::create(format!("{}.asm", stem)).expect("should be able to create file");
    //asm_file.write_all(asm.as_bytes()).expect("should be able to write to file");
    //std::process::Command::new("nasm").arg("-felf64").arg(format!("{}.asm", stem)).spawn().expect("should be able to run");
    //std::process::Command::new("ld").arg("-o").arg(stem).arg(format!("{}.o", stem)).spawn().expect("should be able to link");
    Ok(())
}
