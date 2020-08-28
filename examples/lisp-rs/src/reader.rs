// use std::io::{BufReader, BufferedReader, Reader};
use std::io::{self, BufReader, Read};

use std::fmt;

// This data structure is the output of the reading phase of the interpreter.
// An S-expression is composed of either an integer, a float, a string, a symbol,
// or a list. A list has a pointer to the head (car) of its values, and a pointer
// to the rest of the list (cdr).
#[derive(Debug, Clone, PartialEq)]
pub enum Sexp {
    Int(i64),
    Float(f64),
    Str(String),
    Symbol(String),
    Boolean(bool),
    Cons(Box<Sexp>, Box<Sexp>),
    Nil,
    NoMatch,
}

use garbage_collector::*;
impl GcObj for Sexp {}
impl Mark for Sexp {}
impl Finalize for Sexp {}

// A ReadResult is the output of the reader. It is either an S-expression upon
// success or a String error message upon failure.
pub type ReadResult = Result<Sexp, String>;

pub struct SexpReader {
    unget_stack: Vec<char>,
}

impl SexpReader {
    pub fn new() -> SexpReader {
        SexpReader {
            unget_stack: vec![],
        }
    }

    pub fn parse_all<T: Read>(&mut self, reader: &mut BufReader<T>) -> Result<Vec<Sexp>, String> {
        let mut out = vec![];
        loop {
            match self.parse(reader) {
                Ok(Sexp::NoMatch) => break,
                Err(v) => return Err(v),
                Ok(v) => out.push(v),
            }
        }
        Ok(out)
    }

    // Top-level parse of an S-expression. The empty string is parsed as
    // a Nil.
    pub fn parse<T: Read>(&mut self, reader: &mut BufReader<T>) -> ReadResult {
        match self.get_char(reader, true) {
            Some(c) => match c {
                '\'' => self.parse_quoted_sexp(reader),
                ',' => self.parse_unquoted_sexp(reader),
                '`' => self.parse_quasiquoted_sexp(reader),
                '(' => self.parse_list(reader),
                _ => {
                    self.unget_char(c);
                    self.parse_atom(reader)
                }
            },
            None => Ok(Sexp::NoMatch),
        }
    }

    // Wrapper around parse() that allows for the parsing of strings.
    // not actually dead code - used in the tests.
    #[allow(dead_code)]
    pub fn parse_str(&mut self, string: &str) -> ReadResult {
        let reader = BufReader::new(string.as_bytes());
        let mut buf_reader = BufReader::new(reader);
        self.parse(&mut buf_reader)
    }

    pub fn parse_str_all(&mut self, string: &str) -> Result<Vec<Sexp>, String> {
        let reader = BufReader::new(string.as_bytes());
        let mut buf_reader = BufReader::new(reader);
        self.parse_all(&mut buf_reader)
    }

    fn parse_quoted_sexp<T: Read>(&mut self, reader: &mut BufReader<T>) -> ReadResult {
        let cdr = self.parse(reader)?;
        let new_sexp = Sexp::Cons(Box::new(Sexp::Symbol("quote".to_string())), Box::new(cdr));
        Ok(new_sexp)
    }

    fn parse_unquoted_sexp<T: Read>(&mut self, reader: &mut BufReader<T>) -> ReadResult {
        match self.peek_char(reader, true) {
            Some('@') => {
                let _ = self.get_char(reader, true);
                let cdr = self.parse(reader)?;
                let new_sexp = Sexp::Cons(
                    Box::new(Sexp::Symbol("unquote-splicing".to_string())),
                    Box::new(cdr),
                );
                Ok(new_sexp)
            }
            _ => {
                let cdr = self.parse(reader)?;
                let new_sexp =
                    Sexp::Cons(Box::new(Sexp::Symbol("unquote".to_string())), Box::new(cdr));
                Ok(new_sexp)
            }
        }
    }

    fn parse_quasiquoted_sexp<T: Read>(&mut self, reader: &mut BufReader<T>) -> ReadResult {
        let cdr = self.parse(reader)?;
        let new_sexp = Sexp::Cons(
            Box::new(Sexp::Symbol("quasiquote".to_string())),
            Box::new(cdr),
        );
        Ok(new_sexp)
    }

    fn parse_list<T: Read>(&mut self, reader: &mut BufReader<T>) -> ReadResult {
        let car = match self.parse(reader) {
            Ok(value) => value,
            Err(e) => match self.get_char(reader, true) {
                Some(')') => return Ok(Sexp::Nil),
                _ => return Err(e),
            },
        };
        let cdr = match self.get_char(reader, true) {
            Some(e) if e == '.' => {
                // look ahead one token to see if we are looking at an ellipsis (...)
                match self.peek_char(reader, true) {
                    Some(v) if v != '.' => {
                        let result = self.parse(reader)?;
                        match self.get_char(reader, true) {
                            Some(')') => result,
                            _ => return Err("Expected )".to_string()),
                        }
                    }
                    // if we are, treat it like a symbol
                    _ => {
                        self.unget_char(e);
                        self.parse_list(reader)?
                    }
                }
            }
            Some(e) => {
                self.unget_char(e);
                self.parse_list(reader)?
            }
            None => return Err("Unexpected EOF, expected . or atom".to_string()),
        };

        Ok(Sexp::Cons(Box::new(car), Box::new(cdr)))
    }

    fn parse_atom<T: Read>(&mut self, reader: &mut BufReader<T>) -> ReadResult {
        match self.peek_char(reader, true) {
            Some(c) => match c {
                '\"' => self.parse_string_literal(reader),
                c if c.is_digit(10) => self.parse_number(reader),
                '#' => self.parse_boolean(reader),
                _ => self.parse_symbol(reader),
            },
            None => Err("Unexpected EOF while scanning atom".to_string()),
        }
    }

    fn parse_boolean<T: Read>(&mut self, reader: &mut BufReader<T>) -> ReadResult {
        let _ = self.get_char(reader, true);
        match self.get_char(reader, false) {
            Some(e) => match e {
                't' => Ok(Sexp::Boolean(true)),
                'f' => Ok(Sexp::Boolean(false)),
                _ => Err(format!("Unknown boolean literal, got {}", e)),
            },
            None => Err("Unexpected EOF while scanning boolean literal".to_string()),
        }
    }

    fn parse_string_literal<T: Read>(&mut self, reader: &mut BufReader<T>) -> ReadResult {
        let _ = self.get_char(reader, false);
        let mut string = "".to_string();
        loop {
            match self.get_char(reader, false) {
                Some(e) => match e {
                    '\"' => break,
                    '\\' => match self.parse_escape_char(reader) {
                        Some(v) => string.push(v),
                        None => return Err(format!("Unexpected escape sequence, got {}", e)),
                    },
                    '\n' => {
                        return Err(
                            "Unescaped newlines are not allowed in string literals".to_string()
                        )
                    }
                    _ => string.push(e),
                },
                None => return Err("Unexpected EOF while scanning string literal".to_string()),
            }
        }
        Ok(Sexp::Str(string))
    }

    fn parse_escape_char<T: Read>(&mut self, reader: &mut BufReader<T>) -> Option<char> {
        match self.get_char(reader, false) {
            Some(e) => match e {
                '\"' => Some('\"'),
                '\'' => Some('\''),
                '\\' => Some('\\'),
                'n' => Some('\n'),
                'r' => Some('\r'),
                't' => Some('\t'),
                _ => None,
            },
            None => None,
        }
    }

    fn parse_number<T: Read>(&mut self, reader: &mut BufReader<T>) -> ReadResult {
        let mut is_double = false;
        let mut string = "".to_string();
        loop {
            match self.get_char(reader, false) {
                Some(e) if e == '.' && is_double => {
                    return Err("More than one . in numeric literal".to_string())
                }
                Some(e) if e == '.' => {
                    is_double = true;
                    string.push(e);
                }
                Some(e) if e.is_digit(10) => string.push(e),
                Some(e) => {
                    self.unget_char(e);
                    break;
                }
                None => break,
            }
        }
        if is_double {
            Ok(Sexp::Float(string.parse::<f64>().unwrap()))
        } else {
            Ok(Sexp::Int(string.parse::<i64>().unwrap()))
        }
    }

    fn parse_symbol<T: Read>(&mut self, reader: &mut BufReader<T>) -> ReadResult {
        let mut symbol = match self.peek_char(reader, false) {
            Some(e) if self.is_valid_for_identifier(e) => {
                let ch = self.get_char(reader, false).unwrap();
                let mut s = String::new();
                s.push(ch);
                // Workaround to replace convertion from char to string
                // String::from_char(1, self.get_char(reader, false).unwrap())
                s
            }
            Some(e) => return Err(format!("Unexpected character: got {}, expected an atom", e)),
            None => return Err("Unexpected EOF".to_string()),
        };
        loop {
            match self.get_char(reader, false) {
                Some(v) if self.is_valid_for_identifier(v) => symbol.push(v),
                Some(v) => {
                    self.unget_char(v);
                    break;
                }
                None => break,
            }
        }
        Ok(Sexp::Symbol(symbol))
    }

    fn is_valid_for_identifier(&self, c: char) -> bool {
        match c {
            '!'
            | '$'
            | '%'
            | '&'
            | '*'
            | '+'
            | '-'
            | '.'
            | '~'
            | '/'
            | ':'
            | '<'
            | '='
            | '>'
            | '?'
            | '@'
            | '^'
            | '_'
            | 'a'...'z'
            | 'A'...'Z'
            | '0'...'9' => true,
            _ => false,
        }
    }

    fn get_char<T: Read>(
        &mut self,
        reader: &mut BufReader<T>,
        skip_whitespace: bool,
    ) -> Option<char> {
        loop {
            match self.unget_stack.pop() {
                Some(e) if !e.is_whitespace() || !skip_whitespace => return Some(e),
                Some(_) => continue,
                None => (),
            };

            // Workaround to replace `reader.read_char()`
            let mut one_char_buffer = [0];
            let n_bytes_read = reader.read(&mut one_char_buffer);
            match n_bytes_read {
                Ok(n) if n > 0 => {}
                Ok(n) => return None,
                Err(e) => return None,
            };
            let ch = one_char_buffer[0] as char;

            match ch {
                c if !c.is_whitespace() || !skip_whitespace => return Some(c),
                _ => (),
            };
        }
    }

    fn unget_char(&mut self, c: char) {
        self.unget_stack.push(c);
    }

    fn peek_char<T: Read>(
        &mut self,
        reader: &mut BufReader<T>,
        skip_whitespace: bool,
    ) -> Option<char> {
        match self.get_char(reader, skip_whitespace) {
            Some(c) => {
                self.unget_char(c);
                Some(c)
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_ints() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("42");
        assert!(result.is_ok());
        let sexp = result.unwrap();
        match sexp {
            Sexp::Int(x) => assert_eq!(x, 42),
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        };
    }

    #[test]
    fn parses_floats() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("9.8");
        assert!(result.is_ok());
        let sexp = result.unwrap();
        match sexp {
            Sexp::Float(x) => assert_eq!(x, 9.8),
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        };
    }

    #[test]
    fn parses_strings() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("\"hello world\"");
        assert!(result.is_ok());
        let sexp = result.unwrap();
        match sexp {
            Sexp::Str(x) => assert_eq!(x, "hello world".to_string()),
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        };
    }

    #[test]
    fn parses_symbols() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("hello");
        assert!(result.is_ok());
        let sexp = result.unwrap();
        match sexp {
            Sexp::Symbol(x) => assert_eq!(x, "hello".to_string()),
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        };
    }

    #[test]
    fn parses_lists() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("(1 2 3)");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Cons(
                box Sexp::Int(x),
                box Sexp::Cons(box Sexp::Int(y), box Sexp::Cons(box Sexp::Int(z), box Sexp::Nil)),
            ) => {
                assert_eq!(x, 1);
                assert_eq!(y, 2);
                assert_eq!(z, 3);
            }
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        };
    }

    #[test]
    fn parses_quotes() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("'42");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Cons(box Sexp::Symbol(s), box Sexp::Int(i)) => {
                assert_eq!(s, "quote".to_string());
                assert_eq!(i, 42);
            }
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        };
    }

    #[test]
    fn parses_quoted_lists() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("'(1 2 3)");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Cons(
                box Sexp::Symbol(s),
                box Sexp::Cons(
                    box Sexp::Int(a),
                    box Sexp::Cons(
                        box Sexp::Int(b),
                        box Sexp::Cons(box Sexp::Int(c), box Sexp::Nil),
                    ),
                ),
            ) => {
                assert_eq!(s, "quote".to_string());
                assert_eq!(a, 1);
                assert_eq!(b, 2);
                assert_eq!(c, 3);
            }
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        };
    }

    #[test]
    fn parses_improper_lists() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("(1 . 2)");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Cons(box Sexp::Int(a), box Sexp::Int(b)) => {
                assert_eq!(a, 1);
                assert_eq!(b, 2);
            }
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        }
    }

    #[test]
    fn parses_ellipsis_as_symbol() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("...");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Symbol(s) => assert_eq!(s, "...".to_string()),
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        }
    }

    #[test]
    fn parses_boolean_true() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("#t");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Boolean(v) => assert!(v),
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        }
    }

    #[test]
    fn parses_boolean_false() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("#f");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Boolean(v) => assert!(!v),
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        }
    }

    #[test]
    fn parses_empty_list() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("()");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Nil => (),
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        }
    }

    #[test]
    fn parses_quoted_empty_list() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("'()");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Cons(box Sexp::Symbol(ref quote), box Sexp::Nil) => {
                assert_eq!(*quote, "quote".to_string())
            }
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        }
    }

    #[test]
    fn parses_quasiquote() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str("`5");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Cons(box Sexp::Symbol(ref quote), box Sexp::Int(5)) => {
                assert_eq!(*quote, "quasiquote".to_string())
            }
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        }
    }

    #[test]
    fn parses_unquote() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str(",5");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Cons(box Sexp::Symbol(ref quote), box Sexp::Int(5)) => {
                assert_eq!(*quote, "unquote".to_string())
            }
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        }
    }

    #[test]
    fn parses_unquote_splicing() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str(",@5");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let sexp = result.unwrap();
        match sexp {
            Sexp::Cons(box Sexp::Symbol(ref quote), box Sexp::Int(5)) => {
                assert_eq!(*quote, "unquote-splicing".to_string())
            }
            _ => panic!("Parsed incorrectly, got {:?}", sexp),
        }
    }

    #[test]
    fn parses_several_expressions() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str_all("(hello) (world)");
        let sexp = result.unwrap();
        assert_eq!(sexp.len(), 2);
        match sexp[0] {
            Sexp::Cons(box Sexp::Symbol(ref s), box Sexp::Nil) => {
                assert_eq!(*s, "hello".to_string())
            }
            ref s => panic!("Parsed incorrectly, got {:?}", s),
        };
        match sexp[1] {
            Sexp::Cons(box Sexp::Symbol(ref s), box Sexp::Nil) => {
                assert_eq!(*s, "world".to_string())
            }
            ref s => panic!("Parsed incorrectly, got {:?}", s),
        }
    }

    #[test]
    fn parses_several_empty_lists() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str_all("()()");
        let sexp = result.unwrap();
        assert_eq!(sexp.len(), 2);
        match sexp[0] {
            Sexp::Nil => (),
            ref s => panic!("Parsed incorrectly, got {:?}", s),
        };
        match sexp[1] {
            Sexp::Nil => (),
            ref s => panic!("Parsed incorrectly, got {:?}", s),
        }
    }

    #[test]
    fn parses_several_improper_lists() {
        let mut reader = SexpReader::new();
        let result = reader.parse_str_all("(1 . 2) (3 . 4)");
        let sexp = result.unwrap();
        assert_eq!(sexp.len(), 2);
        match sexp[0] {
            Sexp::Cons(box Sexp::Int(a), box Sexp::Int(b)) => {
                assert_eq!(a, 1);
                assert_eq!(b, 2);
            }
            ref s => panic!("Parsed incorrectly, got {:?}", s),
        };
        match sexp[1] {
            Sexp::Cons(box Sexp::Int(a), box Sexp::Int(b)) => {
                assert_eq!(a, 3);
                assert_eq!(b, 4);
            }
            ref s => panic!("Parsed incorrectly, got {:?}", s),
        };
    }
}
