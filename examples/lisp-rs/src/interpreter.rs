
use super::reader;
use garbage_collector::*;
use std::collections::HashSet;
use std::fmt::{self, Error, Formatter};
use std::ops::Deref;
// The LispValue enum is the type of all Lisp values at runtime. These are
// the same as the S-expression representation, except that functions can also
// be values. LispValues are always used as a reference counted pointer.
#[derive(Debug, PartialEq)]
pub enum LispValue {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Symbol(String),
    Func(Function),
    Cons(Handle<LispValue>, Handle<LispValue>),
    Nil,
}

impl<'a> GcObj for LispValue {}

impl<'a> Finalize for LispValue {}
impl<'a> Mark for LispValue {
    fn mark(&self, visit: &mut dyn FnMut(*const GcBox<()>)) {
        match self {
            Self::Cons(x, y) => {
                x.mark(visit);
                y.mark(visit);
            }
            _ => (),
        }
    }
}
impl fmt::Display for LispValue {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "{}", self.pretty_print())
    }
}
// Impl of PartialEq for the Function type indicating that functions can never
// be equal to one another.
impl PartialEq for Function {
    fn eq(&self, _: &Function) -> bool {
        false
    }

    fn ne(&self, _: &Function) -> bool {
        true
    }
}
impl LispValue {
    // Simple algorithm for pretty-printing an S-expression.
    // The algorithm goes like this:
    // 1) If self isn't a list, print it and return.
    // 2) If self is a list, print an open paren.
    //   2a) Print the car of the list.
    //   2b) If cdr is a list, recurse to step 2a with cdr as the new list
    //   2c) If cdr is nil, print nothing,
    //   2d) If cdr is anything else, print a "." followed by a space
    //       and recursively print the cdr.
    // This function returns a string so "printing" isn't done, but it's basically
    // the same thing.
    pub fn pretty_print(&self) -> String {
        match *self {
            LispValue::Int(v) => v.to_string(),
            LispValue::Float(v) => v.to_string(),
            LispValue::Str(ref v) => format!("\"{}\"", v),
            LispValue::Symbol(ref v) => format!("{}", v),
            LispValue::Cons(ref car, ref cdr) => {
                let (s_car, s_cdr) = self.print_cons(&**car, &**cdr);
                format!("({} {})", s_car, s_cdr)
            }
            LispValue::Nil => "()".to_string(),
            LispValue::Bool(v) => {
                if v {
                    "#t".to_string()
                } else {
                    "#f".to_string()
                }
            } //LispValue::Func(ref c) => format!("{}", c),
        }
    }

    fn print_cons(&self, car: &LispValue, cdr: &LispValue) -> (String, String) {
        let car_str = car.pretty_print();
        let cdr_str = match *cdr {
            LispValue::Cons(ref c_car, ref c_cdr) => {
                let (s_car, s_cdr) = self.print_cons(&**c_car, &**c_cdr);
                if s_cdr.len() == 0 {
                    format!("{}", s_car)
                } else {
                    format!("{} {}", s_car, s_cdr)
                }
            }
            LispValue::Nil => "".to_string(),
            _ => format!(". {}", cdr.pretty_print()),
        };
        (car_str, cdr_str)
    }
}
#[derive(Debug)]
pub enum FunctionParameters {
    Fixed(Vec<String>),
    Variadic(Vec<String>, String),
}

impl FunctionParameters {
    pub fn get_variables(&self) -> Vec<String> {
        match *self {
            FunctionParameters::Fixed(ref vec) => vec.clone(),
            FunctionParameters::Variadic(ref vec, ref rest) => {
                let mut temp = vec.clone();
                temp.push(rest.clone());
                temp
            }
        }
    }
}

type Closure = Vec<(String, Option<Handle<LispValue>>)>;
// Functions can be one of three things - an internal function (defined by defun or
// lambda), an external Rust function exposed to the interpreter, or a macro.
// Macros aren't supported yet.
#[allow(dead_code)] // macros aren't used yet
#[derive(Debug)]
pub enum Function {
    InternalFunction(FunctionParameters, Handle<reader::Sexp>, Closure),
    ExternalFunction(String, fn(Vec<Handle<LispValue>>) -> EvalResult),
    Macro(FunctionParameters, Handle<reader::Sexp>),
}
pub type EvalResult = Result<Handle<LispValue>, String>;

pub type HValue = Handle<LispValue>;
use super::environment;
pub struct Interpreter {
    environment: environment::Environment,
}

use reader::Sexp;

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut interpreter = Interpreter {
            environment: environment::Environment::new(),
        };
        //interpreter.load_intrinsics();
        interpreter
    }

    pub fn eval(&mut self, heap: &mut Heap, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Int(i) => Ok(heap.allocate(LispValue::Int(i)).to_heap()),
            Sexp::Float(f) => Ok(heap.allocate(LispValue::Float(f)).to_heap()),
            Sexp::Str(ref s) => Ok(heap.allocate(LispValue::Str(s.clone())).to_heap()),
            //Sexp::Symbol(ref s) => self.eval_symbol(s.clone()),
            Sexp::Boolean(b) => Ok(heap.allocate(LispValue::Bool(b)).to_heap()),
            //Sexp::Cons(box Sexp::Symbol(ref s), ref right) if self.is_intrinsic(s) => {
            //    self.eval_intrinsic(s, &**right)
            //}
            //Sexp::Cons(ref left, ref right) => self.eval_function(&**left, &**right),
            Sexp::Nil => Err("Unknown form ()".to_string()),
            _ => unreachable!(),
        }
    }
    fn is_intrinsic(&self, name: &String) -> bool {
        match name.as_str() {
            "if" | "defun" | "defmacro" | "lambda" | "define" | "quote" | "unquote" | "and"
            | "or" | "progn" | "quasiquote" => true,
            _ => false,
        }
    }
    fn eval_progn<'a>(&mut self, heap: &mut Heap<'a>, sexp: &reader::Sexp) -> EvalResult {
        let mut last: Option<HValue> = None;
        let mut cursor = sexp;
        loop {
            match *cursor {
                Sexp::Nil => return Ok(last.unwrap_or(heap.allocate(LispValue::Nil).to_heap())),
                Sexp::Cons(ref car, ref cdr) => {
                    let result = self.eval(heap, &**car)?;
                    last = Some(result.clone());
                    cursor = &**cdr;
                }
                _ => return Err("Improper lists disallowed in progn form".to_string()),
            }
        }
    }
    fn eval_quote<'a, 'b: 'a>(
        &mut self,
        heap: &'b mut Heap<'a>,
        sexp: &reader::Sexp,
    ) -> EvalResult {
        fn sexp_to_lvalue<'a, 'b: 'a>(heap: &'a mut Heap<'b>, s: &reader::Sexp) -> HValue {
            match *s {
                Sexp::Int(i) => heap.allocate(LispValue::Int(i)).to_heap(),
                Sexp::Float(i) => heap.allocate(LispValue::Float(i)).to_heap(),
                Sexp::Str(ref s) => heap.allocate(LispValue::Str(s.clone())).to_heap(),
                Sexp::Symbol(ref s) => heap.allocate(LispValue::Symbol(s.clone())).to_heap(),
                Sexp::Boolean(b) => heap.allocate(LispValue::Bool(b)).to_heap(),
                Sexp::Cons(ref car, ref cdr) => {
                    let car = sexp_to_lvalue(heap, &**car);
                    let cdr = sexp_to_lvalue(heap, &**cdr);
                    let data = heap.allocate(LispValue::Cons(car, cdr));

                    data.to_heap()
                }
                Sexp::Nil => allocate(LispValue::Nil).to_heap(),
                _ => unreachable!(),
            }
        }
        Ok(sexp_to_lvalue(heap, sexp))
    }
    fn eval_and<'a>(&mut self, heap: &'a mut Heap<'a>, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(ref car, ref cdr) => match self.eval(heap, &**car) {
                Ok(val) => match val.deref() {
                    &LispValue::Bool(false) => Ok(val.clone()),
                    _ => self.eval_and(heap, &**cdr),
                },
                Err(e) => Err(e),
            },
            Sexp::Nil => Ok(heap.allocate(LispValue::Bool(true)).to_heap()),
            ref e => match self.eval(heap, e) {
                Ok(val) => match val.deref() {
                    &LispValue::Bool(false) => Ok(val.clone()),
                    _ => Ok(heap.allocate(LispValue::Bool(true)).to_heap()),
                },
                Err(e) => Err(e),
            },
        }
    }

    fn eval_or<'a>(&mut self, heap: &'a mut Heap<'a>, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(ref car, ref cdr) => match self.eval(heap, &**car) {
                Ok(val) => match val.deref() {
                    &LispValue::Bool(true) => Ok(val.clone()),
                    _ => self.eval_or(heap, &**cdr),
                },
                Err(e) => Err(e),
            },
            Sexp::Nil => Ok(heap.allocate(LispValue::Bool(false)).to_heap()),
            ref e => match self.eval(heap, e) {
                Ok(val) => match val.deref() {
                    &LispValue::Bool(false) => Ok(val.clone()),
                    &LispValue::Nil => Ok(heap.allocate(LispValue::Bool(false)).to_heap()),
                    _ => Ok(heap.allocate(LispValue::Bool(true)).to_heap()),
                },
                Err(e) => Err(e),
            },
        }
    }

    fn eval_if<'a>(&mut self, heap: &'a mut Heap<'a>,sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            /*Sexp::Cons(ref condition,
                         Box::new( Sexp::Cons(ref true_branch,
                                          Box::new( Sexp::Cons(ref false_branch,
                                                           Box::new( Sexp::Nil))))) => {
                let cond = self.eval(heap,&**condition)?;
                if let &LispValue::Bool(false) = cond.deref() {
                    self.eval(heap,&**false_branch)
                } else {
                    self.eval(heap,&**true_branch)
                }
            }
            Sexp::Cons(ref condition,
                         box Sexp::Cons(ref true_branch,
                                          box Sexp::Nil)) => {
                let cond = try!(self.eval(&**condition));
                if let &LispValue::Bool(false) = cond.deref() {
                    Ok(heap.allocate(LispValue::Nil))
                } else {
                    self.eval(&**true_branch)
                }
            }*/
            _ => Err("Invalid pattern for if form".to_string())
        }
    }

    fn eval_define<'a>(&mut self,heap: &'a mut Heap<'a>, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            /*Sexp::Cons(box Sexp::Symbol(ref sym), box Sexp::Cons(ref exp, box Sexp::Nil)) => {
                let value = try!(self.eval(&**exp));
                self.environment.put_global(sym.clone(), value.clone());
                Ok(value)
            }
            Sexp::Cons(box Sexp::Symbol(ref sym), ref exp) => {
                let value = try!(self.eval(&**exp));
                self.environment.put_global(sym.clone(), value.clone());
                Ok(value)
            }
            Sexp::Cons(ref other, _) => Err(format!("Not a symbol: {:?}", other)),*/
            Sexp::Cons(x,y) => {
                match (*x,*y) {
                    (Sexp::Symbol(ref sym),Sexp::Cons(ref exp, xx)) if *xx == Sexp::Nil => {
                        let value = self.eval(heap,&**exp)?;
                        self.environment.put_global(sym.clone(), value);
                        Ok(value)
                    }
                    (Sexp::Symbol(ref sym),ref exp) => {
                        let value = self.eval(heap,&*exp)?;
                        self.environment.put_global(sym.clone(),value);
                        Ok(value)
                    },
                    (other,_)=> Err(format!("Not a symbol: {:?}", other)),
                }
            }
            _ => Err(format!("No arguments to define form"))
        }
    }

    // algorithm for evaluating a quasiquoted S-expression:
    // 1) incremenent the quasiquote counter
    // 2) if the atom isn't a list, evaluate it as a quoted atom and decrement the quasiquote counter.
    // 3) if the atom is a list...
    //  a) If the car of the list is unquoted, decrement the quasiquote counter. If the quasiquote
    //     counter is zero, evaluate the car of the list. If the counter is less than zero, error.
    //     If the counter is greater than zero, evaluate the car of the list as a quoted atom.
    //  b) If the car of the list is not quoted, evaluate the car of the list as a quoted atom.
    //  c) Evaluate the cdr of the list (goto step 2)
    fn eval_quasiquote<'a>(&mut self,heap: &'a mut Heap<'a>, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
             Sexp::Cons(box Sexp::Cons(box Sexp::Symbol(ref s), ref quoted), ref cdr) if *s == "unquote".to_string() => {
                let result = self.eval(&**quoted)?;
                let rest = try!(self.eval_quasiquote(&**cdr));
                Ok(heap.allocate(LispValue::Cons(result, rest)))
            },
            Sexp::Cons(box Sexp::Symbol(ref s), ref quoted) if *s == "unquote".to_string() => {
                let result = try!(self.eval(&**quoted));
                Ok(result)
            },
            Sexp::Cons(box Sexp::Cons(box Sexp::Symbol(ref s), ref quoted), box Sexp::Nil) if *s == "unquote-splicing".to_string() => {
                let result = try!(self.eval(&**quoted));
                Ok(result)
            }
            Sexp::Cons(box Sexp::Cons(box Sexp::Symbol(ref s), _), _) if *s == "unquote-splicing".to_string() => {
                Err("Invalid unquote-splicing form".to_string())
            }
            Sexp::Cons(box Sexp::Symbol(ref s), _) if *s == "unquote-splicing".to_string() => {
                Err("Invalid unquote-splicing form".to_string())
            },
            Sexp::Cons(ref car, ref cdr) => {
                let result = try!(self.eval_quote(&**car));
                let rest = try!(self.eval_quasiquote(&**cdr));
                Ok(heap.allocate(LispValue::Cons(result, rest)))
            }
            ref val => self.eval_quote(heap,val)
        }
    }

    fn eval_defun(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(box Sexp::Symbol(ref sym),
                         box Sexp::Cons(ref parameters,
                                          box Sexp::Cons(ref body,
                                                           box Sexp::Nil))) => {
                let params = self.eval_list_as_parameter_list(&**parameters);
                let free_vars = self.get_free_variables(params.get_variables(), &**body);
                let closure = free_vars.into_iter().map(|x| (x.clone(), self.environment.get(x.clone()).clone())).collect();
                let func = heap.allocate(LispValue::Func(Function::InternalFunction(params, heap.allocate(*body.clone()), closure)));
                self.environment.put_global(sym.clone(), func.clone());
                Ok(func)
            }
            Sexp::Cons(ref other, _) => Err(format!("Not a symbol: {:?}", other)),
            _ => Err("No arguments to defun form".to_string())
        }
    }

    fn eval_lambda(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(ref parameters,
                         box Sexp::Cons(ref body,
                                          box Sexp::Nil)) => {
                let params = self.eval_list_as_parameter_list(&**parameters);
                let free_vars = self.get_free_variables(params.get_variables(), &**body);
                let closure = free_vars.into_iter().map(|x| (x.clone(), self.environment.get(x.clone()).clone())).collect();
                let func = LispValue::Func(Function::InternalFunction(params, heap.allocate(*body.clone()), closure));
                Ok(heap.allocate(func))
            }
            _ => Err("Incorrect pattern for lambda form".to_string())
        }
    }

    fn eval_symbol(&mut self, sym: String) -> EvalResult {
        match self.environment.get(sym.clone()) {
            Some(value) => Ok(value),
            None => Err(format!("Unbound symbol: {}", sym))
        }
    }

    fn eval_function(&mut self, car: &reader::Sexp, cdr: &reader::Sexp) -> EvalResult {
        let sym_val = try!(self.eval(car));
        match *sym_val {
            LispValue::Func(ref f) => match *f {
                Function::InternalFunction(ref parameters, ref body, ref closure) => self.eval_internal_function(cdr, parameters, body.clone(), closure),
                Function::ExternalFunction(_, func) => self.eval_external_function(cdr, func),
                Function::Macro(ref parameters, ref body) => self.eval_macro(cdr, parameters, body.clone())
            },
            _ => Err(format!("Value is not callable: {}", sym_val))
        }
    }

    fn eval_internal_function(&mut self,
                              actual_params: &reader::Sexp,
                              formal_params: &FunctionParameters,
                              body: Rc<reader::Sexp>,
                              closure: &Closure) -> EvalResult {
        let params = try!(self.sexp_map(actual_params, |s, t| s.eval(t)));
        let actual : &Vec<String> = match *formal_params {
            FunctionParameters::Fixed(ref vec) => {
                if params.len() != vec.len() {
                    return Err("Incorrect number of parameters".to_string());
                }
                vec
            },
            FunctionParameters::Variadic(ref vec, _) => {
                if params.len() < vec.len() {
                    return Err("Incorrect number of parameters".to_string());
                }
                vec
            }
        };
        self.environment.enter_scope();
        for (value, binding) in params.iter().zip(actual.iter()) {
            self.environment.put(binding.clone(), value.clone());
        }
        if let FunctionParameters::Variadic(_, ref rest) = *formal_params {
            let rest_as_vector = params.iter().skip(actual.len());
            let list = self.iter_to_list(rest_as_vector);
            self.environment.put(rest.clone(), list);
        }
        for &(ref binding, ref value) in closure.iter() {
            match *value {
                Some(ref v) => self.environment.put(binding.clone(), v.clone()),
                None => match self.environment.get(binding.clone()) {
                    Some(ref v) => self.environment.put(binding.clone(), v.clone()),
                    None => return Err(format!("unbound variable: {}", binding.clone()))
                }
            }
        }
        let result = self.eval(body.deref());
        self.environment.exit_scope();
        result
    }

    fn iter_to_list<'a,
                    T: Iterator<Item = &'a Rc<LispValue>>
                    >(&mut self, mut iter: T) -> Rc<LispValue>
    {
        match iter.next() {
            Some(v) => heap.allocate(LispValue::Cons(v.clone(), self.iter_to_list(iter))),
            None => heap.allocate(LispValue::Nil)
        }
    }

    fn eval_external_function(&mut self,
                              actual_params: &reader::Sexp,
                              func: fn(Vec<Rc<LispValue>>) -> EvalResult) -> EvalResult {
        match self.sexp_map(actual_params, |s, t| s.eval(t)) {
            Ok(v) => func(v),
            Err(e) => Err(e)
        }
    }

    fn eval_macro(&mut self,
                  actual_params: &reader::Sexp,
                  formal_params: &FunctionParameters,
                  body: Rc<reader::Sexp>) -> EvalResult {
        let params = try!(self.sexp_map(actual_params, |s, t| s.eval_quote(t)));
        let actual : &Vec<String> = match *formal_params {
            FunctionParameters::Fixed(ref vec) => {
                if params.len() != vec.len() {
                    return Err("Incorrect number of parameters".to_string());
                }
                vec
            },
            FunctionParameters::Variadic(ref vec, _) => {
                if params.len() < vec.len() {
                    return Err("Incorrect number of parameters".to_string());
                }
                vec
            }
        };
        self.environment.enter_scope();
        for (value, binding) in params.iter().zip(actual.iter()) {
            self.environment.put(binding.clone(), value.clone());
        }
        if let FunctionParameters::Variadic(_, ref rest) = *formal_params {
            let rest_as_vector = params.iter().skip(actual.len());
            let list = self.iter_to_list(rest_as_vector);
            self.environment.put(rest.clone(), list);
        }
        let result = self.eval(body.deref());
        self.environment.exit_scope();
        if let Ok(value) = result {
            let sexp = self.value_to_sexp(value);
            self.eval(&sexp)
        } else {
            result
        }
    }

    fn eval_defmacro(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(box Sexp::Symbol(ref sym),
                         box Sexp::Cons(ref parameters,
                                          box Sexp::Cons(ref body,
                                                           box Sexp::Nil))) => {
                let params = self.eval_list_as_parameter_list(&**parameters);
                let macro_thing = heap.allocate(LispValue::Func(Function::Macro(params, heap.allocate(*body.clone()))));
                self.environment.put_global(sym.clone(), macro_thing.clone());
                Ok(macro_thing)
            }
            _ => Err("Not a legal defmacro pattern".to_string())
        }
    }

    fn sexp_map<F>(&mut self, params: &reader::Sexp,
                func: F
    ) -> Result<Vec<Rc<LispValue>>, String>
        where F: for<'a> Fn(&'a mut Interpreter, &'a Sexp) -> EvalResult
    {
        match *params {
            Sexp::Cons(ref car, ref cdr) => {
                let mut out_vec = vec![];
                match func(self, &**car) {
                    Ok(v) => out_vec.push(v),
                    Err(e) => return Err(e)
                };
                match self.sexp_map(&**cdr, func) {
                    Ok(vec) => {
                        out_vec.extend(vec.into_iter());
                        Ok(out_vec)
                    }
                    Err(e) => Err(e)
                }
            }
            Sexp::Nil => Ok(vec![]),
            _ => Err("Cannot use an improper list as parameters to a function".to_string())
        }
    }

    fn value_to_sexp(&self, value: Rc<LispValue>) -> reader::Sexp {
        match *value {
            LispValue::Int(i) => Sexp::Int(i),
            LispValue::Float(i) => Sexp::Float(i),
            LispValue::Str(ref s) => Sexp::Str(s.clone()),
            LispValue::Bool(b) => Sexp::Boolean(b),
            LispValue::Symbol(ref s) => Sexp::Symbol(s.clone()),
            LispValue::Cons(ref car, ref cdr) => Sexp::Cons(box self.value_to_sexp(car.clone()),
                                                   box self.value_to_sexp(cdr.clone())),
            LispValue::Nil => Sexp::Nil,
            _ => unreachable!()
        }
    }
    
    // The function is similar to eval_list_as_parameters, but it just gets the names
    // of all of the symbols in the linked list instead of evaluating them. This
    // is also used for evaluating parameters for a function call.
    fn eval_list_as_parameter_list(&self, params: &reader::Sexp) -> FunctionParameters {
        let mut cursor = params;
        let mut out = vec![];
        let mut out_rest = None;
        loop {
            match *cursor {
                Sexp::Cons(box Sexp::Symbol(ref s), box Sexp::Symbol(ref rest)) => {
                    out.push(s.clone());
                    out_rest = Some(rest.clone());
                    break;
                },
                Sexp::Cons(box Sexp::Symbol(ref s), ref cdr) => {
                    out.push(s.clone());
                    cursor = &**cdr;
                },
                Sexp::Nil => break,
                _ => unreachable!()
            };
        }
        if out_rest.is_some() {
            FunctionParameters::Variadic(out, out_rest.unwrap())
        } else {
            FunctionParameters::Fixed(out)
        }
    }

    fn get_free_variables(&self, variables: Vec<String>, body: &Sexp) -> Vec<String> {
        let parameter_set : HashSet<String> = variables.iter().map(|&ref x| x.clone()).collect();
        let variable_set : HashSet<String> = self.get_variables(body);
        variable_set.difference(&parameter_set).map(|&ref x| x.clone()).collect()
    }

    fn get_variables(&self, body: &Sexp) -> HashSet<String> {
        match *body {
            Sexp::Symbol(ref s) if !self.is_intrinsic(s) => {
                let mut set = HashSet::new();
                set.insert(s.clone());
                set
            },
            Sexp::Cons(ref car, ref cdr) => {
                let free_car = self.get_variables(&**car);
                let free_cdr = self.get_variables(&**cdr);
                free_car.union(&free_cdr).map(|&ref x| x.clone()).collect()
            },
            _ => HashSet::new()
        }
    }
}
