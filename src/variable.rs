use crate::parser;
use std::collections::{HashMap, hash_map};
use std::sync::Arc;

/// A variable value.
#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Array(Vec<String>),
    Function(Box<parser::Command>),
}

pub fn value_as_str(value: &Option<Value>) -> &str {
    match value {
        Some(Value::String(value)) => value,
        Some(Value::Function(_)) => "(function)",
        // Bash returns the first element in the array.
        Some(Value::Array(elems)) => match elems.get(0) {
            Some(elem) => elem.as_str(),
            _ => "",
        },
        None => "",
    }
}

/// A shell variable.
#[derive(Debug)]
pub struct Variable {
    // The inner value. `None` represents *null*.
    value: Option<Value>,
}

impl Variable {
    /// Creates a `Variable`. This does not add to the
    /// any scope.
    pub fn new(value: Option<Value>) -> Variable {
        Variable { value }
    }

    /// Returns a reference to the inner value.
    #[inline]
    pub fn value(&self) -> &Option<Value> {
        &self.value
    }

    pub fn array(&self) -> Option<std::slice::Iter<String>> {
        match &self.value {
            Some(Value::Array(elems)) => Some(elems.iter()),
            _ => None,
        }
    }

    /// References its value as `$foo`.
    pub fn as_str(&self) -> &str {
        value_as_str(&self.value)
    }

    /// References its value as `$foo[expr]`.
    pub fn value_at(&self, index: usize) -> Option<&str> {
        match &self.value {
            Some(Value::Array(elems)) => elems.get(index).map(|elem| elem.as_str()),
            _ => None,
        }
    }

    pub fn is_function(&self) -> bool {
        match &self.value {
            Some(Value::Function(_)) => true,
            _ => false,
        }
    }
}

/// A variable scope.
pub struct Frame {
    /// A `(variable name, varible)` map.
    vars: HashMap<String, Arc<Variable>>,
}

impl Frame {
    pub fn new() -> Frame {
        Frame {
            vars: HashMap::new(),
        }
    }

    pub fn iter(&self) -> hash_map::Iter<String, Arc<Variable>> {
        self.vars.iter()
    }

    pub fn define(&mut self, key: &str) {
        self.vars.insert(key.into(), Arc::new(Variable::new(None)));
    }

    pub fn set(&mut self, key: &str, value: Value) {
        self.vars
            .insert(key.into(), Arc::new(Variable::new(Some(value))));
    }

    pub fn remove(&mut self, key: &str, function: bool) -> Option<Arc<Variable>> {
        if let Some(var) = self.get(key) {
            let is_function = var.is_function();
            if is_function == function {
                return self.vars.remove(key);
            }
        }
        None
    }

    pub fn get(&self, key: &str) -> Option<Arc<Variable>> {
        self.vars.get(key).cloned()
    }

    /// Returns `$1`, `$2`, ...
    pub fn get_args(&self) -> Vec<Arc<Variable>> {
        let mut args = Vec::new();
        for i in 1.. {
            if let Some(var) = self.get(&i.to_string()) {
                args.push(var.clone());
            } else {
                break;
            }
        }

        args
    }

    /// Returns `$1`, `$2`, ...
    pub fn get_string_args(&self) -> Vec<String> {
        let mut args = Vec::new();
        for var in self.get_args() {
            if let Some(Value::String(value)) = var.value() {
                args.push(value.clone());
            }
        }

        args
    }

    /// Sets `$1`, `$2`, ...
    pub fn set_args(&mut self, args: &[String]) {
        for (i, arg) in args.iter().enumerate() {
            self.set(&(i + 1).to_string(), Value::String(arg.clone()));
        }
    }

    /// Sets `$<index>`.
    pub fn set_nth_arg(&mut self, index: usize, value: Value) {
        self.set(&index.to_string(), value)
    }

    /// Removes `$<index>`.
    pub fn remove_nth_arg(&mut self, index: usize) -> Option<Arc<Variable>> {
        self.remove(&index.to_string(), false)
    }

    /// Returns `$<index>`.
    pub fn get_nth_arg(&self, index: usize) -> Option<Arc<Variable>> {
        self.get(&index.to_string())
    }

    /// The number of function arguments (`$1`, ...).
    pub fn num_args(&self) -> usize {
        let mut num_args = 0;
        for i in 1..=9 {
            if self.get(&i.to_string()).is_none() {
                break;
            }

            num_args += 1;
        }

        num_args
    }
}
