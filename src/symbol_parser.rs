//! Parser for demangled Rust symbols.
//!
//! This module extracts function, type, and crate information from symbols like:
//! `<egui::plugin::PluginsOrdered>::for_each_dyn::<<egui::plugin::PluginsOrdered>::on_widget_under_pointer::{closure#0}>`

use std::fmt;

/// Parsed symbol with function, type, and crate information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    /// The function/method name including closures (e.g., `render::{closure#0}`)
    function: String,
    /// The type or containing module name
    type_: String,
    /// The crate name (first path segment)
    crate_: String,
}

impl Symbol {
    /// Parse a symbol string into a Symbol.
    pub fn parse(input: &str) -> Self {
        match SymbolExtractor::new(input).extract() {
            Ok(sym) => sym,
            Err(_) => Symbol {
                function: input.to_string(),
                type_: "<unknown>".to_string(),
                crate_: "<unknown>".to_string(),
            },
        }
    }

    /// Returns the function/method name (including closures).
    pub fn function(&self) -> &str {
        &self.function
    }

    /// Returns the type where the function is implemented.
    pub fn type_(&self) -> &str {
        &self.type_
    }

    /// Returns the crate name.
    pub fn crate_(&self) -> &str {
        &self.crate_
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}::{}", self.crate_, self.type_, self.function)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    pub position: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "parse error at position {}: {}",
            self.position, self.message
        )
    }
}

impl std::error::Error for ParseError {}

/// Parser that extracts symbol information.
struct SymbolExtractor<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> SymbolExtractor<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn extract(mut self) -> Result<Symbol, ParseError> {
        let (crate_, type_, function) = self.parse_qualified_path(false)?;

        if self.pos < self.input.len() {
            return Err(self.error(format!(
                "unexpected character '{}' at end of input",
                self.current_char().unwrap_or('?')
            )));
        }

        // Simpler loop to replace '{closure#N}' with 'λN' in function, without regex
        let function = if let Some(func) = function {
            let mut output = String::with_capacity(func.len());
            let mut i = 0;
            while i < func.len() {
                if func[i..].starts_with("{closure#") {
                    let num_start = i + 9;
                    let mut num_end = num_start;
                    while num_end < func.len() && func.as_bytes()[num_end].is_ascii_digit() {
                        num_end += 1;
                    }
                    if num_end < func.len() && func.as_bytes()[num_end] == b'}' {
                        // found pattern
                        output.push('λ');
                        output.push_str(&func[num_start..num_end]);
                        i = num_end + 1; // skip past '}'
                        continue;
                    }
                }
                output.push(func.as_bytes()[i] as char);
                i += 1;
            }
            Some(output)
        } else {
            None
        };

        Ok(Symbol {
            function: function.unwrap_or("<unknown>".to_string()),
            type_: type_.unwrap_or("<unknown>").to_string(),
            crate_: crate_.unwrap_or("<unknown>").to_string(),
        })
    }

    fn error(&self, message: String) -> ParseError {
        ParseError {
            message,
            position: self.pos,
        }
    }

    fn current_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn peek_char(&self) -> Option<char> {
        self.current_char()
    }

    fn peek_str(&self, s: &str) -> bool {
        self.input[self.pos..].starts_with(s)
    }

    fn advance(&mut self) {
        if let Some(c) = self.current_char() {
            self.pos += c.len_utf8();
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn expect(&mut self, expected: &str) -> Result<(), ParseError> {
        if self.input[self.pos..].starts_with(expected) {
            self.pos += expected.len();
            Ok(())
        } else {
            Err(self.error(format!("expected '{expected}'")))
        }
    }

    fn is_ident_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_ident_cont(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    /// Parse an identifier and return a slice into the input.
    fn parse_ident(&mut self) -> Result<&'a str, ParseError> {
        let start = self.pos;
        if let Some(c) = self.current_char() {
            if !Self::is_ident_start(c) {
                return Err(self.error(format!("expected identifier, found '{c}'")));
            }
            self.advance();
        } else {
            return Err(self.error("expected identifier, found end of input".to_string()));
        }

        while let Some(c) = self.current_char() {
            if Self::is_ident_cont(c) {
                self.advance();
            } else {
                break;
            }
        }

        let ident = &self.input[start..self.pos];

        // Skip optional hash suffix like [dfe5a278f1a6295e]
        self.skip_hash_suffix();

        Ok(ident)
    }

    /// Skip a hash suffix like `[dfe5a278f1a6295e]`
    fn skip_hash_suffix(&mut self) {
        if self.peek_char() != Some('[') {
            return;
        }
        let saved_pos = self.pos;
        self.advance(); // skip '['

        // Expect hex digits
        while let Some(c) = self.current_char() {
            if c.is_ascii_hexdigit() {
                self.advance();
            } else {
                break;
            }
        }

        // Expect closing ']'
        if self.peek_char() == Some(']') {
            self.advance();
        } else {
            // Not a valid hash suffix, restore position
            self.pos = saved_pos;
        }
    }

    /// Skip a number.
    fn skip_number(&mut self) {
        while let Some(c) = self.current_char() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Skip a braced special like `{closure#0}` or `{shim:vtable#0}`.
    fn skip_braced_special(&mut self) -> Result<(), ParseError> {
        self.expect("{")?;

        if self.peek_str("closure#") {
            self.expect("closure#")?;
            self.skip_number();
            self.expect("}")?;
            return Ok(());
        }

        if self.peek_str("shim:") {
            self.expect("shim:")?;
            let _ = self.parse_ident()?;
            self.expect("#")?;
            self.skip_number();
            self.expect("}")?;
            return Ok(());
        }

        Err(self.error("expected 'closure#' or 'shim:' after '{'".to_string()))
    }

    /// Skip generic parameters `<...>`, handling nested brackets.
    fn skip_generic_params(&mut self) -> Result<(), ParseError> {
        self.expect("<")?;
        let mut depth = 1;

        while depth > 0 {
            match self.current_char() {
                Some('<') => {
                    depth += 1;
                    self.advance();
                }
                Some('>') => {
                    depth -= 1;
                    self.advance();
                }
                Some(_) => self.advance(),
                None => return Err(self.error("unclosed '<' in generic parameters".to_string())),
            }
        }

        Ok(())
    }

    /// Parse a simple path and return (crate, type, function, function_with_closures).
    /// For simple paths, function_with_closures includes any trailing closures.
    fn parse_simple_path(&mut self) -> Result<SimplePathInfo<'a>, ParseError> {
        let mut second_to_last: Option<&'a str> = None;
        let mut function_start = self.pos;

        let ident = self.parse_ident()?;
        let first = Some(ident);
        let mut last = Some(ident);
        let mut function_end = self.pos;

        // Skip optional generics on first ident (don't include in function name)
        if self.peek_char() == Some('<') {
            self.skip_generic_params()?;
        }

        while self.peek_str("::") {
            // Check if this is turbofish (::< )
            let after_colons = self.pos + 2;
            if after_colons < self.input.len() && self.input[after_colons..].starts_with('<') {
                break;
            }

            self.expect("::")?;

            // Check for closure/shim - include in function name but don't update last
            if self.peek_char() == Some('{') {
                self.skip_braced_special()?;
                function_end = self.pos;
                break;
            }

            // Check for nested impl target - stop here
            if self.peek_char() == Some('<') {
                break;
            }

            second_to_last = last;
            // Update function_start to the start of this ident
            function_start = self.pos;
            let ident = self.parse_ident()?;
            last = Some(ident);
            function_end = self.pos;

            // Skip optional generics (don't include in function name)
            if self.peek_char() == Some('<') {
                self.skip_generic_params()?;
            }
        }

        // function_with_closures is from function_start to function_end
        let function_with_closures = Some(&self.input[function_start..function_end]);

        Ok(SimplePathInfo {
            crate_: first,
            type_for_simple: second_to_last,
            innermost: last,
            function_with_closures,
        })
    }

    /// Skip a type and return the crate name if found.
    fn skip_type(&mut self) -> Result<Option<&'a str>, ParseError> {
        self.skip_whitespace();

        if self.peek_char() == Some('&') {
            self.advance();
            self.skip_whitespace();
            if self.peek_char() == Some('\'') {
                self.advance();
                let _ = self.parse_ident()?;
                self.skip_whitespace();
            }
            if self.peek_str("mut ") {
                self.expect("mut ")?;
            }
            self.skip_whitespace();
            return self.skip_type();
        }

        if self.peek_char() == Some('(') {
            self.advance();
            self.skip_whitespace();
            while self.peek_char() != Some(')') {
                self.skip_qualified_path()?;
                self.skip_whitespace();
                if self.peek_char() == Some(',') {
                    self.advance();
                    self.skip_whitespace();
                }
            }
            self.expect(")")?;
            return Ok(None);
        }

        if self.peek_char() == Some('\'') {
            self.advance();
            let _ = self.parse_ident()?;
            return Ok(None);
        }

        if self.peek_char() == Some('<') {
            self.expect("<")?;
            self.skip_whitespace();
            let crate_ = self.skip_qualified_path()?;
            self.skip_whitespace();
            if self.peek_str("as ") {
                self.expect("as ")?;
                self.skip_qualified_path()?;
                self.skip_whitespace();
            }
            self.expect(">")?;
            return Ok(crate_);
        }

        if self.peek_str("dyn ") {
            self.expect("dyn ")?;
            return self.skip_type();
        }

        if self.peek_str("for<") {
            self.expect("for<")?;
            while self.peek_char() != Some('>') {
                self.skip_whitespace();
                if self.peek_char() == Some('\'') {
                    self.advance();
                    let _ = self.parse_ident()?;
                }
                self.skip_whitespace();
                if self.peek_char() == Some(',') {
                    self.advance();
                }
            }
            self.expect(">")?;
            self.skip_whitespace();
            return self.skip_type();
        }

        let info = self.parse_simple_path()?;
        Ok(info.crate_)
    }

    /// Skip a qualified path and return the crate name.
    fn skip_qualified_path(&mut self) -> Result<Option<&'a str>, ParseError> {
        self.skip_whitespace();
        let crate_ = self.skip_type()?;

        while self.peek_str("::") {
            if self.pos + 2 < self.input.len() && self.input[self.pos + 2..].starts_with('<') {
                self.expect("::")?;
                self.skip_generic_params()?;
                continue;
            }

            self.expect("::")?;

            if self.peek_char() == Some('{') {
                self.skip_braced_special()?;
                continue;
            }

            if self.current_char().is_none()
                || self.peek_char() == Some('>')
                || self.peek_char() == Some(',')
            {
                break;
            }

            let _ = self.parse_ident()?;

            if self.peek_str("::<") {
                self.expect("::")?;
                self.skip_generic_params()?;
            } else if self.peek_char() == Some('<') {
                self.skip_generic_params()?;
            }
        }

        Ok(crate_)
    }

    /// Parse `<Type>` or `<Type as Trait>` and extract base type info.
    fn parse_impl_target(&mut self) -> Result<(Option<&'a str>, Option<&'a str>), ParseError> {
        self.expect("<")?;
        self.skip_whitespace();

        let (crate_, type_, _) = self.parse_qualified_path(true)?;

        self.skip_whitespace();

        if self.peek_str("as ") {
            self.expect("as ")?;
            self.skip_qualified_path()?;
            self.skip_whitespace();
        }

        self.expect(">")?;

        Ok((crate_, type_))
    }

    /// Parse a type and return info.
    fn parse_type_info(&mut self) -> Result<TypeInfo<'a>, ParseError> {
        self.skip_whitespace();

        if self.peek_char() == Some('&') {
            self.advance();
            self.skip_whitespace();
            if self.peek_char() == Some('\'') {
                self.advance();
                let _ = self.parse_ident()?;
                self.skip_whitespace();
            }
            if self.peek_str("mut ") {
                self.expect("mut ")?;
            }
            self.skip_whitespace();
            return self.parse_type_info();
        }

        if self.peek_char() == Some('(') {
            self.advance();
            self.skip_whitespace();
            while self.peek_char() != Some(')') {
                self.skip_qualified_path()?;
                self.skip_whitespace();
                if self.peek_char() == Some(',') {
                    self.advance();
                    self.skip_whitespace();
                }
            }
            self.expect(")")?;
            return Ok(TypeInfo::empty());
        }

        if self.peek_char() == Some('\'') {
            self.advance();
            let _ = self.parse_ident()?;
            return Ok(TypeInfo::empty());
        }

        if self.peek_char() == Some('<') {
            let (crate_, type_) = self.parse_impl_target()?;
            return Ok(TypeInfo::impl_target(crate_, type_));
        }

        if self.peek_str("dyn ") {
            self.expect("dyn ")?;
            return self.parse_type_info();
        }

        if self.peek_str("for<") {
            self.expect("for<")?;
            while self.peek_char() != Some('>') {
                self.skip_whitespace();
                if self.peek_char() == Some('\'') {
                    self.advance();
                    let _ = self.parse_ident()?;
                }
                self.skip_whitespace();
                if self.peek_char() == Some(',') {
                    self.advance();
                }
            }
            self.expect(">")?;
            self.skip_whitespace();
            return self.parse_type_info();
        }

        let info = self.parse_simple_path()?;
        Ok(TypeInfo::simple_path(info))
    }

    /// Check if the next tokens are `::` followed by `<` (turbofish).
    fn peek_turbofish(&self) -> bool {
        if !self.peek_str("::") {
            return false;
        }
        let after_colons = self.pos + 2;
        after_colons < self.input.len() && self.input[after_colons..].starts_with('<')
    }

    /// Parse a qualified path and return (crate, type, function).
    fn parse_qualified_path(
        &mut self,
        for_type: bool,
    ) -> Result<(Option<&'a str>, Option<&'a str>, Option<&'a str>), ParseError> {
        self.skip_whitespace();

        let base = self.parse_type_info()?;

        // Track segments after the base
        let mut has_segments = false;
        let mut function_start: Option<usize> = None;
        let mut function_end: Option<usize> = None;

        while self.peek_str("::") {
            let saved_pos = self.pos;

            if self.peek_turbofish() {
                self.expect("::")?;
                self.skip_generic_params()?;
                // Don't extend function_end for turbofish - just skip generics
                continue;
            }

            self.expect("::")?;

            if self.peek_char() == Some('{') {
                // Closure/shim - include in function span
                self.skip_braced_special()?;
                function_end = Some(self.pos);
                has_segments = true;
                continue;
            }

            if self.current_char().is_none()
                || self.peek_char() == Some('>')
                || self.peek_char() == Some(',')
            {
                self.pos = saved_pos;
                break;
            }

            // Parse identifier - this starts the function name (or resets it)
            let ident_start = self.pos;
            let _ = self.parse_ident()?;

            // Update function to start at this identifier
            function_start = Some(ident_start);
            function_end = Some(self.pos);
            has_segments = true;

            // Skip any generics (turbofish or direct) but don't include in function name
            if self.peek_turbofish() {
                self.expect("::")?;
                self.skip_generic_params()?;
            } else if self.peek_char() == Some('<') {
                self.skip_generic_params()?;
            }
        }

        if !has_segments {
            // No segments after base
            match base {
                TypeInfo::SimplePath(info) => {
                    let type_ = if for_type {
                        info.innermost
                    } else {
                        info.type_for_simple
                    };
                    Ok((info.crate_, type_, info.function_with_closures))
                }
                TypeInfo::ImplTarget { crate_, type_ } => Ok((crate_, type_, type_)),
                TypeInfo::Empty => Ok((None, None, None)),
            }
        } else {
            // Has segments after base - function is from function_start to function_end
            let function = match (function_start, function_end) {
                (Some(start), Some(end)) => Some(&self.input[start..end]),
                _ => None,
            };
            let (base_crate, base_type) = base.crate_and_type();
            Ok((base_crate, base_type, function))
        }
    }
}

/// Info from parsing a simple path.
struct SimplePathInfo<'a> {
    crate_: Option<&'a str>,
    type_for_simple: Option<&'a str>,
    innermost: Option<&'a str>,
    function_with_closures: Option<&'a str>,
}

/// Info from parsing a type.
enum TypeInfo<'a> {
    Empty,
    ImplTarget {
        crate_: Option<&'a str>,
        type_: Option<&'a str>,
    },
    SimplePath(SimplePathInfo<'a>),
}

impl<'a> TypeInfo<'a> {
    fn empty() -> Self {
        TypeInfo::Empty
    }

    fn impl_target(crate_: Option<&'a str>, type_: Option<&'a str>) -> Self {
        TypeInfo::ImplTarget { crate_, type_ }
    }

    fn simple_path(info: SimplePathInfo<'a>) -> Self {
        TypeInfo::SimplePath(info)
    }

    fn crate_and_type(&self) -> (Option<&'a str>, Option<&'a str>) {
        match self {
            TypeInfo::Empty => (None, None),
            TypeInfo::ImplTarget { crate_, type_ } => (*crate_, *type_),
            TypeInfo::SimplePath(info) => (info.crate_, info.innermost),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_simple_method() {
        let sym = Symbol::parse("<gaze::app::AppState>::render").unwrap();
        assert_eq!(sym.function(), "render");
        assert_eq!(sym.type_(), "AppState");
        assert_eq!(sym.crate_(), "gaze");
    }

    #[test]
    fn test_symbol_method_with_closure() {
        let sym = Symbol::parse("<gaze::app::AppState>::render::{closure#0}").unwrap();
        assert_eq!(sym.function(), "render::{closure#0}");
        assert_eq!(sym.type_(), "AppState");
        assert_eq!(sym.crate_(), "gaze");
    }

    #[test]
    fn test_symbol_trait_impl() {
        let sym = Symbol::parse("<<gaze::app::AppState>::render::{closure#0} as core::ops::function::FnOnce<(&mut egui::ui::Ui,)>>::call_once").unwrap();
        assert_eq!(sym.function(), "call_once");
        assert_eq!(sym.type_(), "AppState");
        assert_eq!(sym.crate_(), "gaze");
    }

    #[test]
    fn test_symbol_generic_type() {
        let sym = Symbol::parse("std::vec::Vec<T>::push").unwrap();
        assert_eq!(sym.function(), "push");
        assert_eq!(sym.type_(), "Vec");
        assert_eq!(sym.crate_(), "std");
    }

    #[test]
    fn test_symbol_free_function() {
        let sym = Symbol::parse("std::io::stdin").unwrap();
        assert_eq!(sym.function(), "stdin");
        assert_eq!(sym.type_(), "io");
        assert_eq!(sym.crate_(), "std");
    }

    #[test]
    fn test_symbol_with_turbofish() {
        let sym = Symbol::parse("<Foo>::bar::<Baz>").unwrap();
        assert_eq!(sym.function(), "bar");
        assert_eq!(sym.type_(), "Foo");
        assert_eq!(sym.crate_(), "Foo");
    }

    #[test]
    fn test_symbol_complex_hrtb() {
        let sym = Symbol::parse("<alloc::boxed::Box<dyn for<'a> core::ops::function::FnOnce<(&'a mut egui::ui::Ui,), Output = ()>>>").unwrap();
        assert_eq!(sym.crate_(), "alloc");
    }

    #[test]
    fn test_generic_return_type() {
        let sym = Symbol::parse("<egui::containers::frame::Frame>::show::<(), <jsx::jsx::EguiElement>::render<<jsx::jsx_view::NodeContext>::render::{closure#0}>::{closure#2}::{closure#0}>").unwrap();
        assert_eq!(sym.crate_(), "egui");
        assert_eq!(sym.function(), "show");
        assert_eq!(sym.type_(), "Frame");
    }

    #[test]
    fn moar() {
        let sym = Symbol::parse("<alloc::boxed::Box<dyn for<'a> core::ops::function::FnOnce<(&'a mut egui::ui::Ui,), Output = ()>> as core::ops::function::FnOnce<(&mut egui::ui::Ui,)>>::call_once").unwrap();
        assert_eq!(sym.crate_(), "alloc");
        assert_eq!(sym.function(), "call_once");
        assert_eq!(sym.type_(), "Box");
    }

    #[test]
    fn test_shim() {
        let sym = Symbol::parse("<<gaze::app::AppState>::render::{closure#0} as core::ops::function::FnOnce<(&mut egui::ui::Ui,)>>::call_once::{shim:vtable#0}").unwrap();
        assert_eq!(sym.function(), "call_once::{shim:vtable#0}");
        assert_eq!(sym.type_(), "AppState");
        assert_eq!(sym.crate_(), "gaze");
    }

    #[test]
    fn test_simple_path() {
        let sym = Symbol::parse("foo::bar::baz").unwrap();
        assert_eq!(sym.function(), "baz");
        assert_eq!(sym.type_(), "bar");
        assert_eq!(sym.crate_(), "foo");
    }

    #[test]
    fn test_single_ident() {
        let sym = Symbol::parse("foo").unwrap();
        assert_eq!(sym.function(), "foo");
        assert_eq!(sym.crate_(), "foo");
    }

    #[test]
    fn test_simple_path_with_closure() {
        let sym = Symbol::parse("foo::bar::baz::{closure#0}").unwrap();
        assert_eq!(sym.function(), "baz::{closure#0}");
        assert_eq!(sym.type_(), "bar");
        assert_eq!(sym.crate_(), "foo");
    }

    #[test]
    fn test_nested_closures() {
        let sym = Symbol::parse("<Foo>::bar::{closure#0}::{closure#1}").unwrap();
        assert_eq!(sym.function(), "bar::{closure#0}::{closure#1}");
        assert_eq!(sym.type_(), "Foo");
        assert_eq!(sym.crate_(), "Foo");
    }

    #[test]
    fn test_symbol_with_hash() {
        let sym = Symbol::parse("<egui[dfe5a278f1a6295e]::ui::Ui>::interact").unwrap();
        assert_eq!(sym.function(), "interact");
        assert_eq!(sym.type_(), "Ui");
        assert_eq!(sym.crate_(), "egui");
    }

    fn test_symbol_with_hash_and_generics() {
        let sym = Symbol::parse("eframe[7cb594708737837b]::native::run::with_event_loop::<core[4fd6bfa457654cdd]::result::Result<(), eframe[7cb594708737837b]::Error>, eframe[7cb594708737837b]::native::run::run_wgpu::{closure#0}>::{closure#0}").unwrap();
        assert_eq!(sym.function(), "with_event_loop::{closure#0}");
        assert_eq!(sym.type_(), "native");
        assert_eq!(sym.crate_(), "eframe");
    }
}
