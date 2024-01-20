use std::collections::HashSet;

use uuid::Uuid;

type TaskId = Uuid;

#[derive(Debug, PartialEq, Clone)]
pub struct Task {
    pub id: TaskId,
    pub name: String,
    pub description: String,
    pub tags: HashSet<String>,
    pub project: Option<String>
}

impl Task {
    pub fn new(name: String, description: String, tags: HashSet<String>, project: Option<String>) -> Self {
        Self { id: Uuid::new_v4(), name, description, tags, project }
    }
}

impl From<&str> for Task {
    fn from(s: &str) -> Self {
        let mut text = String::new();
        let mut tags: HashSet<String> = HashSet::new();
        let mut project = String::new();

        for word in s.split_whitespace() {
            match word.chars().next() {
                Some('#') => project = word[1..].to_string(),
                Some('@') => {tags.insert(word[1..].to_string());},
                _ => {
                    text.push_str(word);
                    text.push_str(" ")
                },
            }
        }

        Self::new(text.trim_end().to_string(), String::new(), tags, project.into())
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Or,
    And,
    Not,
    Text(String),
    Tag(String),
    Project(String),
    LParen,
    RParen,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Identifier(String),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn evaluate(&self, task: &Task) -> bool {
        match self {
            Expr::Identifier(identifier) => {
                // check for type of identifier based on first character
                match identifier.chars().next().unwrap() {
                    '@' => task.tags.contains(&identifier[1..]),
                    '#' => task.project == Some(identifier[1..].to_string()),
                    _ => task.name.contains(identifier)
                }
            },
            Expr::Not(expr) => {
                // Negate the evaluation of the inner expression
                !expr.evaluate(task)
            },
            Expr::And(left, right) => {
                // Both left and right expressions must be true
                left.evaluate(task) && right.evaluate(task)
            },
            Expr::Or(left, right) => {
                // Either left or right expression must be true
                left.evaluate(task) || right.evaluate(task)
            },
        }
    }
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut iter = input.chars().peekable();

    while let Some(&ch) = iter.peek() {
        match ch {
            '|' => {
                iter.next();
                if iter.next() == Some('|') {
                    tokens.push(Token::Or);
                }
            },
            '&' => {
                iter.next();
                if iter.next() == Some('&') {
                    tokens.push(Token::And);
                }
            },
            '!' => {
                iter.next();
                tokens.push(Token::Not);
            },
            '(' => {
                iter.next();
                tokens.push(Token::LParen);
            },
            ')' => {
                iter.next();
                tokens.push(Token::RParen);
            },
            '@' => {
                let mut ident = String::new();
                iter.next();
                ident.push(ch);
                while let Some(&ch) = iter.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        ident.push(ch);
                        iter.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Tag(ident));
            },
            '#' => {
                let mut ident = String::new();
                iter.next();
                ident.push(ch);
                while let Some(&ch) = iter.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        ident.push(ch);
                        iter.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Project(ident));
            },
            '"' => {
                iter.next(); // Skip the opening quote
                let mut ident = String::new();
                while let Some(&ch) = iter.peek() {
                    match ch {
                        '"' => {
                            iter.next(); // Skip the closing quote
                            break;
                        },
                        '\\' => { // Escape character
                            iter.next(); // Skip the backslash
                            if let Some(&escaped_ch) = iter.peek() {
                                ident.push(escaped_ch); // Add the escaped character
                                iter.next();
                            }
                        },
                        _ => {
                            ident.push(ch);
                            iter.next();
                        }
                    }
                }
                tokens.push(Token::Text(ident));
            },
            _ => { iter.next(); } // Skip whitespace and unrecognized characters
        }
    }

    tokens
}

pub struct Parser {
    pub tokens: Vec<Token>,
    current: usize,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let tokens = tokenize(input);
        Parser { tokens, current: 0, errors: Vec::new() }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        let expr = self.parse_expr();
        if self.has_errors() {
            None
        } else {
            expr
        }
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        let mut expr = self.parse_term()?;
        while self.match_token(&Token::Or) {
            expr = if let Some(right) = self.parse_term() {
                Expr::Or(Box::new(expr), Box::new(right))
            } else {
                self.error("Expected expression after '||'");
                return None;
            };
        }
        Some(expr)
    }

    fn parse_term(&mut self) -> Option<Expr> {
        let mut expr = self.parse_factor()?;
        while self.match_token(&Token::And) {
            expr = if let Some(right) = self.parse_factor() {
                Expr::And(Box::new(expr), Box::new(right))
            } else {
                self.error("Expected expression after '&&'");
                return None;
            };
        }
        Some(expr)
    }

    fn parse_factor(&mut self) -> Option<Expr> {
        if self.match_token(&Token::Not) {
            let expr = self.parse_factor()?;
            return Some(Expr::Not(Box::new(expr)));
        }

        if self.match_token(&Token::LParen) {
            let expr = self.parse_expr()?;
            if !self.match_token(&Token::RParen) {
                self.error("Expected ')' after expression");
                return None;
            }
            return Some(expr);
        }

        if let Some(ident) = self.consume_identifier() {
            return Some(Expr::Identifier(ident));
        }

        self.error("Expected expression");
        None
    }

    fn match_token(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.current += 1;
            true
        } else {
            false
        }
    }

    fn check(&self, token: &Token) -> bool {
        self.tokens.get(self.current).map_or(false, |t| t == token)
    }

    fn consume_identifier(&mut self) -> Option<String> {
        match self.tokens.get(self.current) {
            Some(Token::Text(ident)) => {
                self.current += 1;
                Some(ident.clone())
            }
            Some(Token::Tag(ident)) => {
                self.current += 1;
                Some(ident.clone())
            }
            Some(Token::Project(ident)) => {
                self.current += 1;
                Some(ident.clone())
            }
            _ => None,
        }
    }

    fn error(&mut self, message: &str) {
        let error_message = format!("Error at position {}: {}", self.current, message);
        self.errors.push(error_message);
    }

    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quoted_string_exact() {
        let input = r#""some task""#;
        let task = Task::from("some task");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        assert_eq!(passed, true);
    }

    #[test]
    fn test_quoted_string_partial() {
        let input = r#""task""#;
        let task = Task::from("some task");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        assert_eq!(passed, true);
    }

    #[test]
    fn test_tag() {
        let input = r#"@tag"#;
        let task = Task::from("some task @tag");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        assert_eq!(passed, true);
    }

    #[test]
    fn test_project() {
        let input = r#"#project"#;
        let task = Task::from("some task #project");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        assert_eq!(passed, true);
    }

    // Negation tests

    #[test]
    fn test_negate_quoted_string_exact() {
        let input = r#"!"some task""#;
        let task = Task::from("some task");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        assert_eq!(passed, false);
    }

    #[test]
    fn test_negate_quoted_string_partial() {
        let input = r#"!"task""#;
        let task = Task::from("some task");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        assert_eq!(passed, false);
    }

    #[test]
    fn test_negate_tag() {
        let input = r#"!@tag"#;
        let task = Task::from("some task @tag");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        assert_eq!(passed, false);
    }

    #[test]
    fn test_negate_project() {
        let input = r#"!#project"#;
        let task = Task::from("some task #project");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        assert_eq!(passed, false);
    }

    // OR tests

    #[test]
    fn test_quoted_string_or_project_or_tag() {
        let input = r#"#project || @tag || "some task""#;
        let task = Task::from("some task");
        let task2 = Task::from("some #project");
        let task3 = Task::from("some @tag");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        let passed2 = expr.evaluate(&task2);
        let passed3 = expr.evaluate(&task3);
        assert_eq!(passed, true);
        assert_eq!(passed2, true);
        assert_eq!(passed3, true);
    }

    #[test]
    fn test_quoted_string_or_project() {
        let input = r#"#project || "some task""#;
        let task = Task::from("some task");
        let task2 = Task::from("some #project");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        let passed2 = expr.evaluate(&task2);
        assert_eq!(passed, true);
        assert_eq!(passed2, true);
    }

    #[test]
    fn test_quoted_string_or_tag() {
        let input = r#"@tag || "some task""#;
        let task = Task::from("some task");
        let task2 = Task::from("some @tag");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        let passed2 = expr.evaluate(&task2);
        assert_eq!(passed, true);
        assert_eq!(passed2, true);
    }

    #[test]
    fn test_tag_or_project() {
        let input = r#"#project || @tag"#;
        let task = Task::from("some task @tag");
        let task2 = Task::from("some #project");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        let passed2 = expr.evaluate(&task2);
        assert_eq!(passed, true);
        assert_eq!(passed2, true);
    }

    // AND tests

    #[test]
    fn test_quoted_string_and_project_and_tag() {
        let input = r#"#project && @tag && "some task""#;
        let task = Task::from("some task @tag #project");
        let task2 = Task::from("some #project");
        let task3 = Task::from("some @tag");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        let passed2 = expr.evaluate(&task2);
        let passed3 = expr.evaluate(&task3);
        assert_eq!(passed, true);
        assert_eq!(passed2, false);
        assert_eq!(passed3, false);
    }

    #[test]
    fn test_quoted_string_and_project() {
        let input = r#"#project && "some task""#;
        let task = Task::from("some task");
        let task2 = Task::from("some task #project");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        let passed2 = expr.evaluate(&task2);
        assert_eq!(passed, false);
        assert_eq!(passed2, true);
    }

    #[test]
    fn test_quoted_string_and_tag() {
        let input = r#"@tag && "some task""#;
        let task = Task::from("some task");
        let task2 = Task::from("some task @tag");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        let passed2 = expr.evaluate(&task2);
        assert_eq!(passed, false);
        assert_eq!(passed2, true);
    }

    #[test]
    fn test_tag_and_project() {
        let input = r#"#project && @tag"#;
        let task = Task::from("some task @tag");
        let task2 = Task::from("some #project");
        let task3 = Task::from("some @tag #project");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        let passed2 = expr.evaluate(&task2);
        let passed3 = expr.evaluate(&task3);
        assert_eq!(passed, false);
        assert_eq!(passed2, false);
        assert_eq!(passed3, true);
    }

    // Precedence tests

    #[test]
    fn test_string_and_tag_or_project() {
        let input = r#""some" && #project || @tag"#;
        let task = Task::from("some task @other_tag");
        let task2 = Task::from("some #project");
        let task3 = Task::from("some @tag #project");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        let passed2 = expr.evaluate(&task2);
        let passed3 = expr.evaluate(&task3);
        assert_eq!(passed, false);
        assert_eq!(passed2, true);
        assert_eq!(passed3, true);
    }

    #[test]
    fn test_tag_and_project_or_string() {
        let input = r#"#project && @tag || "some""#;
        let task = Task::from("some task @tag");
        let task2 = Task::from("some #project");
        let task3 = Task::from("some @tag #project");
        let mut parser = Parser::new(input);
        let expr = parser.parse().unwrap();
        let passed = expr.evaluate(&task);
        let passed2 = expr.evaluate(&task2);
        let passed3 = expr.evaluate(&task3);
        assert_eq!(passed, true);
        assert_eq!(passed2, true);
        assert_eq!(passed3, true);
    }
}
