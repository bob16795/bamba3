use bamba3;
use bamba3::parser::Parsable;

#[cfg(test)]
mod parser_tests {
    use super::*;

    #[test]
    fn addition_parsing() {
        let mut scn = bamba3::scanner::Scanner::new("5 + 3".to_string(), "???".to_string());

        bamba3::nodes::top_expression::TopExpression::parse(&mut scn).expect("failed to parse");

        assert!(scn.is_at_end());
    }

    #[test]
    fn unary_parsing() {
        let mut scn = bamba3::scanner::Scanner::new("!!(5 + 4)".to_string(), "???".to_string());

        bamba3::nodes::top_expression::TopExpression::parse(&mut scn).expect("failed to parse");

        assert!(scn.is_at_end());
    }
}
