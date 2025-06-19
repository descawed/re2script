use anyhow::{anyhow, Result};
use chumsky::prelude::*;

use crate::ast::*;

fn parser<'a>() -> impl Parser<'a, &'a str, Vec<Function>, extra::Err<Rich<'a, char>>> {
    let line_comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded_by(text::inline_whitespace())
        .ignored();

    let block_comment = just("/*")
        .then(any().and_is(just("*/").not()).repeated())
        .then(just("*/"))
        .padded_by(text::inline_whitespace())
        .ignored();

    let inline_ignore = block_comment.or(line_comment).or(text::inline_whitespace().at_least(1)).repeated();

    // because we use newlines to delimit instructions, we need to be careful using this, because it
    // can end up stealing newlines that another parser needed as a delimiter
    let ignore = block_comment.or(line_comment).or(text::whitespace().at_least(1)).repeated();

    // support both CRLF and LF
    let line_end = just('\r').or_not().ignore_then(just('\n')).padded_by(inline_ignore).repeated().at_least(1);

    let dec = text::digits(10)
        .to_slice()
        .map(|s: &str| Expression::Int(s.parse().unwrap()));

    let hex = just("0x")
        .ignore_then(text::digits(16))
        .to_slice()
        .map(|s: &str| Expression::Int(i32::from_str_radix(s, 16).unwrap()));

    let num = hex.or(dec);

    let neg_num = just('-')
        .ignore_then(num)
        .map(|n| match n {
            Expression::Int(i) => Expression::Int(-i),
            _ => unreachable!(),
        });

    let ident = text::ident().to_slice().map(|s: &str| Expression::Identifier(String::from(s)));

    let var = just("var[")
        .ignore_then(num.padded_by(ignore))
        .then_ignore(just(']'))
        .map(|e| match e {
            // TODO: trigger some kind of error on overflow
            Expression::Int(i) => Expression::Var(i as u8),
            _ => unreachable!(),
        });

    let atom = var.or(ident).or(neg_num).or(num);

    let bit_flags = atom
        .separated_by(just('|').padded_by(inline_ignore))
        .at_least(2)
        .collect::<Vec<_>>()
        .map(Expression::BitFlags);

    let expr = bit_flags.or(atom);

    let arg = text::ident()
        .to_slice()
        .then_ignore(just(':').padded_by(ignore))
        .or_not()
        .then(expr.padded_by(ignore))
        .map(|(n, e)| Argument::new(n.map(String::from), e));

    let arg_list = just('(')
        .padded_by(inline_ignore)
        .ignore_then(arg.padded_by(ignore).separated_by(just(',')).collect::<Vec<_>>())
        .then_ignore(just(')').padded_by(inline_ignore));

    let goto = text::keyword("goto")
        .padded_by(inline_ignore)
        .ignore_then(text::ident().to_slice())
        .map(|s: &str| Statement::GoTo(String::from(s)));

    let for_loop = text::keyword("for")
        .padded_by(inline_ignore)
        .ignore_then(atom)
        .map(Statement::ForLoop);

    // need to check this last because a single identifier by itself (e.g. goto or for above) will
    // match this
    let instruction = text::ident()
        .to_slice()
        .padded_by(inline_ignore)
        .then(arg_list.or_not())
        .map(|(n, a)| Statement::Instruction(String::from(n), a.unwrap_or_default()));

    let op = choice(
        (
            just("==").to(Operator::Equal),
            just("!=").to(Operator::NotEqual),
            just("<=").to(Operator::LessThanOrEqual),
            just("<").to(Operator::LessThan),
            just(">=").to(Operator::GreaterThanOrEqual),
            just(">").to(Operator::GreaterThan),
            just("+=").to(Operator::Add),
            just("-=").to(Operator::Sub),
            just("*=").to(Operator::Mul),
            just("/=").to(Operator::Div),
            just("%=").to(Operator::Mod),
            just("<<=").to(Operator::LeftShift),
            just(">>>=").to(Operator::SignedRightShift),
            just(">>=").to(Operator::RightShift),
            just("&=").to(Operator::BitAnd),
            just("|=").to(Operator::BitOr),
            just("^=").to(Operator::BitXor),
            just("&").to(Operator::BitMask),
            just("=").to(Operator::Assign),
        )
    );

    let var_not = just('~')
        .ignore_then(var)
        .map(|v| match v {
            Expression::Var(i) => Statement::VarOperation { id: i, op: Operator::BitNot, source: Expression::Int(0) },
            _ => unreachable!(),
        });

    let var_binary = var
        .then(op.padded_by(inline_ignore))
        .then(expr)
        .map(|((v, op), e)| match v {
            Expression::Var(id) => Statement::VarOperation { id, op, source: e },
            _ => unreachable!(),
        });

    let var_op = var_not.or(var_binary).or(var.map(|v| match v {
        Expression::Var(id) => Statement::VarOperation { id, op: Operator::Value, source: Expression::Int(0) },
        _ => unreachable!(),
    })).boxed();

    let flag = just("flag[")
        .ignore_then(num.padded_by(ignore))
        .then_ignore(just("]["))
        .then(num.padded_by(ignore))
        .then_ignore(just(']'))
        .map(|(n, b)| match (n, b) {
            (Expression::Int(bank), Expression::Int(bit)) => (bank as u8, bit as u8),
            _ => unreachable!(),
        });

    let flag_not = just('!')
        .ignore_then(flag)
        .map(|(bank, bit)| Statement::FlagCheck { bank, bit, value: false });

    let flag_assign = flag
        .then_ignore(just('=').padded_by(inline_ignore))
        .then(atom)
        .map(|((bank, bit), e)| Statement::FlagSet { bank, bit, value: e });

    let flag_op = flag_not.or(flag_assign).or(flag.map(|(bank, bit)| Statement::FlagCheck { bank, bit, value: true })).boxed();

    let basic_stmt = flag_op.or(var_op).or(goto).or(for_loop).or(instruction).boxed();

    let stmt = recursive(|stmt| {
        let block = basic_stmt
            .clone()
            .padded_by(ignore)
            .then_ignore(just('{').padded_by(inline_ignore))
            .then_ignore(line_end)
            .then(stmt.clone().separated_by(line_end).collect::<Vec<_>>().padded_by(ignore))
            .then_ignore(just('}'))
            .map(|(head, body)| Statement::Block(Box::new(head), body));

        let label = text::ident()
            .to_slice()
            .then_ignore(just(':').padded_by(inline_ignore))
            .padded_by(ignore)
            .then(stmt.padded_by(inline_ignore))
            .map(|(i, s)| Statement::Label(String::from(i), Box::new(s)));

        label.or(block).or(basic_stmt).boxed()
    });

    let block_body = just('{')
        .padded_by(inline_ignore)
        .ignore_then(line_end)
        .ignore_then(stmt.separated_by(line_end).collect::<Vec<_>>().padded_by(ignore))
        .then_ignore(just('}'));

    let init_func = text::keyword("init")
        .or(text::keyword("main"))
        .padded_by(ignore)
        .ignore_then(block_body.clone())
        .map(|body| Function::new(FunctionName::Init, body));

    let exec_func = text::keyword("function")
        .padded_by(inline_ignore)
        .ignore_then(text::ident().to_slice())
        .padded_by(ignore)
        .then(block_body)
        .map(|(name, body)| Function::new(FunctionName::Exec(String::from(name)), body));

    let function = init_func.or(exec_func).boxed();

    function
        .separated_by(line_end)
        .at_least(1)
        .collect::<Vec<_>>()
        .padded_by(ignore)
        .then_ignore(end())
}

fn get_line_number(text: &str, index: usize) -> (usize, usize) {
    let fragment = &text[..index];
    let line_start = fragment.rfind('\n').unwrap_or(0);
    let line_num = fragment.matches('\n').count() + 1;
    (line_num, index - line_start)
}

pub fn parse<T: AsRef<str>>(script: T) -> Result<Vec<Function>> {
    let script = script.as_ref();
    let parsed = parser().parse(script);
    parsed.into_result().map_err(|errors| {
        let mut s = String::new();
        for e in errors {
            let span = e.span();
            let (start_line, start_char) = get_line_number(script, span.start);
            let (end_line, end_char) = get_line_number(script, span.end);
            let msg = format!(
                "{} (line {}:{} to line {}:{})\n",
                e.reason(),
                start_line,
                start_char,
                end_line,
                end_char,
            );
            s.push_str(&msg);
        }

        anyhow!("Script parsing failed:\n{}", s)
    })
}