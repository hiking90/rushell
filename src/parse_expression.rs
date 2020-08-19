// fn parse_expression(pair: pest::iterators::Pair<Rule>) -> f64 {
//     match pair.as_rule() {
//         Rule::expression |
//         Rule::multiply_exp |
//         Rule::pow_exp => {
//             let mut value = 0.;
//             let mut op: Box<dyn Fn(f64,f64)->f64> = Box::new(|_l,r|->f64 { r });

//             for inner in pair.into_inner() {
//                 match inner.as_rule() {
//                     Rule::PLUS => op = Box::new(|l,r| {l + r}),
//                     Rule::MINUS => op = Box::new(|l,r| {l - r}),
//                     Rule::TIMES => op = Box::new(|l,r| {l*r}),
//                     Rule::DIV => {
//                         op = Box::new(|l,r| {l/r});
//                     },
//                     Rule::POW => op = Box::new(|l,r| {l.powf(r)}),
//                     _ => {
//                         let rvalue = parse_expression(inner);
//                         value = op(value, rvalue);
//                     }
//                 }
//             }

//             value
//         },
//         Rule::plus_signed_atom | Rule::minus_signed_atom => {
//             let mut pair = pair.into_inner();
//             let sign = pair.next().unwrap().as_str().to_owned();
//             let mut value = parse_expression(pair.next().unwrap());
//             if sign == "-" {
//                 value *= -1.;
//             }
//             value
//         },
//         Rule::atom | Rule::scientific => {
//             println!("{}", pair);
//             parse_expression(pair.into_inner().next().unwrap())
//         },
//         Rule::NUMBER => pair.as_str().parse::<f64>().unwrap(),
//         _ => unreachable!(),
//     }
// }