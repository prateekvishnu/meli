/*! A parser module for user commands passed through the Ex mode.
*/
use std;
use nom::digit;


named!(usize_c<usize>,
       map_res!(map_res!(ws!(digit), std::str::from_utf8), std::str::FromStr::from_str));

named!(pub goto<usize>,
       preceded!(tag!("b "),  
                 call!(usize_c))
      );
