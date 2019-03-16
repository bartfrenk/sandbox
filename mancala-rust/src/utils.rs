// Concatenates the elements of `xs` seperated by `sep`.
pub fn join<'a, I, S: 'a + ToString>(xs: I, sep: &str) -> String
where
    I: IntoIterator<Item = &'a S>,
{
    let empty = String::from("");
    let concat = |acc: String, x: &S| {
        if acc.is_empty() {
            acc + &x.to_string()
        } else {
            acc + sep + &x.to_string()
        }
    };
    xs.into_iter().fold(empty, concat)
}
