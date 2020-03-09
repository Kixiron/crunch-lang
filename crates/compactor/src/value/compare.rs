use core::cmp;

#[derive(Debug, Clone, Copy)]
/// Gauges the relationship between two Values,
/// Always reflects Equivalency, sometimes also reflects Greater/Less than.
pub enum Compare {
    Equal,
    Less,
    Greater,
    Unequal,
    /// Sometimes, things just don't line up.
    /// (trying to compare a Bool to a BigInt, are ya?)
    Incomparable,
}

impl Compare {
    /// Generates a Comparison which respects the nuances of greater than and equal to.
    /// Usually only applicable for numbers.
    ///
    /// Comparison to i.e. `std::f32::NAN` simply yields `Unequal`, as is the case with Rust.
    pub fn ordering<O: cmp::PartialOrd>(a: &O, b: &O) -> Self {
        use cmp::Ordering;

        a.partial_cmp(b)
            .map(|c| match c {
                Ordering::Less => Compare::Less,
                Ordering::Greater => Compare::Greater,
                Ordering::Equal => Compare::Equal,
            })
            .unwrap_or(Compare::Unequal)
    }

    /// A Bool, for example, can't be "less than" another Bool, but it can be different.
    /// Types that impl PartialOrd probably shouldn't be compared this way, prefer `Compare::full`.
    pub fn just_equality<E: Eq>(a: &E, b: &E) -> Self {
        if a == b {
            Compare::Equal
        } else {
            Compare::Unequal
        }
    }
}
