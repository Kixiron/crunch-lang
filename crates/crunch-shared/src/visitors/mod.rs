pub mod ast;
pub mod hir;
pub mod mir;

pub trait Visit<T> {
    type Output;

    fn visit(&mut self, data: &T) -> Self::Output;
}

impl<T, V> Visit<&'_ T> for V
where
    V: Visit<T>,
{
    type Output = <V as Visit<T>>::Output;

    #[crate::instrument(name = "reference", skip(self, reference))]
    fn visit(&mut self, reference: &&'_ T) -> Self::Output {
        self.visit(*reference)
    }
}

impl<T, V> Visit<Option<T>> for V
where
    V: Visit<T>,
{
    type Output = Option<<V as Visit<T>>::Output>;

    #[crate::instrument(name = "option", skip(self, option))]
    fn visit(&mut self, option: &Option<T>) -> Self::Output {
        option.as_ref().map(|val| self.visit(val))
    }
}

impl<T, V> Visit<Vec<T>> for V
where
    V: Visit<T>,
{
    type Output = Vec<<V as Visit<T>>::Output>;

    #[crate::instrument(name = "vec", skip(self, vec))]
    fn visit(&mut self, vec: &Vec<T>) -> Self::Output {
        vec.iter().map(|elem| self.visit(elem)).collect()
    }
}
