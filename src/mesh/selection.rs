use super::{traversal::{Traversal, TraversalError}, MeshSelection};


#[derive(Clone)]
/// This structure keeps track of selecting multiple mesh items at a time to enable some more-complex operations
/// to act differently based on if they are applied to one item or multiple.
pub struct Selection<'m> {
    traversal:Traversal<'m>,
    selection:MeshSelection,
}

impl<'m> From<Traversal<'m>> for Selection<'m> {
    fn from(traversal: Traversal<'m>) -> Self {
        let selection = traversal.get_position().into();
        Self { traversal, selection}
    }
}

impl<'m> Selection<'m> {
    pub fn build(self) -> Result<MeshSelection, TraversalError> {
        if self.traversal.is_error() {
            Err(self.traversal.to_error().unwrap())
        } else {
            Ok(self.selection)
        }
    }
}