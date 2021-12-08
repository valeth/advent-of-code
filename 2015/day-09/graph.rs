use std::{
    rc::Rc,
    cell::RefCell,
    fmt::{self, Debug},
    hash::{Hash, Hasher},
};

#[derive(Default)]
pub struct Graph<T, W> {
    pub nodes: Vec<Node<T, W>>,
}

impl<T, W> Graph<T, W>
    where T: Eq + Clone,
          W: Copy
{
    pub fn add_edge(&mut self, edge: &(T, T, W)) {
        let (a, b, w) = edge;

        let mut node_a = {
            match self.nodes.iter().find(|&node| *node == *a) {
                Some(n) => n.to_owned(),
                None => {
                    let new_node = Node::new(a.clone());
                    self.nodes.push(new_node.clone());
                    new_node
                }
            }
        };

        let mut node_b = {
            match self.nodes.iter().find(|&node| *node == *b) {
                Some(n) => n.to_owned(),
                None => {
                    let new_node = Node::new(b.clone());
                    self.nodes.push(new_node.clone());
                    new_node
                }
            }
        };

        node_a.add_edge(node_b.clone(), *w);
        node_b.add_edge(node_a, *w);
    }
}

impl<T, W> Debug for Graph<T, W>
    where T: Debug,
          W: Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Graph {{")?;
        for node in &self.nodes {
            for (outgoing, weight) in &node.borrow().outgoing {
                writeln!(f, "    {:?} -- [{:?}] -- {:?}", node.borrow().data, weight, outgoing.borrow().data)?;
            }
        }
        writeln!(f, "}}")
    }
}


pub struct Node<T, W>(pub Rc<RefCell<InnerNode<T, W>>>);

impl<T, W> Node<T, W> {
    pub fn new(data: T) -> Self {
        let inner = InnerNode { outgoing: Vec::new(), data };
        Self(Rc::new(RefCell::new(inner)))
    }

    pub fn add_edge(&mut self, node: Self, weight: W) {
        self.0.borrow_mut().outgoing.push((node, weight));
    }

    pub fn borrow(&self) -> std::cell::Ref<'_, InnerNode<T, W>> {
        self.0.borrow()
    }
}

impl<T, W> Clone for Node<T, W> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T, W> Hash for Node<T, W>
    where T: Hash
{
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        let this = self.borrow();
        this.data.hash(hasher);
    }
}

impl<T, W> PartialEq<T> for Node<T, W>
    where T: PartialEq
{
    fn eq(&self, other: &T) -> bool {
        self.0.borrow().data == *other
    }
}

impl<T, W> Eq for Node<T, W>
    where T: Eq {}

impl<T, W> PartialEq for Node<T, W>
    where T: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.0.borrow().data == other.0.borrow().data
    }
}

impl<T, W> Debug for Node<T, W>
    where T: Debug, W: Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Node")
            .field("data", &self.0.borrow().data)
            .finish()
    }
}


pub struct InnerNode<T, W> {
    pub data: T,
    pub outgoing: Vec<(Node<T, W>, W)>,
}
