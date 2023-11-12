# Relation Explorer

## (Ideas for) features
- [x] Show a relation editor that allows user to select
    - [x] A number n from 1 to 10
    - [x] A rectangular matrix with checkboxes that allows specifying arbitraty relation on a set with n elements
    - [x] Basic history support - ability to undo relation edits
    - [x] Show a graph (as in graph theory) of the relation

- [ ] Multiple ways to initialize relation
    - [x] Initialize as empty
    - [x] Resize existing relation without changing its elements
    - [ ] Generate random relation
        - [x] any
        - [x] partial function
            - [x] function
                - [x] bijective
                    - [ ] involution
                    - [ ] derangement
        - [ ] random relation that is any of the well defined types (equivalence, function, reflexive, lattice etc.)

- [ ] Show whether the specified relation satisfies well-known properties:
    - [x] reflexivity
    - [x] irreflexivity
    - [x] symmetry
    - [x] antisymmetry
    - [x] transitivity
    - [x] asymmetric (<=> antisymmetric and irreflexive)
    - [x] connected
    - [x] acyclic
    - [x] partial function
        - [x] total function
            - [ ] surjective
            - [ ] injective
            - [x] bijective
                - [x] derangement
                - [x] involution

- [ ] Add an explanation for each property, showing why given relation satisfied given property (or not). Highlight relevant elements in explanation
    - [x] reflexive
    - [x] irreflexive
    - [x] symmetric
    - [x] antisymmetric
    - [x] asymmetric
    - [x] transitive
    - [x] connected
    - [x] acyclic
    - [x] partial function
    - [x] function
    - [ ] bijection
    - [ ] derangement
    - [ ] involution

- [ ] Given the properties that it satisfies show whether it is one of well know structures:
    - [ ] Equivalence relation
        - [ ] show corresponding partition
    - [ ] Preorder
        - [ ] show poset arising from identifying equivalent elements
    - [ ] Poset
        - [ ] show hasse diagram
        - [ ] show [covering relation](https://en.wikipedia.org/wiki/Covering_relation)
        - [ ] width (size of the largest antichain) - https://en.wikipedia.org/wiki/Dilworth%27s_theorem
        - [ ] height (size of the longest chain) - https://en.wikipedia.org/wiki/Mirsky%27s_theorem
        - [ ] list antichains (they correspond to lower / upper sets)
    - [ ] Total order
        - [ ] highlight incomparable elements if not
    - [ ] Lattice
        - [ ] Show table of meet and join operations (if not full lattice, show why it fails to be, e.g. 2 elements don't have meet / have multiple minimal elements in upper set)
        - [ ] show infimum and supremum-irreducible elements
        - [ ] show equivalence classes
    - [ ] Function
        - [ ] Show fixed points

- [ ] For each of the above shows the total number of structures of this kind (with links to OEIS, see [counting transitive relations](https://en.wikipedia.org/wiki/Transitive_relation#Counting_transitive_relations))

- [ ] Provides operations like
    - [x] reflexive closure
    - [x] reflexive reduction
    - [x] symmetric closure
    - [x] transitive closure
    - [x] transitive reduction (only on acyclic graphs)
    - [ ] equivalence closure
    - [x] composition (with itself)
    - [x] converse
    - [x] complement
    - [x] strongly connected components (of relation viewed as a graph)
    - [x] topological sort (only on acyclic graphs)
    - [ ] calculate concept lattice (Binary relations have been described through their induced concept lattices)

- [ ] mention properties of various operations (E.g. The union of two transitive relations need not be transitive, but intersection always is; converse and complement preserve symmetry etc.)
- [ ] use inclusion-exclusion principle to show exact numbers of relations within each cocnept of concept lattice or relations

Various interesting facts that could be shown in UI:
- [ ] We can reconstruct the order relation from the lattice operation infimum and supremum by x ≤ y <=> x = x ⋀ y <=> x ⋁ y = y
- Number of labeled and unlabeled lattices: https://oeis.org/A055512 and https://www.researchgate.net/publication/225384356_Counting_Finite_Lattices
- Show various powers of the relation (what happens if we keep composing relation with itself?)
- Identify (lattice) antichain within given lattice
- Implement a function that (only for posets) gives cover relation
- There should be bunch of functions like toPoset : Rel -> Maybe Poset, toLattice : Rel -> Maybe Lattice etc., in the spirit of "parse, don't validate" and then there should be operations that are valid e.g. only on lattices
- Implement a way to generate all posets (find paper?)
- Implement a way to generate all lattices
- get better understanding of antichains in posets: https://en.wikipedia.org/wiki/Antichain

