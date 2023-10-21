# Relation Explorer

## (Ideas for) features
- [x] Show a relation editor that allows user to select
    - [x] A number n from (0?) 1 to 10
    - [x] A rectangular matrix with checkboxes that allows specifying arbitraty relation on a set with n elements
    - [ ] Support for undo / redo history of modifications

- [ ] Multiple ways to initialize relation
    - [x] Initialize as empty
    - [x] Resize existing relation without changing its elements
    - [ ] Generate random relation
        - [ ] any
        - [ ] random relation that is any of the well defined types

- [ ] Show whether the specified relation satisfies well-known properties:
    - [x] reflexivity
    - [x] irreflexivity
    - [x] symmetry
    - [x] antisymmetry
    - [x] transitivity
    - [x] assymetric (<=> antisymmetric and irreflexive)
    - [x] function
        - [ ] partial / total
        - [ ] surjective
        - [ ] injective
        - [ ] bijective

- [ ] When property is not satisfied add "why?" tooltip that will explain why
    - [ ] Also visually highlight missing/extra elements that are breaking the property

- [ ] Given the properties that it satisfies show wheter it is one of well know structures:
    - [ ] Equivalence relation
        - [ ] show corresponding partition
    - [ ] Preorder
        - [ ] show posset arising from identifying equivalent elements
    - [ ] Poset
        - [ ] show hasse diagram
    - [ ] Total order
        - [ ] highlight incomparable elements if not
    - [ ] Lattice
        - [ ] Show table of meet and join operations (if not full lattice, show why it fails to be, e.g. 2 elements don't have meet / have multiple minimal elements in upper set)

- [ ] For each of the above shows the total number of structures of this kind (with links to OEIS, see [counting transitive relations](https://en.wikipedia.org/wiki/Transitive_relation#Counting_transitive_relations))

- [ ] Provides operations like
    - [x] reflexive closure
    - [x] symmetric closure
    - [x] transitive closure
        - [ ] make it possible to show how transitive closure can be constructed by repeatedly composing with the original relation
    - [ ] equivalence closure
    - [ ] transitive reduction
    - [x] composition (with itself)
    - [x] converse
    - [x] complement
    - [ ] calculate concept lattice (Binary relations have been described through their induced concept lattices)

- [ ] mention properties of various operations (E.g. The union of two transitive relations need not be transitive, but intersection always is; converse and complement preserve symmetry etc.)
- [ ] use inclusion-exclusion principle to show exact numbers of relations within each cocnept of concept lattice or relations
