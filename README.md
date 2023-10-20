# Relation Explorer

## (Ideas for) features
- [x] Show a relation editor that allows user to select
    - [x] A number n from (0?) 1 to 10
    - [x] A rectangular matrix with checkboxes that allows specifying arbitraty relation on a set with n elements

- [ ] Show whether the specified relation satisfies well-known properties:
    - [x] reflexivity
    - [x] symmetry
    - [x] antisymmetry
    - [x] transitivity
    - [ ] functional(ity?)
        - [ ] partial / total
        - [ ] surjective
        - [ ] injective
        - [ ] bijective
    - [ ] ...
- [ ] When property is not satisfied add "why?" tooltip that will explain why
    - [ ] Also visually highlight missing/extra elements that are breaking the property

- [ ] Given the properties that it satisfies show wheter it is one of well know structures:
    - [ ] Equivalence relation
        - [ ] show corresponding partition
    - [ ] Preorder
    - [ ] Poset
        - [ ] show hasse diagram
    - [ ] Total order
    - [ ] Lattice
    - [ ] ...

- [ ] For each of the above shows the total number of structures of this kind (with links to OEIS, see [counting transitive relations](https://en.wikipedia.org/wiki/Transitive_relation#Counting_transitive_relations))

- [ ] Provides operations like
   - [ ] reflexive closure
   - [ ] transitive closure
   - [ ] equivalence closure
   - [ ] transitive reduction
   - [ ] reflexive closure
   - [ ] composition (with itself)
   - [ ] converse
   - [ ] complement
   - [ ] calculate concept lattice (Binary relations have been described through their induced concept lattices)

