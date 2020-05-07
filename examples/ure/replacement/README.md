# Rule of Replacement

URE-based implementation of the rule of replacement
https://en.wikipedia.org/wiki/Rule_of_replacement. The problem is that
due to the immutable character of the AtomSpace [atoms can be added or
removed but never modified (Values aside)] pattern matcher rewrites
only creates new subgraphs but do not replace old ones.

## Example

For instance given the KB

```
(Evaluation
  (Predicate "believe")
  (List
    (Concept "Max Tegmark")
    (Similarity
      (Concept "Universe")
      (Concept "Mathematics"))))

(Evaluation
  (Predicate "synonymous")
  (Set
    (Concept "Mathematics")
    (Concept "Consciousness")))
```

How to infer that Max Tegmark believes the universe is conscious given
that mathematics is synonymous to consciousness, supposedly?

We can certainly write an ad-hoc rule such as, semi-formally

```
(believe X (Similarity Y Z))
(synonymous Z W)
|-
(believe X (Similarity Y W))
```

but that wouldn't work if we have that Nil believes that Max Tegmark
believes that the universe is made of math, and want to infer that Nil
believes that Max Tegmark believes that the universe is conscious. For
that we need a more general rule.

Ideally we'd like to have a rule expressing that if A and B are
synonymous then any term T containing A can be rewritten to contain B
instead.

That is semi-formally

```
(synonymous A B)
T
|-
T[A/B]
```

There is an implicit recursion in this notation that we need to
unwrap. The most generic rule we have, given the current limitations
of Atomese (EDIT: since then a new atom type has been introduced for
that https://wiki.opencog.org/w/JoinLink), is

```
(synonymous A B)
(L LG A RG)
|-
(synonymous
  (L LG A RG)
  (L LG B RG))
```

where `L` is some link type and `G1` and `G2` are globs with interval
`[0, +inf)`.

Then the rule can be recursively applied over the rewrite product.

## Usage

Load `replacement.scm` in guile

```bash
guile -l replacement.scm
```

Then the results should be in the `results` variable.

## See also

https://wiki.opencog.org/w/JoinLink can also be used to perform
replacement.
