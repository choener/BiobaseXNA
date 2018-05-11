0.11.0.0
--------

0.10.0.0
--------

- redesigned Biobase.Secondary.Basepair

0.9.3.1
-------

- removed upper version bounds, bumped dependent versions
- added d1Distance for fast basepair distance calculations

0.9.3.0
-------

- bigger version bump together with multiple ghc compatibility

0.9.2.1
-------

- stack.yaml, some version bumping

0.9.2.0
-------

- fixed overlapping instances in AA

0.9.1.0
-------

- introduction of pattern synonyms for letters in molecular alphabets
- travis-ci integration

0.9.0.0
-------

- major cleanup of the XNA library: explicit encoding of RNA,DNA, or XNA (XNA
  contains both U and T)
- IUPAC notation (degenerate nucleotides) has an efficient encoding as well
- translation between XNA and protein (Biobase.Primary.Trans)
- import Biobase.Primary to get everything under Primary.*
- import Biobase.Secondary to get everything under Secondary.*
- SubOptDistance now extends all structure lines of RNAsubopt with a third
  field, the distance
- Diagrams provide methods to validate folding and cofolding secondary
  structure strings.
- serialization capabilities for 'Letter's

0.8.3.0
-------

- bugfix version: use vector-th-unbox to generate unboxed vector instances

0.8.2.0
-------

- dotBracket -> unsafeDotBracket
- new 'dotBracket' function works in the error monad

0.8.1.1
-------

- added T/U conversion functions

0.8.1.0
-------

- Biobase.Primary.IUPAC for degenerate base symbol conversion

0.8.0.0
-------

- Biobase.Codon -> Biobase.AAseq
- and efficient encoding of AAseqs

0.7
---

- updated to PrimitiveArray >= 0.5
- added Codon table

0.6.2.0
-------

- Updated to PrimitiveArray >= 0.2.0.0
