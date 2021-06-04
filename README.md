![github action: master](https://github.com/choener/BiobaseXNA/actions/workflows/ci.yml/badge.svg?branch=master)
![github action: hackage](https://github.com/choener/SciBaseTypes/actions/workflows/hackage.yml/badge.svg)

# BiobaseXNA

Efficient encoding of (short) biological sequences. This package ist designed
to deal with *in-memory* snippets of DNA, RNA, and amino acids. The encoding is
geared toward time-efficiency, not necessarily space efficiency (we use Int's
for encoding characters, not the smallest type possible).

Additional modules provide conversion capabilities between different types of
characters according to biological laws, and some biochemical constraint
information. The latter includes canonical and non-canonical pairing
information for RNA.

Actual energy parameters for pairings are provided by other packages, for
example BiobaseTurner for the loop energy model with measured parameters.



#### Contact

Christian Hoener zu Siederdissen  
Leipzig University, Leipzig, Germany  
choener@bioinf.uni-leipzig.de  
http://www.bioinf.uni-leipzig.de/~choener/  

