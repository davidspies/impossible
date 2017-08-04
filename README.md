# impossible

Based on "Seemingly Impossible Functional Programs" by Martin Escardo
(http://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/)
Note that "search" and "find" are reversed from the definitions in the
article.

The modulus-of-continuity function (modulus) used here is significantly faster
for all inputs both which use large indices and also which are more
computationally expensive.

Additionally there's a "relevant" function which lists (in arbitrary order) all
indices that have any impact (toggling them can potentially change the output)
on a particular function
