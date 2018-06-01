# A PROLOG interpreter and dependencies calculator for a mini flowchart language

# Requirements

  In order for this to work you need to install both `swi-prolog` and the
  `graphviz` toolbox.

# Example

  Here is how to get the dependency graph of a program. There are some test
  cases defined at the end of the file `dot.pl`. Currently, there is only one
  test program: matrices multiplication (the size of the matrices can be changed
  by editing `dot.pl`)
 
  1. Type `swipl dot.pl`: It will load the file `dot.pl` into the PROLOG
     interpreter.
  2. Type `dot:test(1, _, X), dot:exec(X, Opes, Z), dot:dependencies(Opes,
     Deps), dot:save_graph_to('graphname', Deps).` And replace `graphname` by the
     name of the you want to give to the dependency graph. This graph will be
     saved in the file `graphname.dot`
  3. Type `dot -Tps graphname.dot -o graphname.pdf` ransform the dot file into a pdf.

Note: Do not call your graph `graph` because that is a reserved word of the graphviz
language.
