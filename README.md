# Compiling This Project

To compile, you will need a curent installation of Haskell Platform,
which can be obtained from here: https://www.haskell.org/platform/

Then, you can build this project by entering the following commands:

```shell
# cabal sandbox init
# cabal install
```

The resulting executable will be palced at
`.cabal-sandbox/bin/decision-tree-classifier`. 

# Running this Project

After you build `decision-tree-classifier`, you can run it as follows,
from the root of the source directory. To perform a k-fold validation,
run the following commmand. 

```shell
# .cabal-sandbox/bin/decision-tree-classifier -validate
```

To output the internal representation of the graph in DOT (GraphViz)
format, use the following command:

```shell
# .cabal-sandbox/bin/decision-tree-classifier -show
```

