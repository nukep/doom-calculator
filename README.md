# Doom Calculator

## What is this?

This project builds a Doom WAD that implements an adding machine! It is written in Clojure.

I go into depth in my blog post: https://blog.otterstack.com/posts/202212-doom-calculator/


## How to build the WAD map

You need to install Leiningen, a project build tool for Clojure. Instructions are on the site: https://leiningen.org/

You will also need to install Java to use Clojure. If it's not already installed, one way to obtain Java is to install OpenJDK 11 or 17 from Adoptium: https://adoptium.net/

Once everything is installed, run the following in your shell:

```sh
lein run
```

The above will execute the `-main` function in [src/doomcalc/core.clj](src/doomcalc/core.clj), which will create out.wad.

Note that the map is missing BSP node and segment information, so it won't run in Vanilla Doom out of the box. If you would like to build this extra information, I recommend using a map editor such as SLADE and re-saving the level.

## Interactive REPL environment

There are many ways to use the Clojure REPL. The lowest learning-curve IMO is to use Leiningen. In your shell:

```sh
lein repl
```

This will give you immediate access to an interactive REPL environment where you can run Clojure code.

### Optional: VS Code, Calva

If you'd like to use an IDE, I use the VS Code plugin "Calva". Outside of VS Code, CIDER is good if you're an Emacs user. In this section, I will focus on Calva.

You can install Calva by searching for it in the VS Code extensions section.

Follow the steps here to connect to a Clojure project: https://calva.io/connect/
