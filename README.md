# comfy

Some comfortable clojure(script) utils.

Fairly lightweight, consists of general-purpose functions and macros that work with the basic abstractions of clojure (data-structures, sequences, transducers). Has no dependencies.

Inspired by [medley](https://github.com/weavejester/medley).

Functions with `:no-doc` metadata are considered implementation details that are subject to change.

## Roadmap

### Alpha, subject to breaking changes!

Will stay alpha at least until clojure 1.9.0 is released, at which time 1.0.0 will be released with a promise not to break anything.

## Usage

comfy leverages spec, and requires a clojure version >= 1.9.0-alpha16
and/or a clojurescript version >= 1.9.542

Add this to your `:dependencies`

```clojure
[madstap/comfy "1.0.0-alpha4"]
```

Require like this:

```
(ns foo.core
 (:require
  [madstap.comfy :as comfy]))
```

In cljs there's no need for `:require-macros`.

## Things it has

##### `prewalk-reduce`, `prewalk-transduce`, `postwalk-reduce` and `postwalk-transduce`

Exactly what it says on the tin; reduce and transduce versions of the functions in `clojure.walk`.

##### `deep-merge` and `deep-merge-with`

Pretty self explanatory, they appear in a lot of places and now here as well.

One thing I've done differently to some other implementations is that a nested
`nil` is treated as an empty map, like with `merge` proper.

A use-case I've found is to merge attr maps in hiccup,
where there might be a `:style` key that has a nested map.

##### `group-by` as a transducing context

Sometimes when I use `group-by`, I also want to transform the values
as they're added to the vector at each key. Accepting a transducer
affords great flexibility in how to transform the items as they're added to the vector.
It's also more performant than doing something like `medley/map-vals` afterwards.

A new instance of the reducing function is created for each key,
with it's own state, if any.

```clojure
;; The use-case that made me write this is when comforming a spec.
(s/def ::foos (s/* (s/or :int int? :str string?)))

(->> [1 2 "foo" "bar" 3]
     (s/conform ::foos)
     (comfy/group-by key (map val))) ;;=> {:int [1 2 3], :str ["foo" "bar"]}
```

##### `keep` and `run!` with multiple collections arity

I found the fact that the core versions can only take one collection quite surprising,
so I made versions without that limitation. When passed multiple collections,
they behave like map.

##### `forcat`, `forv`, `for-map` and other variations on `for`

`forcat` is to `for` like `mapcat` is to `map`. Similarly `forv` => `mapv`.

`for-map` creates a map, and takes a key and a value expression as the body.

##### `defs`

It's `def`, but with destructuring. Quite handy at the repl, may be useful in actual code.

##### But wait, there's more!

Check out the [api docs](https://madstap.github.io/comfy/madstap.comfy.core.html).

## License

Copyright © 2017 Aleksander Madland Stapnes

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
