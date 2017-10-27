# comfy

Some comfortable clojure(script) utils with no dependencies.

Part general-purpose functions and macros, part syntax sugar.

## Usage

Requires a clojure version >= 1.9.0-alpha16
and/or a clojurescript version >= 1.9.542

Add this to your `:dependencies`

```clojure
[madstap/comfy "1.0.4"]
```

Require like this:

```clojure
(ns foo.core
 (:require
  [madstap.comfy :as comfy]))
```

In cljs `:require-macros` is not needed.

Starting at version one-point-oh, I won't change the meaning of stuff,
preferring to introduce [new names and deprecating (but not deleting) old ones](https://www.youtube.com/watch?v=oyLBGkS5ICk).
Accretion, not breakage.

There are a number functions and macros that are modified versions of the
`clojure.core` ones of the same name. They only add capabilities to the
originals while maintaining compatibility, so it's safe to do the following.

```clojure
(ns foo.core
 (:refer-clojure :exclude [for group-by keep run!])
 (:require
  [madstap.comfy :refer [for group-by keep run!]))
```

(At least as of the 1.9.0 alphas. I can imagine future changes to `clojure.core`
breaking this assumption, but consider it unlikely.)

Functions with `:no-doc` metadata are considered implementation
details, and subject to change, as is anything in the
`madstap.comfy.alpha` namespace. If you find yourself using anything from
either, open an issue and I'll consider making it permanent.

## Things it has

#### `prewalk-reduce`, `prewalk-transduce`, `postwalk-reduce` and `postwalk-transduce`

Exactly what it says on the tin; reduce and transduce versions of the functions in `clojure.walk`.

#### `deep-merge` and `deep-merge-with`

Pretty self explanatory, they appear in a lot of places and now here as well.

One thing I've done differently to some other implementations is that a nested
`nil` is treated as an empty map, like with `merge` proper.

A use-case I've found is to merge attr maps in hiccup,
where there might be a `:style` key that has a nested map.

#### `group-by` as a transducing context

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

It can also take a reducing function and an (optional) init value, in which case
it acts like `transduce`.

```clojure
(comfy/group-by odd? (map char) str (range 97 (inc 122)))
;;=> {true "acegikmoqsuwy", false "bdfhjlnprtvxz"}
```

#### `keep` and `run!` with multiple collections arity

I found the fact that the core versions can only take one collection quite surprising,
so I made versions without that limitation. When passed multiple collections,
they behave like map.

#### `for`, `forv`, `forcat`, `forcatv` and `for-map` with additional modifiers

`forv` and `forcat` is to `for` as `mapv` and `mapcat` is to map.

`for-map` builds a map and takes two expressions in the body,
for each key and value in the resulting map.

They can all take the additional modifiers `:do`, `:when-not`, `:when-let`, `:while-not` and`:while-let`.

* `:do` performs an arbitrary side effect, mostly for debugging purposes.
* `:when-not` and `:while-not` are the opposites of `:when` and `:while`.
* `[,,, :when-let [[x y] (foo)]]` translates to
`[:let [temp (foo)] :when temp :let [[x y] temp]]`, ditto for `:while-let`.


```clojure
(comfy/forv [x (range 3)]
  (inc x))
;;=> [1 2 3]

(comfy/for-map [x (range 3)]
  (str x) x)
;;=> {"0" 0, "1" 1, "2" 2}

(comfy/forcat [x (range 4)]
  (range x))
;;=> (0 0 1 0 1 2)
```

#### `defs`

It's `def`, but with destructuring.

```clojure
(comfy/defs {ring-ajax-post                   :ajax-post-fn
             ring-ajax-get-or-ws-handshake    :ajax-get-or-ws-handshake-fn
             receive                          :ch-recv ; ChannelSocket's receive channel
             ^{:arglists '([id event])} send! :send-fn ; ChannelSocket's send API fn
             connected-uids                   :connected-uids} ; Watchable, read-only atom
  (sente/make-channel-socket! sente-web-server-adapter {}))
```

#### `str->int` and `str->dec`

Easy and portable parsing of integers and decimals. Return `nil` when given a
string with the wrong format.

#### `conj-some`, `assoc-some` and `assoc-in-some`

Like their namesakes in `clojure.core`, but ignore nil values.

#### `append` and `append-some`

Append elements to a sequence.

Unlike the other sequence functions the sequence is the first argument.
That's because they operate on the sequence as a whole, and not on each individual element.

In the vast majority of cases you should prefer using a vector and `conj`,
but when writing macros you often want to append stuff to code (lists).
(In this case lists and sequences are interchangeable.)

```clojure
(s/def ::foo-args
  (s/cat :name symbol?, :docstring (s/? string?), :expr any?))

(defmacro foo [& forms]
  (let [{:keys [name docstring expr]} (s/conform ::foo-args forms)]
    (comfy/append-some `(def) name docstring expr)))
```

#### But wait, there's more!

Check out the [api docs](https://madstap.github.io/comfy/madstap.comfy.html).


## Rationale

There already exists a bunch of these util libraries for clojure.

Here's an incomplete list of them:

* [detritus](https://github.com/arrdem/detritus)
* [encore](https://github.com/ptaoussanis/encore)
* [medley](https://github.com/weavejester/medley)
* [plumbing](https://github.com/plumatic/plumbing)
* [suchwow](https://github.com/marick/suchwow)
* [useful](https://github.com/flatland/useful)
* [tupelo](https://github.com/cloojure/tupelo)

So why another one? Well, mostly convenience. This one is mine,
I can add whatever I need to it and cut releases whenever I feel like it.

I can choose not to break anything, unlike detritus which says in it's readme
"breaking changes will be frequent". Which is totally fine as long as it's
honest about it, but I want something that I can just mindlessly
upgrade without thinking.

Also, importantly, no deps. Of the above libraries, I think only medley
and detritus have no dependencies.

I tend to add this lib and [medley](https://github.com/weavejester/medley)
straight away to any new projects. One of them always seem to come in handy.
medley has both the qualities that I described above;
it probably won't ever break anything and has no dependencies.

I have re-implemented or independently invented a lot of stuff that already
exists in other libs, but won't add anything that's already in medley.


## License

Copyright © 2017 Aleksander Madland Stapnes

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
