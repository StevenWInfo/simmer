# Simmer

Simmer is an embedded scripting language designed to make writing code fast and easy.

It's currently a work in progress. Some basic stuff works, but there's still a lot to be implemented and it's nowhere near being stable or straightforward to build.

---

The only language that it's currently implemented and embedded in is [Purescript](https://www.purescript.org/). It could be implemented in other "host" languages, which is why I generally refer to a "host" language rather than Purescript specifically, but there's currently no work being done to implement it into other languages.

In Simmer, there is no way to export code from one file and use it in another. Simmer is made to be both simple and good for scripting, not for creating libraries. Instead, you write the libraries that Simmer code needs in the host language like Purescript which is much better for writing libraries in.

You can use Simmer as a regular embedded language to put into tools written in the host language, but most of the focus is on using Simmer as a scripting language that happens to have its libraries written in the host langauge.
