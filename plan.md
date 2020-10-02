# Plan for Ligi' Dominadmondo

I plan on having two compilers. The first compiler is intentionally simplistic
so it can be rewritten/maintained in any language. It will only support a subset of Ligi's
features, primarily removing the more advanced inference features. The second compiler
will be modular, with the core written in the aformentioned subset. The core will
simply be a re-implementation of the first compiler, and will then compile the modules
to support the rest of Ligi's features.
