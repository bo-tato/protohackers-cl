Solutions to the [Protohackers](https://protohackers.com/) challenges in Common Lisp. 

I'm just learning, if you want well-written and commented code by an expert lisp
hacker then read [PAIP](https://norvig.github.io/paip-lisp/), not this.

You can download all dependencies and run any challenge with guix, for example:
``` sh
guix shell -L extra-packages/ -m manifest.scm  -- sbcl --load protohackers.asd --eval "(require :protohackers)" --load challenge0.lisp 
```
