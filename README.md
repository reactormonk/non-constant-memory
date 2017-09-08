To reproduce:

```
stack build --profile
stack exec -- json-unify-exe +RTS -hm
```

Approximate result:

![graph](json-unify-exe.svg)

For longer runs, just copy the folders in out a few more times.
