To reproduce:

```
stack build --profile
stack exec -- json-unify-exe +RTS -hm
```

Result:

![graph](json-unify-exe.svg)

Shouldn't load all of the 40MB into memory (?)
