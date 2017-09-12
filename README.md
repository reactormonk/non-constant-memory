To reproduce:

```
stack build --profile
stack exec -- json-unify-exe +RTS -hm
```

Result:

thread blocked indefinitely in an STM transaction
