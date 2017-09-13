profile :
		 stack build --profile && stack exec -- json-unify-exe +RTS -hm && hp2pretty json-unify-exe.hp
