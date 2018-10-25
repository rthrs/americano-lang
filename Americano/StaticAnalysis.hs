module StaticAnalysis where

-- TODO

type Foo = Err String

failure :: Show a => a -> Foo
failure x = Bad $ "Undefined case: " ++ show x

denoteTypeB :: TypeB -> Foo
denoteTypeB x = case x of
  TComplex typec -> failure x
  TPrimitive typep -> failure x
  Fun typeb typebs -> failure x

denoteTypeP :: TypeP -> Foo
denoteTypeP x = case x of
  TInt -> failure x
  TString -> failure x
  TBool -> failure x
  TVoid -> failure x

denoteTypeC :: TypeC -> Foo
denoteTypeC x = case x of
  TArray typeb -> failure x
  TDict typedv typedk -> failure x

denoteTypeDK :: TypeDK -> Foo
denoteTypeDK x = case x of
  TDictKey typep -> failure x

denoteTypeDV :: TypeDV -> Foo
denoteTypeDV x = case x of
  TDictVal typeb -> failure x

checkTypes = Nothing
