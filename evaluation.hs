data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          | TmAccept
          deriving (Eq, Show)
          

isNumericVal TmZero = True
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _ = False

isVal TmTrue = True
isVal TmFalse = True
isVal t = if isNumericVal t then True else False

eval1 t = case t of
            TmIf cond s1 s2 -> case cond of
                                 TmTrue  -> s1
                                 TmFalse -> s2
                                 _       -> let t1 = eval1 cond in case t1 of
                                                                     TmAccept -> TmAccept
                                                                     _        -> TmIf t1 s1 s2
            TmSucc t1 -> let t1' = eval1 t1 in case t1' of
                                                 TmAccept -> TmAccept
                                                 _        -> TmSucc t1'
            TmPred t1 -> case t1 of
                           TmZero    -> TmZero
                           TmSucc nv -> if isNumericVal nv then nv else let t1' = eval1 t1 
                                                                        in case t1' of
                                                                             TmAccept -> TmAccept
                                                                             _        -> TmPred t1'
                           _         -> TmAccept
            TmIsZero t1 -> case t1 of
                             TmZero    -> TmTrue
                             TmSucc nv -> case isNumericVal nv of
                                            True -> TmFalse
                             _         -> let t1' = eval1 t1 in case t1' of
                                                                  TmAccept -> TmAccept
                                                                  _        -> TmIsZero t1'
            _ -> TmAccept


eval t = let t' = eval1 t
         in 
           case t' of
             TmAccept -> t
             _ -> eval t'
             
-- testes --
ex1 = TmPred (TmSucc (TmSucc (TmPred (TmSucc TmZero))))
ex2 = TmIf (TmIsZero (TmPred (TmSucc (TmSucc TmZero)))) TmTrue TmFalse
ex3 = TmIf TmZero TmTrue TmFalse
ex4 = TmSucc (TmPred TmZero)

