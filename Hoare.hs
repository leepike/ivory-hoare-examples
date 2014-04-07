{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Hoare where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

hoare :: Def ('[Sint32] :-> Sint32)
hoare = proc "hoare" $ \x ->
    requires (x <? 10)
  $ ensures (\result -> result <? 20)
  $ body $ do
  -- First branch should be dead code, by requires.
  ifte_ (x >=? 20)
        (ret 99)
        (do y <- local (ival x)
            for (10 :: Ix 10) $ \_ -> do v <- deref y
                                         store y (v + 1)
            ret =<< deref y)


-- The following requires ivory-backend-c to be installed.
runIt :: IO ()
runIt =
  let p = package "Hoare" (incl hoare) in
  runCompiler [p] initialOpts { stdOut = True, constFold = True }

-- Result of runIt using the backend.
{-

int32_t hoare(int32_t n_var0)
{
    REQUIRES((bool) (n_var0 < (int32_t) 10));
    if ((bool) (n_var0 >= (int32_t) 20)) {
        ENSURES((bool) ((int32_t) 99 < (int32_t) 20));
        return (int32_t) 99;
    } else {
        int32_t n_local0 = n_var0;
        int32_t* n_ref1 = &n_local0;

        for (int32_t n_ix2 = (int32_t) 0; n_ix2 <= (int32_t) 9; n_ix2++) {
            int32_t n_deref3 = *n_ref1;

            *n_ref1 = (int32_t) (n_deref3 + (int32_t) 1);
        }

        int32_t n_deref4 = *n_ref1;

        ENSURES((bool) (n_deref4 < (int32_t) 20));
        return n_deref4;
    }
}

-}
