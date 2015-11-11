{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.TH.Path.View
    ( View(viewLens, ViewType)
    , viewInstanceType
    , viewTypes
    ) where

import Control.Lens (Lens')
import Control.Monad.States (MonadStates)
import Data.Set as Set (fromList, Set)
import Language.Haskell.TH hiding (prim)
import Language.Haskell.TH.Context (InstMap)
import Language.Haskell.TH.Desugar as DS (DsMonad)
import Language.Haskell.TH.TypeGraph.Arity (typeArity)
import Language.Haskell.TH.TypeGraph.Prelude (unlifted)

-- | If there is an instance of View for a type @a@, then when @a@
-- appears in the data, the lens returned by 'viewLens' is used to
-- compute the value of type @ViewType a@, and that value is used
-- instead.  For exmaple, a 'View' could be used to modify the value
-- obtained from a database so that it is more suitable to the user
-- interface that uses that value.  Then the @ViewType a@ value is
-- transformed back into an @a@ and stored in the database.
class View a where
    type ViewType a
    viewLens :: Lens' a (ViewType a)

-- | Determine whether there is a 'View' instance for a type and if so
-- return @ViewType a@.
viewInstanceType :: DsMonad m => Type -> m (Maybe Type)
viewInstanceType typ =
    do prim <- unlifted typ
       arity <- typeArity typ
       case arity == 0 && not prim of
         True -> do
           vInsts <- runQ $ reifyInstances ''ViewType [typ]
           case vInsts of
             [TySynInstD _ (TySynEqn [_typ] type2)] -> return $ Just type2
             [] -> return Nothing
             _ -> error $ "Unexpected view instance(s): " ++ show vInsts
         _ -> return Nothing

-- | Retrieve every View instance known to the Q monad and return the
-- union of all of their a and b types.
viewTypes :: (DsMonad m, MonadStates InstMap m) => m (Set Type)
viewTypes = do
  FamilyI _ tySynInsts <- runQ $ reify ''ViewType
  return $ Set.fromList $ concatMap (\ (TySynInstD _vt (TySynEqn [a] b)) -> [a, b]) tySynInsts
