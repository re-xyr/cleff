{-# LANGUAGE AllowAmbiguousTypes #-}
module Cleff.Internal.Env
  ( Env
  , Handling
  , Handler
  , esSend
  , empty
  , read
  , adjust
  , overwriteLocal
  , overwriteGlobal
  , overwriteSelfGlobal
  , extend
  , update
  ) where

import           Cleff.Internal.Monad
import qualified Cleff.Internal.Rec   as Rec
import           Data.Any             (fromAny, pattern Any)
import qualified Data.RadixVec        as Vec
import           Prelude              hiding (read)
import           Unsafe.Coerce        (unsafeCoerce)

-- | The internal representation of effect handlers. This is just a natural transformation from the effect type
-- @e ('Eff' es)@ to the effect monad @'Eff' es@ for any effect stack @es@.
--
-- In interpreting functions (see "Cleff.Internal.Interpret"), the user-facing 'Cleff.Handler' type is transformed into
-- this type.
newtype InternalHandler e = InternalHandler (∀ es. e (Eff es) ~> Eff es)

-- | The send-site environment.
data SendSite esSend e = SendSite
  {-# UNPACK #-} !(Env esSend) -- ^ The send-site 'Env'.
  {-# UNPACK #-} !(HandlerPtr e) -- ^ The pointer to the current effect handler.

-- | The typeclass that denotes a handler scope, handling effect @e@ sent from the effect stack @esSend@ in the
-- effect stack @es@.
--
-- You should not define instances for this typeclass whatsoever.
class Handling esSend e es | esSend -> e es where
  -- @esSend@ is existential so it uniquely determines the other two variables. As handling scopes can nest, the other
  -- two variables cannot determine anything.

  -- | Obtain the send-site environment.
  sendSite :: SendSite esSend e
  sendSite = error
    "Cleff.Internal.Env.sendSite: Attempting to access the send site without a reflected value. This is perhaps \
    \because you are trying to define an instance for the 'Handling' typeclass, which you should not be doing \
    \whatsoever. If that or other shenanigans seem unlikely, please report this as a bug."

-- | Get the pointer to the current effect handler itself.
hdlPtr :: ∀ esSend e es. Handling esSend e es => HandlerPtr e
hdlPtr = let SendSite _ ptr = sendSite @esSend in ptr

-- | Get the send-site 'Env'.
esSend :: Handling esSend e es => Env esSend
esSend = let SendSite env _ = sendSite in env

-- | Newtype wrapper for instantiating the 'Handling' typeclass locally, a la the reflection trick. We do not use
-- the @reflection@ library directly so as not to expose this piece of implementation detail to the user.
newtype InstHandling esSend e es a = InstHandling (Handling esSend e es => a)

-- | Instantiate an 'Handling' typeclass, /i.e./ pass an implicit send-site environment in. This function shouldn't
-- be directly used anyhow.
instHandling :: ∀ esSend e es a. (Handling esSend e es => a) -> SendSite esSend e -> a
instHandling x = unsafeCoerce (InstHandling x :: InstHandling esSend e es a)

-- | The type of an /effect handler/, which is a function that transforms an effect @e@ from an arbitrary effect stack
-- into computations in the effect stack @es@.
type Handler e es = ∀ esSend. Handling esSend e es => e (Eff esSend) ~> Eff es

-- | Transform a 'Handler' into an 'InternalHandler' given a pointer that is going to point to the 'InternalHandler'
-- and the current 'Env'.
mkInternalHandler :: HandlerPtr e -> Env es -> Handler e es -> InternalHandler e
mkInternalHandler ptr es handle = InternalHandler \e -> Eff \ess ->
  unEff (instHandling handle (SendSite ess ptr) e) (update ess es)

-- | Create an empty 'Env' with no address allocated.
empty :: Env '[]
empty = Env Rec.empty Vec.empty

-- | Read the handler a pointer points to. \( O(1) \).
read :: ∀ e es. e :> es => Env es -> ∀ es'. e (Eff es') ~> Eff es'
read (Env stack heap) = fromAny $ Vec.lookup (unHandlerPtr (Rec.index @e stack)) heap

-- | Adjust the effect stack via an function over 'Rec'.
adjust :: ∀ es' es. (Rec es -> Rec es') -> Env es -> Env es'
adjust f = \(Env stack heap) -> Env (f stack) heap

-- | Replace the handler a pointer points to. \( O(1) \).
overwriteGlobal :: ∀ e es es'. e :> es => Env es' -> Handler e es' -> Env es -> Env es
overwriteGlobal es hdl (Env stack heap) = Env stack $
  Vec.update m (Any $ mkInternalHandler ptr es hdl) heap
  where ptr@(HandlerPtr m) = Rec.index @e stack

-- | Replace the handler a pointer points to. \( O(1) \).
overwriteSelfGlobal :: ∀ e es es' esSend. Handling esSend e es => Env es' -> Handler e es' -> Env esSend -> Env esSend
overwriteSelfGlobal es hdl (Env stack heap) = Env stack $
  Vec.update ix (Any $ mkInternalHandler ptr es hdl) heap
  where ptr@(HandlerPtr ix) = hdlPtr @esSend

-- | Replace the handler pointer of an effect in the stack. \( O(n) \).
overwriteLocal :: ∀ e es es'. e :> es => Env es' -> Handler e es' -> Env es -> Env es
overwriteLocal es hdl (Env stack heap) = Env
  (Rec.update @e ptr stack)
  (Vec.snoc heap $ Any $ mkInternalHandler ptr es hdl)
  where ptr = HandlerPtr (Vec.size heap)

-- | Add a new effect to the stack with its corresponding handler pointer. \( O(n) \).
extend :: ∀ e es es'. Env es' -> Handler e es' -> Env es -> Env (e : es)
extend es hdl (Env stack heap) = Env
  (Rec.cons ptr stack)
  (Vec.snoc heap $ Any $ mkInternalHandler ptr es hdl)
  where ptr = HandlerPtr (Vec.size heap)

-- | Use the state of LHS as a newer version for RHS. \( O(1) \).
update :: ∀ es es'. Env es' -> Env es -> Env es
update (Env _ heap) (Env stack _) = Env stack heap
