{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}

{- | Implements core data types and combinators for logging actions.
-}

module Colog.Core.Action
       ( -- * Core type and instances
         LogAction (..)
        , (<&)
        , (&>)

         -- * 'Semigroup' combinators
       , foldActions

         -- * Contravariant combinators
       , cfilter
       , cmap
       , (>$<)
       , (Colog.Core.Action.>$)
       , cmapM

         -- * Divisible combinators
       , divide
       , conquer
       , (>*<)
       , (>*)
       , (*<)

         -- * Decidable combinators
       , lose
       , choose
       , (>|<)

         -- * Comonadic combinators
       , extract
       , extend
       , (=>>)
       , (<<=)
       ) where

import Control.Monad (when, (>=>))
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..), stimesMonoid)
import Data.Void (Void, absurd)

#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant (Contravariant (..))
#endif

----------------------------------------------------------------------------
-- Core data type with instances
----------------------------------------------------------------------------

{- | Polymorphic and very general logging action type.

* @__msg__@ type variables is an input for logger. It can be 'Text' or custom
logging messsage with different fields that you want to format in future.

* @__m__@ type variable is for monadic action inside which logging is happening. It
can be either 'IO' or some custom pure monad.

Key design point here is that 'LogAction' is:

* 'Semigroup'
* Contravariant
* Comonad
-}
newtype LogAction m msg = LogAction
    { unLogAction :: msg -> m ()
    }

{- | This instance allows you to join multiple logging actions into single one.

For example, if you have two actions like these:

@
logToStdout :: 'LogAction' IO String  -- outputs String to terminal
logToFile   :: 'LogAction' IO String  -- appends String to some file
@

You can create new 'LogAction' that perform both actions one after another using 'Semigroup':

@
logToBoth :: 'LogAction' IO String  -- outputs String to both terminal and some file
logToBoth = logToStdout <> logToFile
@
-}
instance Applicative m => Semigroup (LogAction m a) where
    (<>) :: LogAction m a -> LogAction m a -> LogAction m a
    LogAction action1 <> LogAction action2 = LogAction $ \a -> action1 a *> action2 a
    {-# INLINE (<>) #-}

    sconcat :: NonEmpty (LogAction m a) -> LogAction m a
    sconcat = foldActions
    {-# INLINE sconcat #-}

    stimes :: Integral b => b -> LogAction m a -> LogAction m a
    stimes = stimesMonoid
    {-# INLINE stimes #-}

instance Applicative m => Monoid (LogAction m a) where
    mappend :: LogAction m a -> LogAction m a -> LogAction m a
    mappend = (<>)
    {-# INLINE mappend #-}

    mempty :: LogAction m a
    mempty = LogAction $ \_ -> pure ()
    {-# INLINE mempty #-}

    mconcat :: [LogAction m a] -> LogAction m a
    mconcat = foldActions
    {-# INLINE mconcat #-}


{- | Operator version of 'unLogAction'

Note that because of the types, something like:
> action <& msg1 <& msg2
doesn't make sense. Instead you want:
> action <& msg1 >> action <& msg2
-}
infix 5 <&
(<&) :: LogAction m msg -> msg -> m ()
(<&) = unLogAction
{-# INLINE (<&) #-}

-- | A flipped version of '<&'
infix 5 &>
(&>) :: msg -> LogAction m msg -> m ()
(&>) = flip unLogAction
{-# INLINE (&>) #-}

----------------------------------------------------------------------------
-- Combinators
----------------------------------------------------------------------------

{- | Joins some 'Foldable' of 'LogAction's into single 'LogAction' using
'Semigroup' instance for 'LogAction'. This is basically specialized version of
'Data.Foldable.fold' function.
-}
foldActions :: (Foldable t, Applicative m) => t (LogAction m a) -> LogAction m a
foldActions actions = LogAction $ \a -> for_ actions $ \(LogAction action) -> action a
{-# INLINE foldActions #-}
{-# SPECIALIZE foldActions :: Applicative m => [LogAction m a]          -> LogAction m a #-}
{-# SPECIALIZE foldActions :: Applicative m => NonEmpty (LogAction m a) -> LogAction m a #-}

{- | Takes predicate and performs given logging action only if predicate returns
'True' on input logging message.
-}
cfilter :: Applicative m => (msg -> Bool) -> LogAction m msg -> LogAction m msg
cfilter predicate (LogAction action) = LogAction $ \a -> when (predicate a) (action a)
{-# INLINE cfilter #-}

{- | This combinator is @contramap@ from contravariant functor. It is useful
when you have something like

@
__data__ LogRecord = LR
    { lrName    :: LoggerName
    , lrMessage :: Text
    }
@

and you need to provide 'LogAction' which consumes @LogRecord@

@
logRecordAction :: 'LogAction' m LogRecord
@

when you only have action that consumes 'Text'

@
logTextAction :: 'LogAction' m Text
@

With 'cmap' you can do the following:

@
logRecordAction :: 'LogAction' m LogRecord
logRecordAction = 'cmap' lrMesssage logTextAction
@

This action will print only @lrMessage@ from @LogRecord@. But if you have
formatting function like this:

@
formatLogRecord :: LogRecord -> Text
@

you can apply it instead of @lrMessage@ to log formatted @LogRecord@ as 'Text'.
-}
cmap :: (a -> b) -> LogAction m b -> LogAction m a
cmap f (LogAction action) = LogAction (action . f)
{-# INLINE cmap #-}

-- | Operator version of 'cmap'.
infixr 3 >$<
(>$<) :: (a -> b) -> LogAction m b -> LogAction m a
(>$<) = cmap
{-# INLINE (>$<) #-}

{- | This combinator is @>$@ from contravariant functor. Replaces all locations
in the output with the same value. The default definition is
@contramap . const@, so this is a more efficient version.
-}
infixl 4 >$
(>$) :: b -> LogAction m b -> LogAction m a
(>$) b (LogAction action) = LogAction (\_ -> action b)

{- | 'cmapM' combinator is similar to 'cmap' but allows to call monadic
functions (functions that require extra context) to extend consumed value.
Consider the following example.

You have this logging record:

@
__data__ LogRecord = LR
    { lrTime    :: UTCTime
    , lrMessage :: Text
    }
@

and you also have logging consumer inside 'IO' for such record:

@
logRecordAction :: 'LogAction' IO LogRecord
@

But you need to return consumer only for 'Text' messages:

@
logTextAction :: 'LogAction' IO Text
@

If you have function that can extend 'Text' to @LogRecord@ like the function
below:

@
withTime :: 'Text' -> 'IO' LogRecord
withTime msg = __do__
    time <- getCurrentTime
    pure (LR time msg)
@

you can achieve desired behavior with 'cmapM' in the following way:

@
logTextAction :: 'LogAction' IO Text
logTextAction = 'cmapM' withTime myAction
@
-}
cmapM :: Monad m => (a -> m b) -> LogAction m b -> LogAction m a
cmapM f (LogAction action) = LogAction (f >=> action)
{-# INLINE cmapM #-}

-- | @divide@ combinator from @Divisible@ type class.
divide :: (Applicative m) => (a -> (b, c)) -> LogAction m b -> LogAction m c -> LogAction m a
divide f (LogAction actionB) (LogAction actionC) = LogAction $ \(f -> (b, c)) ->
    actionB b *> actionC c

-- | @conquer@ combinator from @Divisible@ type class.
conquer :: Applicative m => LogAction m a
conquer = LogAction $ const (pure ())


-- | Operator version of @'divide' 'id'@.
infixr 4 >*<
(>*<) :: (Applicative m) => LogAction m a -> LogAction m b -> LogAction m (a, b)
(LogAction actionA) >*< (LogAction actionB) = LogAction $ \(a, b) ->
    actionA a *> actionB b
{-# INLINE (>*<) #-}

infixr 4 >*
(>*) :: Applicative m => LogAction m a -> LogAction m () -> LogAction m a
(LogAction actionA) >* (LogAction actionB) = LogAction $ \a ->
    actionA a *> actionB ()
{-# INLINE (>*) #-}

infixr 4 *<
(*<) :: Applicative m => LogAction m () -> LogAction m a -> LogAction m a
(LogAction actionA) *< (LogAction actionB) = LogAction $ \a ->
    actionA () *> actionB a
{-# INLINE (*<) #-}

-- | @lose@ combinator from @Decidable@ type class.
lose :: (a -> Void) -> LogAction m a
lose f = LogAction (absurd . f)

-- | @choose@ combinator from @Decidable@ type class.
choose :: (a -> Either b c) -> LogAction m b -> LogAction m c -> LogAction m a
choose f (LogAction actionB) (LogAction actionC) = LogAction (either actionB actionC . f)

-- | Operator version of @'choose' 'id'@.
infixr 3 >|<
(>|<) :: LogAction m a -> LogAction m b -> LogAction m (Either a b)
(LogAction actionA) >|< (LogAction actionB) = LogAction (either actionA actionB)
{-# INLINE (>|<) #-}

{- | If @msg@ is 'Monoid' then 'extract' performs given log action by passing
'mempty' to it.
-}
extract :: Monoid msg => LogAction m msg -> m ()
extract action = unLogAction action mempty

-- TODO: write better motivation for comonads
{- | This is a /comonadic extend/. It allows you to chain different transformations on messages.

>>> logToStdout = LogAction putStrLn
>>> f (LogAction l) = l ".f1" *> l ".f2"
>>> g (LogAction l) = l ".g"
>>> unLogAction logToStdout "foo"
foo
>>> unLogAction (extend f logToStdout) "foo"
foo.f1
foo.f2
>>> unLogAction (extend g $ extend f logToStdout) "foo"
foo.g.f1
foo.g.f2
>>> unLogAction (logToStdout =>> f =>> g) "foo"
foo.g.f1
foo.g.f2

-}
extend :: Semigroup msg => (LogAction m msg -> m ()) -> LogAction m msg -> LogAction m msg
extend f (LogAction action) = LogAction $ \m -> f $ LogAction $ \m' -> action (m <> m')

-- | 'extend' with the arguments swapped. Dual to '>>=' for a 'Monad'.
infixl 1 =>>
(=>>) :: Semigroup msg => LogAction m msg -> (LogAction m msg -> m ()) -> LogAction m msg
(=>>) = flip extend
{-# INLINE (=>>) #-}

-- | 'extend' in operator form.
infixr 1 <<=
(<<=) :: Semigroup msg => (LogAction m msg -> m ()) -> LogAction m msg -> LogAction m msg
(<<=) = extend
{-# INLINE (<<=) #-}


#if MIN_VERSION_base(4,12,0)
instance Contravariant (LogAction m) where
    contramap = cmap
    (>$)      = (Colog.Core.Action.>$)

#endif
