---
title: MTL Motivations
date: 2019-08-17
authors: bkolera
---

# Introduction

When you are starting out with functional programming, it is very easy to find things like
[EventWriterT](http://hackage.haskell.org/package/reflex-0.6.1/docs/Reflex-EventWriter-Base.html#t:EventWriterT) 
and [EventWriter](http://hackage.haskell.org/package/reflex-0.6.1/docs/Reflex-EventWriter-Class.html#t:EventWriter)
to be very confusing. What even are they? And what problems motivate ending up with things like this? If you have 
heard of monad-transformers and mtl but don't quite understand what they are or what they are for, this article 
may be helpful to you! 

Note: To get the most of this artcle, it is best if you already understand basic haskell syntax, typeclasses and Functor/Applicative/Monad. 

# Nested Monads aren't very fun

When writing monadic code, we can get ourselves into awkward circumstances where we have monads inside monads. Take for instance this code:

```haskell
data OurErrors = OutputFileAlreadyExists | ParseFailure deriving (Eq, Show)

parseFile :: Text -> Either OurErrors Thingo
saveThingo :: Thingo -> IO (Either OurErrors ())
-- Code omitted because it doesn't matter

-- Using these things gets pretty awkward and yucky if you have a lot of steps that intermingle eithers 
-- and IO.

main = do
  fileText <- T.readFile "filepath.txt"
  let thingoEither = parseFile fileText
  saveRes <- either (pure . Left) saveThingo thingoEither
  case saveRes of
    Left e -> putStrLn $ "There was an error: " <> show e
    Right _ -> putStrLn "OK"
```

The awkwardness here is that we are sequencing IO actions, but also need to sequence the either computations as well (i.e. we can only progress with the program if the either is Right, otherwise things can only stay a failure. 

# Transformers to the rescue

What we really want is a single layer of structure that is a monad that has both Either and IO functionality. The transfomers package has something that is exactly that. But lets look at it in action before we look at the details:

```haskell
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, except) 

data OurErrors = OutputFileAlreadyExists | ParseFailure deriving (Eq, Show)

parseFile :: Text -> Either OurErrors Thingo
saveThingo :: Thingo -> IO (Either OurErrors ())
-- Code omitted because it doesn't matter

program :: ExceptT OurErrors IO ()
program = do
  -- lift Takes an IO a action and makes it ExceptT Error IO a
  fileText <- lift $ T.readFile "filepath.txt"
  -- except Takes an Either Error a and turns it into an ExceptT Error a
  thingoEither <- except $ parseFile fileText
  -- If we have something that is already IO (Either e a) just use ExceptT to turn it 
  -- into an ExceptT e IO a
  ExceptT $ saveThingo thingoEither
  
-- Note than in program, we don't have to care about the inner either anymore. 
-- It's just a flat monad that we only bind/pull apart once.
  
main = do
  -- We use runExceptT to turn it back into our two layered thing to finally 
  -- run what we need
  saveRes <- (runExceptT program :: IO (Either Error ())
  case saveRes of
    Left e -> putStrLn $ "There was an error: " <> show e
    Right _ -> putStrLn "OK"
```

How does this work? Lets check out the types of ExceptT. It's literally just a newtype of `(m (Either e a))`. See [Control.Monad.Trans.Except](http://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Except.html#t:ExceptT).

And it's instances for Functor, Applicative and Monad just deal with the special either behaviour of the inner either and then use the instance of the underlying monad. 

```haskell
instance (Functor m) => Functor (ExceptT e m) where
    fmap f = ExceptT . fmap (fmap f) . runExceptT

instance (Functor m, Monad m) => Applicative (ExceptT e m) where
    pure a = ExceptT $ return (Right a)
    
    ExceptT f <*> ExceptT v = ExceptT $ do
        mf <- f
        case mf of
            Left e -> return (Left e)
            Right k -> do
                mv <- v
                case mv of
                    Left e -> return (Left e)
                    Right x -> return (Right (k x))
                    
    m *> k = m >>= \_ -> k

instance (Monad m) => Monad (ExceptT e m) where
    m >>= k = ExceptT $ do
        a <- runExceptT m
        case a of
            Left e -> return (Left e)
            Right x -> runExceptT (k x)
    {-# INLINE (>>=) #-}
```

This is doing what we were having to juggle with the inner either, but baked into the Functor/Applicative/Monad instances because we are using the newtype ExceptT.
ExceptT is called a "monad transformer" as it can stack an either like behaviour onto any monad (in our case, we are stacking the either onto IO). It is outside the
topic of this article, but it's impossible to make a generic transformer that will combine the behaviours of any two monads
(aside: [it is possible for applicative](http://hackage.haskell.org/package/applicative-extras-0.1.8/docs/Control-Applicative-Compose.html)),
so we have to make a different tranformer for every kind of behaviour that we want to stack on. There are a bunch of these in [hackage.haskell.org/package/transformers](the transformers package).

# WriterT

WriterT gives us a way to collect data during our program that we can't access in our program 
but we can have it once we've run our program. It's kind of like a logger in you can emit things
to the set of things that have been emitted as your program runs, but it is not like a logger 
in that there is no logging to a console or any other side effect during your program. It's more like 
an append-only collection that you can access once everything is done than it is a logging framework. 

Lets change our program so that we log out each step. This is a super contrived example and you probably shouldn't use Writer this way, but it shows Writer and also stacking the transformers deeper.

```haskell
program :: WriterT [Text] (ExceptT OurErrors IO) ()
program = do
  fileText <- lift . lift $ T.readFile "filepath.txt"
  tell ["Loaded file: filePath.txt"]
  thingoEither <- lift . except $ parseFile fileText
  tell ["Parsed file"] -- This wont get run if the parsing failed!
  lift . ExceptT $ saveThingo thingoEither
  
main = do
  (saveRes, logs) <- (runWriterT (runExceptT program) :: IO (Either OurErrors (), [Text]))
  putStrLn "Program Logs:"
  for_ T.putStrLn logs
  case saveRes of
    Left e -> putStrLn $ "There was an error: " <> show e
    Right _ -> putStrLn "OK"
```

Notice the extra lifts that we had to put in there. Lift is actually from a typeclass and seems a bit magical
because the people that wrote transformers have written a MonadTrans instance for every transformer already.
Think of it as a way to lift up a monad on the layer below up to the higher one. Kinda like pure,
but for one layer of transformer below. 
It is defined in [Control.Monad.Trans.Class](http://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Class.html) if you are curious.

# MTL

When programming with concrete transformers like ExceptT, everything has to agree on the same concrete transformer stack 
and we have to care deeply about the order that we stack them on top of each other. `WriterT [Text] (ExceptT Error IO) a`
is very different to `ExceptT Error (WriterT [Text] IO) a`! Programming with concrete transformer types means that every
part of your program has to change when the transformers change (just see the extra lifts that we had to add with Writer) 
even if parts of your program only care about parts of the transformer stack. This is very non-modular and we can do better! :)

This is where mtl comes into play. It allows us to write our programs like this:

```haskell
data OurErrors = OutputFileAlreadyExists | ParseFailure deriving (Eq, Show)

-- Note MonadIO is actually from base, not MTL.
readFile :: MonadIO m => m Text
parseFile :: MonadError e m => Text -> m Thingo
saveThingo :: (MonadIO m, MonadError e m) => Thingo -> m ()

program :: (MonadWriter [Text] m, MonadIO m, MonadError OurErrors m) => m ()
-- This will still typecheck too:
-- program :: WriterT [Text] (ExceptT Error IO) ()
program = do
  fileText <- readFile
  tell ["Loaded file: filePath.txt"]
  thingoEither <- parseFile fileText
  tell ["Parsed file"] -- This wont get run if the parsing failed!
  saveThingo thingoEither
  
main = do
  (saveRes, logs) <- (runWriterT (runExceptT program) :: IO (Either OurErrors (), [Text]))
  putStrLn "Program Logs:"
  for_ T.putStrLn logs
  case saveRes of
    Left e -> putStrLn $ "There was an error: " <> show e
    Right _ -> putStrLn "OK"
```

That's a lot neater looking, but how can you read it and how does it work? Lets look at how we'd read the type signature of saveThingo.

```haskell
saveThingo :: (MonadIO m, MonadError OurErrors m) => Thingo -> m ()
```

This means that we building a program that can do monady things (i.e do notation) and at each step we can either do IO or we can throw / catch errors of type Error. 

```haskell
saveThingo :: (MonadIO m, MonadError OurErrors m) => Thingo -> m ()
saveThingo t = do
  fileExists <- liftIO $ doesPathExist "savedthingo.json"
  when fileExists $ throwError OutputFileAlreadyExists
  liftIO $ T.writeFile "savedthingo.json" (thingoToJsonTxt t)
```

LiftIO comes from [Control.Monad.IO.Class](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-IO-Class.html) 
and throwError comes from [Control.Monad.Except](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html). 
Tell (in the previous example) comes from [Control.Monad.Writer](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Writer-Lazy.html).

Here we are not dealing with concrete transformers but using functions built around typeclasses. This allows us to write functions that say
"I need a monad that can throw an error, but I don't care about anything else" which means that the bulk of our code doesn't care about the
concrete transformers that are underneath. 

But the concrete transformers are still there: we just delay making a decision about it till right in main. As you can see, our main didn't change
compared to the transformers version because mtl defines all the right instances for WriterT, ExceptT and IO to make
`WriterT [Text] (ExceptT OurErrors IO) a` fit `(MonadError OurErrors m, MonadIO m, Monadwriter [Text] m) => m a! :)

```haskell
main = do
  (saveRes, logs) <- (runWriterT (runExceptT program) :: IO (Either OurErrors (), [Text]))
  putStrLn "Program Logs:"
  for_ T.putStrLn logs
  case saveRes of
    Left e -> putStrLn $ "There was an error: " <> show e
    Right _ -> putStrLn "OK"
```

There is a catch to using mtl style constraints. Because of how the types are specified (they are created in a way so that it maximises 
type inference so that you get what you want without having to annotate types) it means that you can only have one MonadWriter constraint 
in your function and everything needs to have the same log type. There are two ways around this which are outside the scope of this article:

- ClassyMtl style constraints where you defer the concreteness of the error / writer types with classy lenses and prisms. See this article for more info: https://carlo-hamalainen.net/2015/07/20/classy-mtl/
- Newtyping WriterT/ExceptT/Etc and defining all the instances that are needed again (basically copying and pasting your own MonadWriter with a newtype). Obelisk does this here: https://github.com/obsidiansystems/obelisk/blob/develop/lib/route/src/Obelisk/Route/Frontend.hs#L271 wrapping up an event writer (so that obelisk users can still use EventWriter) and creating a specialised SetRoute class for operations on that eventwriter. 

# EventWriterT and EventWriter

EventWriterT is much like WriterT in that it collects up events that your widget has fired and keeps hold of them until
you call [runEventWriterT](http://hackage.haskell.org/package/reflex-0.6.1/docs/Reflex-EventWriter-Base.html#v:runEventWriterT)
where it emits the output of your widget as (a, Event t w). EventWriter needs w to be a semigroup so that it can append the events together,
just like WriterT does. Often you use Endo or a NonEmpty list of events as your semigroup.

This is really useful when you want something really deep in your widget tree to emit an event up some layers of widgets.
Like how writer is for collecting information that the outer program needs and the inner stuff doesn't want to care about it.

The biggest thing with reflex is that you are never actually dealing with a concrete tranformer stack. So the only
way to interact with an EventWriterT is to use tellEvent from
[Reflex.EventWriter.Class](http://hackage.haskell.org/package/reflex-0.6.1/docs/Reflex-EventWriter-Class.html#t:EventWriter).
It all feels a bit abstract when just thinking in the reflex mtl style only.

But remembering how transformers work, lets look at this eventwriter code and see how it works:

```haskell
buttonW :: (EventWriter () m, DomBuilder t m) => Text -> m ()
buttonW t = do
  clickE <- button t
  tellEvent clickE

buttons :: (EventWriter () m, DomBuilder t m) => Text -> m ()
buttons = do
  buttonW "Button 1"
  buttonW "Button 2"
  buttonW "Button 3"

counter :: (DomBuilder t n, MonadHold t m) => m ()
counter = do
  (_, clickE) <- runEventWriterT buttons  -- This forces the concrete type of buttons to be 
                                          -- (DomBuilder t n => EventWriterT () n)
  clicksDyn <- count clickE
  display clicksDyn
```

This works a lot like mtl, but we only peel off one layer and leave reflex to the rest of it's abstraction. This means that our eventwriter is always on top, but that's almost always OK!
