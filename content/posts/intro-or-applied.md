---
title: Introduction or Applied FP Course?
date: 2018-10-23
project: professional-fp-courses
authors: tmorris
---

At QFPL, we often run Functional Programming courses, both to raise awareness for the general community, and [professionally](/projects/professional-fp-courses). We typically offer an Introductory FP Course, which starts at the beginning with fundamental FP exercises, and the Applied FP Course Attendees, which assumes prior FP knowledge and focusses on web and database programming. Participants are often unsure which is most appropriate for them to attend. This post hopes to make a clear test to help answer this question.

During the Introductory FP Course, we cover fundamental concepts such as `Applicative` and `Monad` and why they are used. We investigate the degree of abstraction these concepts provide and discuss the practical consequences. Typically, after these concepts are introduced, participants have gained a hold on the general idea and why we might exploit them in our everyday software engineering. These ideas are introduced over a short period, and although it is not expected to achieve complete fluency, a participant is well-equipped to self-study these ideas in the long term.

General comfort (though not complete fluency) with these ideas is therefore a prerequisite for the Applied FP Course. If you are unsure what that might be like, following are some concrete questions to test yourself on.

1. Write a function with the following type: `Applicative f => [f a] -> f [a]`.

2. There are five functions below. Four of them will always give the same output, for given inputs. Which is the odd one out?

    ```haskell
    f1 :: Maybe Int -> Maybe Int -> Maybe Int
    f1 fa fb = (\a b -> a + b * 10) <$> fa <*> fb
    ```

    ```haskell
    f2 :: Maybe Int -> Maybe Int -> Maybe Int
    f2 fa fb = do a <- fa
                  b <- fb
                  pure (0 + a + b * 10)
    ```

    ```haskell
    f3 :: Maybe Int -> Maybe Int -> Maybe Int
    f3 fa fb = do a <- fa
                  b <- (*10) <$> fb
                  pure (b + a)
    ```

    ```haskell
    f4 :: Maybe Int -> Maybe Int -> Maybe Int
    f4 fa fb = do a <- (*10) <$> fa
                  b <- fb
                  pure (b + a)
    ```

    ```haskell
    f5 :: Maybe Int -> Maybe Int -> Maybe Int
    f5 fa fb = do b <- (\x -> x * 10) <$> fb
                  a <- fa
                  pure (b + a)
    ```

3. Do these two programs always give the same result? Why?
    
    ```haskell
    p1 ::
      IO ()
    p1 =
      let file = "/tmp/file"
      in  do  _ <- writeFile file "abcdef"
              x <- readFile file
              _ <- putStrLn x
              _ <- writeFile file "ghijkl"
              y <- readFile file
              putStrLn (show (x, y))
    ```

    ```haskell
    p2 ::
      IO ()
    p2 =
      let file = "/tmp/file"
          expr = readFile file
      in  do  _ <- writeFile file "abcdef"
              x <- expr
              _ <- putStrLn x
              _ <- writeFile file "ghijkl"
              y <- expr
              putStrLn (show (x, y))
    ```

4. Which of these programs will compile? How many are equivalent to each other?


    ```haskell
    getFile :: FilePath -> IO (FilePath, Chars)
    getFile name = 
      readFile name >>= \contents -> pure (name, contents)
    ```

    ```haskell
    getFile :: FilePath -> IO (FilePath, Chars)
    getFile name = do
      contents <- readFile name
      pure (name, contents)
    ```

    ```haskell
    getFile :: FilePath -> IO (FilePath, Chars)
    getFile = 
      lift2 (<$>) (,) readFile
    ```

    ```haskell
    getFile :: FilePath -> IO (FilePath, Chars)
    getFile name = 
      ((,) name) <$> readFile name
    ```

    ```haskell
    getFile :: FilePath -> IO (FilePath, Chars)
    getFile name = 
      lift2 (>>=) readFile (,)
    ```

    ```haskell
    getFile :: FilePath -> IO (FilePath, Chars)
    getFile name = 
      (name, readFile)
    ```

    ```haskell
    getFile :: FilePath -> IO (FilePath, Chars)
    getFile name = 
      (\contents -> (name, contents)) <$> readFile name
    ```

    ```haskell
    getFile :: FilePath -> IO (FilePath, Chars)
    getFile name = 
      pure ((,) name) <*> readFile name
    ```

    ```haskell
    getFile :: FilePath -> IO (FilePath, Chars)
    getFile name = 
      readFile name >>= ((,) name)
    ```

If you can confidently answer these questions, then the Applied FP Course is for you. If you feel you need to brush up on these ideas, then the Introductory FP Course will cover them off.
