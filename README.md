# inline-python

This is library which embeds python interpreter into haskell programs and allows
calling python code from haskell and haskell from python seamlessly. This
project is inspired by [haskell-R](https://tweag.github.io/HaskellR). and tries
to use similar conventions.

As an example take following program. It captures from environment variables
with `_hs` suffix. This includes haskell functions.

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Python.Inline
import Python.Inline.QQ

main :: IO ()
main = withPython $ do
  let input = [1..10] :: [Int]
  let square :: Int -> IO Int
      square x = pure (x * x)
  print =<< fromPy' @[Int] =<< [pye| [ square_hs(x) for x in input_hs ] |]
```

it would output:

```
[1,4,9,16,25,36,49,64,81,100]
```

