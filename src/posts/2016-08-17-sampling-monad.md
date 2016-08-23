-----
title: Implementation of Sampling Monad
-----
##### Haskell

We want a monad so that we may compose probabilistic computations.

Essentially, we want to write code that looks like the following:

```haskell
fairCoin :: Distribution Bool
fairCoin = binomial 0.5

shadyGambler :: Bool -> Distribution Double
shadyGambler b = if b then discrete [(100, 0.9), ((-100), 0.1)]
                 else certain (-100)
```


> Note: We'll introduce the Distribution a datatype and associated typeclasses later on.

Which represents the following:

We have a fair coin where the event of getting a heads (corresponding to True)
has a Bernoulli distribution parameterized by 0.5.

> Our fair coin lands on heads (True) 50% of the time.

We also have a shady gambler that you can play against. If you toss a coin,
and it lands on **tails**, you lose 100. If it lands on heads you win 100!
But 10% of the time, when the coin lands on heads, the shady gambler runs
away with your money: basically a distribution over Doubles.

Now we would like to produce samples after composing distributions. We
would like to first sample a Boolean from our fairCoin, then pass it as a parameter
to our shadyGambler. And in order to do this, we would like to write code that
looks like this:

```haskell
fairCoinShadyGambler :: Distribution Double
fairCoinShadyGambler = do
    coinResult <- fairCoin
    shadyGambler b
```

Now, we can sample from this distribution and obtain results that we can then
use for whatever purpose.

This example isn't very impressive; you could work out the resulting
distribution on pencil and paper quite easily. But using this
monad we can work our way up to describing much more complex
probability distributions!

For example, let's say that the amount of rain **tomorrow** is normally
distributed and is conditioned on the amount of rain **today**.

We can write it like this:

```haskell
rainTomorrow :: Double -> Distribution Double
rainTomorrow d = if d == 0 then normal 1.0 0.3
                 else if d < 10 then normal (0.8 * d) (0.3 * d)
                 else if d >= 10 then normal (d / 2) (d / 6)
```

This conditional probability distribution allows us to now describe
future events as a a function of current observations.

But let's say we want to produce a probabilistic function that takes
current rain levels and predicts distributions (and eventually values)
of rain **3 days** from now.

```haskell
rain3Days :: Double -> Distribution Double
rain3Days = rainTomorrow >=> rainTomorrow >=> rainTomorrow
```

Or alternatively:

```haskell
rain3Days' :: Double -> Distribution Double
rain3Days' d = do
    d' <- rainTomorrow d
    d'' <- rainTomorrow d'
    rainTomorrow d''
```

Notes:

With my original implementation, the monad laws were broken.

This is because I was sampling **twice** (using the Box-Muller method)
and using the 2nd returned RNG when sampling from a normal distribution
(when I was only sampling **once** and returning the first RNG
for most other distributions, and sampling **zero** times
for Certain distributions).

In order to make these laws work though, we had to get the next
RNG when sampling from the Certain distribution
(making sure to sample from the same range of numbers each time),
and return the first RNG (from the first out of two samples) returned
when sampling from the normal distribution.

-----

Show profiling charts here, demonstrate linear in number of
composed MonteCarlo samples.
