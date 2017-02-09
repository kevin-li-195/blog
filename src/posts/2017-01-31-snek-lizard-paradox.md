-----
title: The Snake Lizard Paradox
subject: Probability/Haskell
-----

Written jointly with [Jacob Errington](https://jerrington.me).

### Suppose your friend visits a pet store...

on two separate days,
and purchases a pet on each day by tossing a fair coin.
Heads, she buys a snake. Tails, she buys a lizard.

After she's purchased these pets, you show up at her
house and see a snake on the floor. What is the probability
that the other pet is a snake?

Intuitively, both purchases are made independently right?
So you would expect that the probability that the other
pet be a snake is 50%

However, this is incorrect. Let us enumerate the possible
combinations of pets.

* Snake, Snake
* Snake, Lizard
* Lizard, Snake
* Lizard, Lizard

Notice that the last case is, in fact, impossible. In other words,
your friend could not have purchased two lizards because you
saw a snake on the ground. Hence, the state space is restricted
to the top three cases.

Observe that all events occur with equal probability. Let us
analyze each event to ascertain the probability that the
other pet is a snake given that we have observed at least
one snake.

In the first case, the probability that the other pet is a
snake is 1, even though we have no idea whether we saw the
first pet or the second pet.

In the second case, the probability that the other pet is a
snake is 0, because we have certainly seen one snake, and
if we have seen one then the other must be a lizard. This
is similar for the third case.

Thus we have $$ \frac{1 + 0 + 0}{3} = \frac{1}{3} $$ The probability
that the other pet is a snake, given that we have
observed a snake on the floor, is one third.

This is a bit surprising, but makes sense if you consider
that you have no idea which pet is the one that is
on the floor (i.e. is it the first or second?).

### Suppose that, if your friend buys a snake...

she names it "Mr. Slithers" with some small probability
*p*. Suppose that you visit her home, a snake slithers
over your feet, and your friend exclaims "that's Mr. Slithers!"
What is the probability that the other pet is a snake?

Now this may seem like one of those goofy physics problems
where they expect you to compute the mass of the sun given
that a green camel stands in the desert at night-time.

But there is a real solution! Really! I promise!

We can formulate the probability that we would like to
compute as a condition probability:

$$ \frac{}{} \mathrm{\Pr ( \text{Both pets are snakes} | \text{One snake is named Mr. Slithers})} $$

Which is read "The probability that both pets are snakes given that
at least one of them is named Mr. Slithers".

Using Baye's Theorem, we can reformulate that as:

$$ \frac{\Pr (\text{One snake is named Mr. Slithers} | \text{Both pets are snakes})
    \Pr(\text{Both pets are snakes})}{\Pr(\text{One snake is named Mr. Slithers})} $$

Now, we have three probabilities to compute!

First: 

$$\frac{}{} \Pr(\text{One snake is named Mr. Slithers} | \text{Both pets are snakes})$$

This is the probability that at least one snake is named Mr. Slithers, given
that both pets are snakes.

If we know that both pets are snakes, then the probability that at least one is
named Mr. Slithers is simply the probability of the complement of the event
that none of them are named Mr. Slithers. Because these events are independent
of each other, we can simply multiply the probability that the first snake
is not named Mr. Slithers with the probability that the second snake
is not named Mr. Slithers. In other words:

$$\begin{eqnarray}
    \Pr(\text{One snake is named Mr. Slithers} | \text{Both pets are snakes}) &=& \nonumber \\
    1 - (\Pr(\text{Snake 1 is not named Mr. Slithers})\Pr(\text{Snake 2 is not named Mr. Slithers}))
\end{eqnarray}$$

This expression is equal to: $$ \frac{}{} 1 - (1 - p)^2 = 2p - p^2 $$

Second:

$$ \frac{}{} \Pr(\text{Both pets are snakes}) $$

This is easy,
since there are four possible combinations of pets, and each one occurs with
equal probability. Thus, we have $$ \frac{1}{4} $$

Third: 

$$ \frac{}{} \Pr(\text{One snake is named Mr. Slithers}) $$

This one is trickier.
We can solve this by analyzing the four cases (i.e. four events), which again
occur with equal probability.

In the first event, where both pets are snakes, the probability that at least
one of them is named Mr. Slithers is, again, the probability of the complement
of the event that neither of them are named Mr. Slithers. Thus, it is

$$ 1 - (1 - p)^2 = 2p - p^2 \frac{}{}$$

In the second event, where pet one is a snake, and pet two is a lizard,
the only way to have one snake be named Mr. Slithers is for
pet one to be named Mr. Slithers. This event occurs with probability *p*.
This is the same for the third event.

For the fourth and final event, the probability that a snake is named
Mr. Slithers is 0. This is because in this event, there are no snakes.

Thus, we have: 

$$ \frac{2p - p^2 + p + p}{4} = \frac{4p - p^2}{4} $$

Now we recombine our expressions! This yields:

$$ \Pr(\text{Both pets are snakes} | \text{One snake is named Mr. Slithers})
= \frac{2 - p}{4 - p} $$

This is surprising and interesting, because it tells us that for
small *p*, the probability that the other pet is a snake
(if our friend shouts "that's Mr. Slithers!" when a snake
slithers across our feet) is close to $$ \frac{1}{2} $$ But for
large *p*, this amount is close to $$\frac{1}{3} $$
Interestingly, the probability of our friend choosing a name
for her snake tells us something about the other pet.

This result is so surprising that we decided to simulate it
(using the Haskell stochastic library!)

```haskell
import Data.Stochastic
import qualified Data.Sequence as S
```

Imports.

```haskell
data Pet = Snake | Lizard
    deriving (Eq, Ord, Show)

data Name = Slithers | NotSlithers
    deriving (Eq, Ord, Show)

Our datatypes representing names and
types of pets.
```

```haskell
petshop :: Sampler Pet
petshop = discrete [(Snake, 0.5), (Lizard, 0.5)]
```

This is the probability of picking a
pet when your friend is at the pet store,
according to a fair coin toss.

```haskell
name :: Double -> Pet -> Sampler Name
name p Snake = discrete [(Slithers, p), (NotSlithers, (1-p))]
name _ Lizard = pure NotSlithers
```

This is the naming function that your
friend uses.

```haskell
friend :: Double -> Sampler Pet
friend p = do
    p1 <- petshop
    p2 <- petshop
    n1 <- name p p1
    n2 <- name p p2
    if n1 == Slithers then do
        return p2
    else if n2 == Slithers then do
        return p1
    else friend p
```

This function represents the process
that your friend executes. She picks
two pets from the pet store, names
each of them randomly according to the
probability *p*, and depending on
which pet is named Mr. Slithers,
returns the species of the other pet.
In the case where she doesn't name
any pet Mr. Slithers, we simply try
again (since we assert that one pet is
a snake and is named Mr. Slithers).

```haskell
main :: IO ()
main = do
    let ps = [0.01,0.02..0.99]
        total = 10000
    results <- mapM (\x -> sampleION total (friend x)) ps
    let snakes = fmap (\x -> 
        fromIntegral (length (S.filter (== Snake) x)) / fromIntegral total) 
        results
    putStrLn $ show snakes
```

We simulate this process 10,000 times,
varying the value of p from small values
to large values. As **unexpected**, for
small *p* we get values close to $$\frac{1}{2}$$,
and for large *p* we get values close to
$$ \frac{1}{3} $$

Cool eh?

This is also known as the [boy girl paradox](https://en.wikipedia.org/wiki/Boy_or_Girl_paradox).
