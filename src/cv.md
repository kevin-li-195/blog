-----
title: Curriculum Vitae
-----

## Programming Languages

1. Haskell
2. C#
3. Java
4. Python
5. Javascript
6. HTML/CSS

## Education

**McGill University** - Montreal, Canada - Class of 2017,
Bachelor of Commerce, Major in Finance with Minor in Computer Science

**International School of Beijing** - Beijing, China - Class of 2013,
International Baccalaureate

## Work Experience

### Morgan Stanley - Institional Securities Technologies - Montreal, Canada
Position: Summer Technology Analyst
Languages used: C#

- Developed new tools for fixed income traders.
- Extended functionality of an internal trading application.

### McGill University - Advanced Networking Research Lab - Montreal, Canada
Position: Research Assistant
Languages used: Javascript

- Implemented discrete event simulation using SimJS to simulate effects 
of social policies on populations.

### Ahvoda Recruitment - Montreal, Canada
Position: Co-Founder
Languages used: Python

- Developed stochastic model using 2,700 gathered datapoints to target
users with preferred job listings.

## Projects

Additional projects can be found on my [Github](https://www.github.com/kevin-li-195).

### **[stat-sampling - Haskell](https://www.github.com/labcoders/jafar)**

Library for monadic composition of probabilistic functions using
arbitrary probability distributions.

The Sample monad allows us to sample from the result of the composed
probabilistic function, which can help us numerically approximate the
distribution of a complicated statistical process. Haskell's do-notation
lets us intuitively describe the act of sampling from a Sample computation.

An extension to the Sample monad is the StochProcess type, which
allows us to construct stochastic processes by monadic composition.
As the StochProcess type is built on top of the Sample monad, we can
use the two together by simply lifting the Sample computation to the
StochProcess type.

A better description can be found in one of my [blog posts](http://kevinl.io/posts/2016-08-17-sampling-monad.html).

### **[Jafar - Haskell](https://www.github.com/labcoders/jafar)**

EDSL for describing Bitcoin trading algorithms, backtester for
said algorithms, plus sample EMA crossover algorithm.

### **[Zeno - Haskell](https://www.github.com/kevin-li-195/zeno)**

Tool for interacting with the mouse cursor with Vim-like commands.
