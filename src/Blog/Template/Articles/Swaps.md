## What are **swaps**?
##### Finance - Derivatives

A good friend asked a good question the other day:

> [GoodFriend] : What are [swaps](https://en.wikipedia.org/wiki/Swap_(finance))?

Good question. Before talking about swaps, I'd like to preface this post by mentioning
that Wikipedia, Investopedia, and other resources online/at your local library can certainly provide a more detailed,
technical, and accurate explanation than I ever could; however, if what you want is to just know what a swap **is**,
without having to touch any weird financial jargon (while picking some up along the way!)
then you've come to the right place! We'll answer this question at a high level by using three common examples of swaps:

1. [Interest rate swaps](https://en.wikipedia.org/wiki/Interest_rate_swap) (IRS)
2. [Currency swaps](https://en.wikipedia.org/wiki/Currency_swap) (not to be confused with [foreign exchange swaps](https://en.wikipedia.org/wiki/Foreign_exchange_swap)
which are a very different kind of swap)
3. [Credit default swaps](https://en.wikipedia.org/wiki/Credit_default_swap) (CDS)

First, a nice, general, informal definition of a swap: 

> A swap is an agreement between two parties to exchange 
> **certain amounts** of **certain things** at **certain times** in a **certain period.**

This definition doesn't really help us though. To understand better what a swap is, it's
necessary to know *why* somebody would want to enter into such an agreement.

> Typically, a party will want to enter a swap agreement
> in order to shield themselves from certain risks
> (also known as hedging).

Okay, that's a bit better, but still a bit too general and abstract. What are the certain risks that
we would like to protect ourselves against?

Each swap agreement described above is entered for the purpose of hedging against a different type of risk; respectively:

1. [Interest rate risk](https://en.wikipedia.org/wiki/Interest_rate_risk)
2. [Foreign exchange risk](https://en.wikipedia.org/wiki/Foreign_exchange_risk)
3. [Credit risk](https://en.wikipedia.org/wiki/Credit_risk)

Cool! First let's understand these risks (again, at a high level), then we can talk about how these swaps can 
protect us against these risks.

### Interest Rate Risk

I'll assume that you, the astute reader, kind of have an idea of what an [interest rate](https://en.wikipedia.org/wiki/Interest_rate)
is. If not, here's a quick definition. If there's demand for it, the topic of interest rates
might warrant [another blog post](www.kevinl.io/blog/posts/interestratepostplaceholder).

> An interest rate, generally expressed as an annual percentage, 
> is the cost of borrowing (if you've borrowed money) **or**
> the return on lending (if you've lent money).

So if you're a Big Business Company and you want to take out a loan of **100,000 Currency** from your local Bank of Money
to spend on Business Expenses and repay in **one year**, you'll need to pay an interest rate on that loan.
Let's say that the Bank of Money hands you a loan contract, and it says that in return for giving you this money today,
you must pay a **fixed** (remember this word! It's important!) **5% annual interest rate** on the initial 100,000
(also called the principal). And so you sign the contract because you think 5% is fair (perhaps because all
the other banks offered a similar deal, or perhaps 5% is close enough to a proxy for interest rates such as
the US 1-year Treasury Bill yield (don't worry if you don't know what this is. You can consider it, for now,
just as some percentage number that fluctuates over time)).

After you sign, you get your 100,000 Currency and start buying Business Equipment, keeping in mind that in one year,
you'll have to give the bank **105,000** Currency (100,000 from paying back the principal, plus 5% of the principal).

So a year goes by, and your Big Business Company has been making money using the Business Equipment
that you had bought. Your loan contract says that now it's time to pay up. 100,000 plus 5,000 Currency is hard to part
with, but you signed the contract and thus, legally, you must honor it
([to the best of your ability](www.kevinl.io/blog/posts/bankruptcypostplaceholder)).

But wait! Interest rates change over time! You look at the interest rate
on a one year loan at the Bank of Money, and you notice that if you take out another 100,000 Currency today, you would
only have to pay an annual **4%** interest rate. Sweet! The interest rate is 4% now, does that mean that you only
have to pay 104,000 Currency today?

Nope. Remember how the contract you signed had a **fixed** interest rate of 5% per annum (meaning annually)? You're stuck paying
105,000 Currency, my friend. Better shell out. But as you fork over your hard-earned cash, you sigh and wonder: "Wouldn't it be nice
if my loan wasn't fixed to a constant interest rate and I could just pay interest at the *current* interest rate whenever
it became due?"

You leave the Bank of Money, return to your Big Business Company, and continue doing Business Things while grumbling how fixed
interest rate loans suck.

Now let's look at your business rival who runs Evil Business Inc, who just walked into the Bank of Money 
to take out a 100,000 Currency loan in order to buy Evil Business Equipment. She thinks she's so clever (classic evil person),
so she asks for a **floating** rate loan. The Bank of Money obliges, and gives her a contract for a one year loan of a
100,000 Currency principal, with an annual interest rate pegged to the [LIBOR](https://en.wikipedia.org/wiki/Libor)
+ 0.50% (the LIBOR is the London Interbank Offered Rate, and it is one of those commonly quoted fluctuating
interest rates out there.).

She quickly looks up the current 1 year LIBOR, and sees that it's **3%**. With the additional 0.50%, that's only a **3.50%**
interest rate. That means that in a year, assuming LIBOR doesn't change (but you, the reader, now probably know
what's coming next), she'll only have to pay **103,500 Currency.**

"What a steal!" she cackles as she signs the contract with a crow's feather dipped in pig's blood.

One year later, she comes back triumphantly to repay her debt of 100,000 + 3,500 Currency. But what's this? The 1 year LIBOR has gone up from 3% to 5%! This means that instead of paying 103,500 Currency, she'll have to pay 105,500 instead.

---

It should be pretty clear what the risk is here!

> Interest rate risk is your exposure to changes in interest rates.
> Fixed rate loans are exposed to downward shifts in interest rates.
> Floating rate loans are exposed to upward shifts in interest rates.
> The converse is true if you are the party who is lending money.

Let's see how Interest Rate Swaps can help us hedge against this risk.

### Interest Rate Swaps

Remember how we had earlier defined a swap as an agreement to exchange certain things at certain periods in time? Well,
we can use such an agreement to take advantage of decreases in interest rates, if we're currently locked into a fixed
rate contract.

Let's say that in our previous case where you had a contract for 100,000 Currency at 5%, you believed that it is likely
that interest rates are going to go down; specifically, the LIBOR. But you can't change the terms of the contract, so
you can form an agreement with someone else that looks like this:

1. You pay them LIBOR + 0.50% on some agreed upon amount (called the **notional**) every year. This notional
amount generally doesn't change hands; it's just there so that we can express payments in percentages.
Let's say that this amount is 50,000 Currency.
2. They pay you a fixed 4% on the notional every year.

Now, we have effectively hedged ourselves slightly against drops in LIBOR. Let's do some math!

Our original contract was for 100,000 Currency, to be paid in one year, at 5% fixed per annum.

We currently have agreed to pay floating-for-fixed, with the terms that we are to **pay** LIBOR + 0.50% and **receive** 4%
on a notional amount of 50,000 Currency.

Let's say that LIBOR, when the debt comes due, has fallen to 2.00%. Without this agreement, we would be stuck paying
105,000 Currency. But now that we have this interest rate swap, we now receive 4% on 50,000 Currency (which comes out
to 2,000 Currency) and pay 2.50% of 50,000 (which comes out to 1,250 Currency), which provides us with a net
income of 750 Currency!

Similarly, if we're on the opposite side of the table (i.e. we are currently locked into a floating rate loan,
and we believe that rates are going to go up), we can choose to enter into an interest rate swap agreement
so that we **pay fixed** and **receive floating.**

Thus, we can use interest rate swaps to control our exposure to changes in interest rates! Whee!

> Note: It should be made clear that of course there is also **upside** exposure if we don't enter
> into an interest rate swap agreement (e.g. if we're in a fixed rate contract, and rates go up, then we're paying
> less than what the market would have us pay. If we're in a floating rate contract, and rates go down,
> then we're paying less than what we otherwise would have paid if rates did not go down).
> 
> This of course is possible! But, as in many cases pertaining to finance and accounting, we want
> to avoid surprises. In other words, we'd like our financial position's sensitivity to variance in interest
> rates to be as low as possible.

### Foreign Exchange Risk

Now that we have a nice understanding of interest rate risk, it should be much easier to generalize
this concept to other forms of risk!

Simply put:
> Foreign exchange risk is risk from to unexpected fluctuations in exchange rates.
> This kind of risk occurs when business is conducted across borders, and transactions
> may be denoted in foreign currencies.

> *Sidenote*: There are many different kinds of exposure when it comes to foreign exchange risk, but it's
> not important to go into that much detail right now. But for those who are more financially versed,
> we'll only talk about foreign exchange risk in the context of [transaction exposure](https://en.wikipedia.org/wiki/Foreign_exchange_risk#Transaction_exposure).