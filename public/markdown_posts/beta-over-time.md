# Is $\beta$ a static quality?
## Subject: Finance, Statistics
### Oct 30, 2018

Just to clarify, we're talking about CAPM $\beta$.

### First question: is beta static?

When talking about $\beta$, it's often in the
context of setting up a beta-neutral portfolio,
or performing risk-attribution.

Although we may care about beta in that
instantaneous moment, in practice all one does
is take the historical returns of a stock
and regress it against some index.

This, naturally, invites the question
"how does beta change over time?". If there
was a pattern in how beta drifts over time,
perhaps there are better rebalancing strategies?

### Method

I took a couple of stocks (not all) in the Dow 30, and
regressed their returns against the SP500.
This choice was arbitrary; I'm sure a similar
analysis can be done with a different stock
universe and a different index. And the choice of
a small subset was simply because my tiny laptop screen
can't render all of them!

I then, for each day, regressed the stock returns
for the past quarter against the index returns,
and output the beta coefficient.

This is the result.

![](/images/beta-without-2008.png)

### Comments

It's interesting how some stocks have a very well behaved
beta drift over time.

This invites more questions as to why some beta
drift looks to be almost linear; granted, the magnitude
is not large, but it's irresistible to imagine
what could be causing this drift.

Furthermore, it's important to note that these
are levered beta's.

Some hypotheses I have are:

- Flows into index-following passive vehicles
    are resulting in stocks having a stronger
    correlation with the overall index
- Pricing in steady de-leveraging in some cases

If anyone has comments about this, feel free to get in touch!
