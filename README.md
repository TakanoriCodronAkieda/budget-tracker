# budget-tracker

A haskell cli to manage your personal budget.

To compile, run
```ghc main```.

To log a spending into your budget manager, run 
```./main spend```

Similarly, to log an income, run
```./main receive```

Lastly, to view your transactions history, run
```./main history```

You can combine this is an arbitrary sequence like
```./main history spend spend receive history```
In this case, the commands will be executed from left to right.
