[define [fibo num] [if [= num 0] 1 [* num [fibo [- num 1]]]]]
[define main 5]