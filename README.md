# NFA to DFA Conversion in Haskell

## Format of the Input
The input for the NFA to DFA conversion program follows a specific format. Each transition is represented in one of the following two formats:
- `STATE | SYMBOL STATE`: This format indicates a transition from `STATE` to `STATE` when the input symbol is `SYMBOL`.
- `STATE | SYMBOL1 STATE1, SYMBOL2 STATE2`: This format represents a transition from `STATE` to `STATE1` when the input symbol is `SYMBOL1`, and from `STATE` to `STATE2` when the input symbol is `SYMBOL2`.
  Example included in input.txt

## How to Run the Program?
```haskell
runghc main.hs <file_name> <optional words>

``` 
To minimize it as well after converting to DFA, add "-m" after the file_name.
Example
```
runghc main.hs input.txt a ab aaa
runghc main.hs input_words.txt
```
Words in the arguments are added to the list of words present in the input file.
