# NFA to DFA Conversion in Haskell

## Format of the Input
The input for the NFA to DFA conversion program follows a specific format. Each transition is represented in one of the following two formats:
- `STATE | SYMBOL STATE`: This format indicates a transition from `STATE` to `STATE` when the input symbol is `SYMBOL`.
- `STATE | SYMBOL1 STATE1, SYMBOL2 STATE2`: This format represents a transition from `STATE` to `STATE1` when the input symbol is `SYMBOL1`, and from `STATE` to `STATE2` when the input symbol is `SYMBOL2`.
  Example included in input.txt

## How to Run the Program?
```haskell
runghc <file_name> <words, if not present in the input file>
``` 
Example
```
runghc f_noinput.txt a ab aaa
runghc f_input.txt
```
If words are present in the arguments then words in the file if present are ignored
## NFA
A Nondeterministic Finite Automaton (NFA) is a mathematical model used in computer science and formal language theory. It consists of a finite set of states, a set of input symbols, a transition function, an initial state, and a set of accepting states. Unlike a Deterministic Finite Automaton (DFA), an NFA can have multiple possible states to transition to for a given input symbol.

## DFA
A Deterministic Finite Automaton (DFA) is another mathematical model used in computer science and formal language theory. Similar to an NFA, it consists of a finite set of states, a set of input symbols, a transition function, an initial state, and a set of accepting states. However, in a DFA, there is only one unique state to transition to for each input symbol.

## Subset Construction

Subset construction is a technique used in automata theory to convert a nondeterministic finite automaton (NFA) into an equivalent deterministic finite automaton (DFA). The subset construction algorithm systematically explores the possible states that the NFA can be in, based on the input symbols, and constructs the corresponding DFA.

The subset construction algorithm works as follows:

1. Start with the initial state of the NFA as the initial state of the DFA.
2. For each input symbol, compute the set of states that the NFA can transition to from the current DFA state. This set is obtained by considering all possible epsilon transitions and transitions for the input symbol.
3. If the computed set of states is not already a DFA state, create a new state in the DFA and add it to the set of unprocessed DFA states.
4. Create a transition from the current DFA state to the newly created DFA state for the current input symbol.
5. Repeat steps 2-4 for all unprocessed DFA states until no new states are created.

The resulting DFA obtained through subset construction will have a unique state for every subset of states from the original NFA. The transition function of the DFA is determined by the transitions between subsets of states in the NFA.


