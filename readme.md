# PETSCIIdad

Live-coding visuals for the C64, similar to ANSIedad.

## Instructions

These are the available operations:

Key|Operation
:---:|:---:
R|Right shift
L|Left shift
A|Bitwise AND
O|Bitwise OR
X|Bitwise XOR
M|Multiplication
S|Substraction
T|t
0|Cursor "home" (NAO)
any other|Numerical value

*Note that operations also have a numerical value!*

PETSCIIdad operates with a "stack". At start, T is in the stack.
Up to four operations can be evaluated.

Check some of these (replace `1` with any number and `?` with any character):

```
1RTA
1RTX
1RTS
1RTO
1L
1R
1RTS

?AMM
?AMS
?AMO
?ASS
?A?S

AMM
AMMO
AMO
A1MX
AMS
AMSO
ASMO
AAMO
ASMX
A?AS
AXSM

M
MSAS
MASA
MASS
MSOS
MO
MO1O
MO1R
MOSX
MO?A
MOSA
MS?A

SM?S
SMSA
SMSO

TAMO
XSO
```

