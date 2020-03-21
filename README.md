# ben (Binary Enumerated Numerics)

This is a simple CLI tool, inspired by awk, for generating binary data by enumerating over a given range of values, matching on a binary pattern, and returning the associated value.


## What is it?
A tool for generating binary files using simple pattern-matching rules for use in programming ROM chips. These chips have some number of input address lines and some number (typically 8) output data lines. When used in place of sequential logic chips, ROM chips can be used to simplify designs and as such the data written to the chips is often the result of some simple expression depending on the input address lines.

The program simply takes the width of the input patterns (the number of address lines) and iterates from 0 to 2 ^ width. For each such value, the first pattern to match is evaluated and output.

## Example Patterns

* Wiping the ROM chip, which is to say setting the output to `0xFF` for all inputs
```
''
```
Note: any values in range that do not match any pattern will result in `0xFF`

* Setting all lines of 256 byte chip to `0x00`
```
'--------:0|0|0|0|0|0|0|0'
```

* Even lines should output `000`, odd lines `111`
```
'-0:0|0|0;-1:1|1|1'
```

* Output should be 1 more than input
```
"xx:(x + 1)"
```
Gives [001, 010, 011, 100]

* Module that 1 more than the input by 4 to ensure 2-bit output
```
"xxx:(( x + 1 ) % 4)"
```
Gives [1,10,11,0,1,10,11,0]

* Equality is useful for outputing status e.g. zero flag from an ALU
```
"xxyy:(((x + y) % 4) == 0)"
```
Gives [1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0]


## TODOs
- Output to stdout as binary data
