# match

This was a take home assignment I did as part of a job interview.

## Usage

```
% cat example.txt | match "hello %{1}"
hello paul
hello peter
hello philip
```

## Development

### Outline

- `app/Main.hs` contains the main entry point
- `src/Lib.hs` contains the parsing, compiling and matching functions

### Tasks

```
% make help
clean                          Clean Haskell local packages
help                           Print available tasks
install                        Compile Haskell binary
repl                           Launch ghci
spec                           Run the specs
watch                          Compile on file changes
```

### Tests

```
Progress: 1/2Engine Tests
  All specs
    Parser Specs
      Parser
        decode
          should parse a simple capture:                                           OK
          should parse a space limitation capture:                                 OK
          should parse a greedy capture:                                           OK
          should parse a mixture of different captures:                            OK
          should parse text:                                                       OK
          should parse text mixed with captures:                                   OK
          should parse text with partial captures:                                 OK
    Compiler Specs
      Compiler
        compile'
          should compile a pattern of text to a regex string:                      OK
          should compile a pattern of simple captures to a regex string:           OK
          should compile a pattern of space limitation captures to a regex string: OK
          should compile a pattern of space limitation captures to a regex string: OK
          should compile a pattern of greedy captures to a regex string:           OK
          should compile a mixed pattern to a regex string:                        OK
    Match Specs
      Match
        match'
          should check if our regex matches:                                       OK
    Sanitisation Specs
      Santisation
        sanitise
          should not touch captures:                                               OK
          should escape regex symbols:                                             OK
    Integration Specs
      Integration
        should pass the simple pattern specification:                              OK
        should pass the space limitation pattern specification:                    OK
        should pass the greedy pattern specification:                              OK

All 19 tests passed (0.00s)
```
