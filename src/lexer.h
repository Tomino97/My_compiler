#pragma once
#include <string>

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
  tok_eof = -1,

  // top level commands
  tok_main = -2,
  tok_def = -3,
  tok_extern = -4,

  //types
  tok_double = -5,
  tok_int = -6,
  tok_string = -22,
  //tok_char - for each character we have its int value
  tok_double_string = -7,
  tok_int_string = -8,
  tok_char_string = -9,
  tok_string_string = -23,

  // primary expressions
  tok_identifier = -10,
  tok_array = -11,

  // control
  tok_if = -12,
  tok_then = -13,
  tok_else = -14,
  tok_for = -15, 
  tok_in = -16,
  tok_return = -17,

  //operators
  tok_equal = -18,
  tok_nonequal = -19,
  tok_negation = -20,

  //definitions of variables
  tok_var = -21,
};

class Lexer {
public:
    std::string identifierStr;      // Filled in if tok_identifier
    double numVal = 0.0;            // Filled in if tok_double
    int numValInt = 0;              // Filled in if tok_int
    unsigned int line = 1;          // Counter for line

    // gettok - Return the next token from standard input.
    int gettok();
};