#include "lexer.h"

int Lexer::gettok() {
    static int lastChar = ' ';

    // Skip any whitespace.
    while (isspace(lastChar)) {
        if(lastChar == '\n')
          line++;
        lastChar = getchar();
    }

    // identifier: [a-zA-Z][a-zA-Z0-9]*
    if (isalpha(lastChar)) {
        identifierStr = lastChar;
        while (isalnum((lastChar = getchar()))) {
            identifierStr += lastChar;
        }

        if (identifierStr == "def") {
            return tok_def;
        }

        if (identifierStr == "extern") {
            return tok_extern;
        }

        if (identifierStr == "if") {
            return tok_if;
        }

        if (identifierStr == "then") {
            return tok_then;
        }

        if (identifierStr == "else") {
            return tok_else;
        }

        if (identifierStr == "for") {  
          return tok_for;
        }

        if (identifierStr == "in") {
          return tok_in;
        }

        if (identifierStr == "var") {
          return tok_var;
        }

        if (identifierStr == "return") {
          return tok_return;
        }

        if (identifierStr == "double") {
          return tok_double_string;
        }

        if (identifierStr == "int") {
          return tok_int_string;
        }

        if (identifierStr == "char") {
          return tok_char_string;
        }

        if (identifierStr == "string") {
          return tok_string_string;
        }

        if (identifierStr == "main") {
          return tok_main;
        }

        if (lastChar == '[')
          return tok_array;

        return tok_identifier;
    }

    // Number: [0-9.]+
    if (isdigit(lastChar) || lastChar == '.') {
      bool floating = false;  
      std::string numStr;
      do {
          numStr += lastChar;
          if(lastChar == '.')
            floating = true;
          lastChar = getchar();
      } while (isdigit(lastChar) || lastChar == '.');

      if(floating) {
        numVal = strtod(numStr.c_str(), 0);
        return tok_double;
      }
      numValInt = strtod(numStr.c_str(), 0);
      return tok_int;
    }

    if(lastChar == '-') {
      int thisChar = lastChar;
      lastChar = getchar();
      if (isdigit(lastChar) || lastChar == '.') {
        bool floating = false;  
        std::string numStr = "-";
        do {
            numStr += lastChar;
            if(lastChar == '.')
              floating = true;
            lastChar = getchar();
        } while (isdigit(lastChar) || lastChar == '.');

        if(floating) {
          numVal = strtod(numStr.c_str(), 0);
          return tok_double;
        }
        numValInt = strtod(numStr.c_str(), 0);
        return tok_int;
      }
      return thisChar;
    }

    // comment until end of line.
    if (lastChar == '#') {
        do {
            lastChar = getchar();
        } while (lastChar != EOF && lastChar != '\n' && lastChar != '\r'); 

        if (lastChar != EOF) {
            return gettok();
        }
    }

    // Check for end of file.  Don't eat the EOF.
    if (lastChar == EOF) {
        return tok_eof;
    }

    if (lastChar == '=') {
      int thisChar = lastChar;
      lastChar = getchar();
      if(lastChar == '=') {
        lastChar = getchar();
        return tok_equal;
      }
      return thisChar;
    }

    if (lastChar == '!') {
      int thisChar = lastChar;
      lastChar = getchar();
      if(lastChar == '=') {
        lastChar = getchar();
        return tok_nonequal;
      }
      return tok_negation;
    }

    if (lastChar == '\\') {
      int thisChar = lastChar;
      lastChar = getchar();
      if(lastChar == 'n') {
        identifierStr = (char) 10;
        lastChar = getchar();
        return tok_identifier;
      }
      return thisChar;
    }

    if(lastChar == '\"') {
      identifierStr = "";
      while ((lastChar = getchar()) != '\"') {
        if(lastChar == '\\') {
          int thisChar = lastChar;
          lastChar = getchar();
          if(lastChar == 'n') 
            lastChar = (char) 10;
          else
            identifierStr += thisChar;
        }
        identifierStr += lastChar;
      }
      lastChar = getchar();
      return tok_string;
    }

    // Otherwise, just return the character as its ascii value.
    int thisChar = lastChar;
    lastChar = getchar();
    return thisChar;
}