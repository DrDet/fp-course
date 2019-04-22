# hw3

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com//fp-homework/blob/master/hw3/LICENSE)
## Grammar
Code      -> Statement*

Statement -> Assignment

Assignment -> VarDef '=' Expression*

Expression -> VarAccess | TICK .*? TICK

VarAccess -> '$' STR_NAME

STR_NAME -> [a-zA-Z_0-9]+
## Done tasks:
