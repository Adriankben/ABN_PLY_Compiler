Copyright (c) <year>, <copyright holder>
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree. 

# Adrian Bennett 1800826 
# Brianna Philp 1903475 
# Ntini Edwards  1904701


import ply.lex as lex
import ply.yacc as yacc

# Press Shift+F10 to execute it or replace it with your code.
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.

reserved = {
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE'
}

# List of token names.
tokens = [
             'INT',
             'FLOAT',
             'IDENTIFIER',
             'STRING',
             'PLUS',
             'MINUS',
             'TIMES',
             'DIVIDE',
             'EQUALS',
             'EQUALITY',
             'GTHAN',
             'GTHANEQ',
             'LTHAN',
             'LTHANEQ',
             'LPAREN',
             'RPAREN',
             'LBRACE',
             'RBRACE'

         ] + list(reserved.values())

#  Regular expression rules for simple tokens
# t_ indicates a token (prefix it with r or R to make a raw string) escape character treated as a literal
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_TIMES = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\='
t_EQUALITY = r'\=='
t_GTHAN = r'\>'
t_GTHANEQ = r'\>='
t_LTHAN = r'\<'
t_LTHANEQ = r'\<='
t_LBRACE = r'\{'
t_RBRACE = r'\}'


# A regular expression rule with some action code
def t_FLOAT(t):
    r'[-+]?[0-9]*\.[0-9]+'
    # String to float
    t.value = float(t.value)
    return t


def t_INT(t):
    r'[-+]?[0-9]+'
    # String to integer
    t.value = int(t.value)
    return t


def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')  # Check for reserved words
    return t


# matches a string enclosed in double quotes, including the quotation marks,
# where the quoted string cannot contain any embedded double quotes
def t_STRING(t):
    r'"[^"]*\"'
    t.type = 'STRING'
    return t


# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    # Within the rule, the lineno attribute of the underlying lexer t.lexer is updated.
    # After the line number is updated, the token is simply discarded since nothing is returned.
    t.lexer.lineno += len(t.value)


# String preceeded by a #
def t_comment(t):
    r'\#.*'
    pass
    # No return value. Token discarded


# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'


# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Build the lexer (remove debug when presenting)
lexer = lex.lex()


# code = '''
# x = 1
# v = 0
# if (x == 1) then { v = x + 4 } else { x - 6 }
# '''


# Give the lexer some input
# lexer.input(code)


# Tokenize
while True:
    tok = lexer.token()
    if not tok:
        break  # No more input
    print(tok)


# Remove ambiguity from grammar
# youtube and https://my.eng.utah.edu/~cs3100/lectures/l14/ply-3.4/doc/ply.html
# PLUS      : level = 1,  assoc = 'left'
# MINUS     : level = 1,  assoc = 'left'
# TIMES     : level = 2,  assoc = 'left'
# DIVIDE    : level = 2,  assoc = 'left'
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('nonassoc', 'IF'),
    ('nonassoc', 'ELSE')
)


def p_abn(p):
    '''
    abn : statement
        | condition
        | expr
        | var_assign
        | empty
    '''
    # run
    print(run(p[1]))
    print(p[1])


def p_statement_if_else(p):
    '''
    statement : IF LPAREN condition RPAREN THEN LBRACE expr RBRACE ELSE LBRACE expr RBRACE
                | IF LPAREN condition RPAREN THEN LBRACE expr RBRACE ELSE LBRACE var_assign RBRACE
                | IF LPAREN condition RPAREN THEN LBRACE var_assign RBRACE ELSE LBRACE expr RBRACE
                | IF LPAREN condition RPAREN THEN LBRACE var_assign RBRACE ELSE LBRACE var_assign RBRACE
    '''
    p[0] = ('if-else', p[3], p[7], p[11])



def p_statement_if(p):
    '''
    statement : IF LPAREN condition RPAREN THEN expr
            | IF LPAREN condition RPAREN THEN var_assign
    '''
    p[0] = ('if', p[3], p[6])


def p_condition(p):
    '''
    condition : expr EQUALITY expr
                | expr GTHAN expr
                | expr GTHANEQ expr
                | expr LTHAN expr
                | expr LTHANEQ expr
    '''
    p[0] = (p[2], p[1], p[3])


def p_var_assign(p):
    '''
    var_assign : IDENTIFIER EQUALS expr
                | IDENTIFIER EQUALS STRING
    '''
    p[0] = ('=', p[1], p[3])


def p_expr_float_int(p):
    '''
    expr : INT
        | FLOAT
    '''
    p[0] = p[1]


def p_expr_var(p):
    '''
    expr : IDENTIFIER
    '''
    p[0] = ('var', p[1])


# POMDAS
def p_expr(p):
    '''
    expr : expr PLUS expr
        | expr MINUS expr
        | expr TIMES expr
        | expr DIVIDE expr
    '''
    p[0] = (p[2], p[1], p[3])


def p_expr_paren(p):
    'expr : LPAREN expr RPAREN'
    p[0] = p[2]


def p_empty(p):
    '''
    empty :
    '''
    p[0] = None


# Error rule for syntax errors
def p_error(p):
    if p:
        print("Syntax error in input at '%s'" % p.value)
    else:
        print("Syntax error at EOF")


parser = yacc.yacc()


# dictionary for variables
env = {}


def run(p):
    global env
    if type(p) == tuple:
        try:
            if p[0] == '+':
                return run(p[1]) + run(p[2])
            elif p[0] == '-':
                return run(p[1]) - run(p[2])
            elif p[0] == '*':
                return run(p[1]) * run(p[2])
            elif p[0] == '/':
                if p[2] == 0:
                    print("You can't divide using 0")
                else:
                    return run(p[1]) / run(p[2])
            elif p[0] == '==':
                return run(p[1]) == run(p[2])
            elif p[0] == '>':
                return run(p[1]) > run(p[2])
            elif p[0] == '>=':
                return run(p[1]) >= run(p[2])
            elif p[0] == '<':
                return run(p[1]) < run(p[2])
            elif p[0] == '<=':
                return run(p[1]) <= run(p[2])
            elif p[0] == '=':
                # assign value to key in dic value to identifier
                env[p[1]] = run(p[2])
                # print whole dictionary of variables
                # print(env)
            # if there is a var we retrieve it from dic to use in expr
            elif p[0] == 'var':
                # if variable is not in the dict tell user
                if p[1] not in env:
                    print("Undeclared variable")
                else:
                    return env[p[1]]
            elif p[0] == 'if-else':
                # print("This is p1", p[1])
                # print("This is p2", p[2])
                # print("This is p2", p[3])
                # print("This run(p[2])", run(p[1]))
                # print("This run(p[2])", run(p[1]))
                # print("This run(p[2])", run(p[1]))
                if bool(run(p[1])):
                    run(p[2])
                else:
                    run(p[3])
            elif p[0] == 'if':
                # print("This is p1", p[1])
                # print("This is p2", p[2])
                # print("This run(p[1])", run(p[1]))
                if bool(run(p[1])):
                    run(p[2])
        except TypeError as e:
            print("TypeError ", str(e))
    else:
        return p


# Test parser using code line 107
# parsed_code = parser.parse(code)
# print(parsed_code)
while True:
    try:
        # Accept user input, give interpreter look
        s = input('abn>> ')
    # end
    except EOFError:
        break
    if not s: continue
    result = parser.parse(s)
