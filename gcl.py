# ------------------------------------------------------------
# parser.py
# Generador de arbol AST para el lenguaje GCL
# Carnet: 15-10345 y 19-10211
# ------------------------------------------------------------
import ply.lex as lex
import ply.yacc as yacc
import sys
from lexer import tokens, reserved
from collections import deque
if ".." not in sys.path: sys.path.insert(0,"..") # type: ignore

# Precedencia de algunas expresiones de GCL
precedence = (
    ('left', 'TkSemicolon'),
    ('left', 'TkAsig'),
    ('left', 'TkTwoPoints'),
    ('left', 'TkPlus', 'TkMinus'),
    ('left', 'TkMult'),
    ('left', 'UMINUS'),
    ('left', 'TkComma'),
    ('left', 'TkFor'),
    ('left', 'TkDo'),
    ('left', 'TkIf'),
    ('left', 'TkGuard'),
    ('left', 'TkArrow'),
    ('left', 'TkIn'),
    ('left', 'TkTo'),
    ('left', 'TkOr'),
    ('left', 'TkAnd'),
    ('left', 'TkNot'),
    ('left', 'TkEqual', 'TkNEqual'),
    ('left', 'TkLess', 'TkLeq', 'TkGeq', 'TkGreater'),
    ('left', 'TkOBracket', 'TkCBracket'),
    ('left', 'TkOpenPar', 'TkClosePar'),
    ('left', 'TkSoForth'),
    ('left', 'TkPrint'),
    ('left', 'TkConcat'),
    ('left', 'TkString'),
    ('left', 'TkId', 'TkNum'),
)

# Produccion para detectar un programa en GCL
def p_program(p):
    '''program : TkOBlock declare TkCBlock  
               | TkOBlock expresion TkCBlock'''
    p[0] = Block("Block", p[2], 0)


# Produccion para detectar la expresion terminal declare
def p_expresion_declare(p):
    '''declare : TkDeclare expresion'''
    
    p[0] = Declare("declare", p[2])


# Produccion para detectar la expresion las secuencias del programa
def p_expresion(p):
    '''expresion : semicolon
                 | asig
                 | negative
                 | twoPoints
                 | aritmetic
                 | comma
                 | space
                 | for
                 | do
                 | if
                 | print
                 | word
                 | proposition
                 | readArray
                 | writeArray
                 | soForth
                 | program
                 | empty'''
    
    p[0] = p[1]

def p_expresion_space_empty(p):
    '''space : twoPoints expresion
             | twoPoints empty
             | readArray expresion
             | twoPoints print
             | readArray print'''
    p[0] = Space_Declare("Space", p[1], p[2])


def p_expresion_semicolon(p):
    '''semicolon : expresion TkSemicolon expresion''' 
    
    p[0] = Secuencia("Secuencia", p[1],p[3])


def p_expresion_asig(p):
    '''asig : expresion TkAsig expresion
            | expresion TkAsig reserved'''
    
    p[0] = Asignation("Asignacion: ", p[1], p[3])


# Produccion para detectar un valor negativo
def p_expresion_uminus(p):
    '''negative : TkMinus number %prec UMINUS
                | TkMinus readArray %prec UMINUS
                | TkMinus word %prec UMINUS
                | TkMinus TkOpenPar aritmetic TkClosePar %prec UMINUS'''
    p[0] = Aritmetic("UMINUS",p[2])

# Produccion para detectar aritmetica
def p_expresion_aritmetic(p):
    '''aritmetic : aritmetic TkPlus aritmetic
                | aritmetic TkMinus aritmetic
                | aritmetic TkMult aritmetic
                | TkOpenPar aritmetic TkClosePar
                | negative
                | number
                | readArray
                | word'''
    if(len(p) > 2 and p[1] != '('):
        if(p[2] == '+'):
            p[0] = Aritmetic("Plus", p[1], p[3])
        elif(p[2] == '-'):
            p[0] = Aritmetic("Minus", p[1], p[3])
        elif(p[2] == '*'):
            p[0] = Aritmetic("Mult", p[1], p[3])
    elif p[1] == '(':
            p[0] = p[2]
    else: 
        p[0] = p[1]

# Produccion para detectar la expresion no terminal TwoPoints
def p_expresion_two_point(p):
    '''twoPoints : expresion TkTwoPoints expresion
                 | expresion TkTwoPoints reserved
                 | number TkTwoPoints expresion'''
    
    p[0] = TwoPoints("TwoPoints", p[1], p[3])


# Produccion para detectar la expresion no terminal Comma
def p_expresion_comma(p):
    "comma : expresion TkComma expresion"
    p[0] = Comma("Comma", p[1], p[3])

def p_reserved(p):
    '''reserved : TkInt
                | TkBool
                | TkTrue
                | TkFalse
                | TkArray
                | TkSkip'''
    
    if (p[1] == 'array'):
        p[0] = Reserved("array", value = p[1])
    elif (p[1] == 'int'):
        p[0] = Reserved("int", value = p[1])
    elif (p[1] == 'true'):
        p[0] = Reserved("Literal:", value = p[1])
    elif (p[1] == 'false'):
        p[0] = Reserved("Literal:", value = p[1])
    else:
        p[0] = Reserved("Literal:", value = p[1])


# Produccion del ! 
def p_not(p):
    '''not : TkNot not
           | TkNot word
           | TkNot proposition
           | TkNot TkOpenPar proposition TkClosePar'''
    
    if (p[1] == '!'):
        p[0] = Not("Not: ", p[2])
    else:
        p[0] = Not("Not: ", p[3])

# Produccion para detectar condiciones bool
def p_proposition(p):
    '''proposition : proposition TkAnd      proposition
                   | proposition TkOr       proposition
                   | proposition TkLess     proposition
                   | proposition TkLeq      proposition
                   | proposition TkGeq      proposition
                   | proposition TkGreater  proposition
                   | proposition TkEqual    proposition
                   | proposition TkNEqual   proposition
                   | TkOpenPar proposition TkClosePar
                   | not
                   | aritmetic
                   | number
                   | readArray
                   | reserved
                   | word'''
    
    if(len(p) > 2):
        if (p[2] == '/\\'):
            p[0] = Condition("And: ", p[1], p[3])
        elif (p[2] == '\\/'):
            p[0] = Condition("Or: ", p[1], p[3])
        elif (p[2] == '<'):
            p[0] = Condition("Less: ", p[1], p[3])
        elif (p[2] == '<='):
            p[0] = Condition("Leq: ", p[1], p[3])
        elif (p[2] == '>='):
            p[0] = Condition("Geq: ", p[1], p[3])
        elif (p[2] == '>'):
            p[0] = Condition("Greater: ", p[1], p[3])
        elif (p[2] == '=='):
            p[0] = Condition("Equal: ", p[1], p[3])
        elif (p[2] == '!='):
            p[0] = Condition("NEqual: ", p[1], p[3])
        elif(p[1] == '('):
            p[0] = p[2]
    else:
        p[0] = p[1]


# Produccion para leer un array
def p_read_array(p):
    '''readArray : reserved TkOBracket soForth TkCBracket
                 | reserved TkOBracket number TkCBracket
                 | reserved TkOBracket negative TkCBracket
                 | word TkOBracket word TkCBracket
                 | word TkOBracket number TkCBracket
                 | word TkOBracket negative TkCBracket
                 | word TkOBracket aritmetic TkCBracket
                 | writeArray TkOBracket number TkCBracket
                 | writeArray TkOBracket word TkCBracket
                 | writeArray TkOBracket aritmetic TkCBracket
                 | writeArray TkOBracket negative TkCBracket
                 | readArray TkOBracket number TkCBracket
                 | readArray TkOBracket word TkCBracket
                 | readArray TkOBracket aritmetic TkCBracket
                 | readArray TkOBracket negative TkCBracket'''
    
    p[0] = ReadArray("ReadArray", p[1], p[3])


# Produccion para escribir un array
def p_write_array(p):
    '''writeArray : word TkOpenPar expresion TkClosePar
                  | TkOpenPar expresion TkClosePar'''
    
    if (len(p) > 4):
        p[0] = WriteArray("WriteArray", p[1], p[3])
    else:
        p[0] = WriteArray("WriteArray", p[2])


# Produccion para detectar la expresion no terminal SoForth
def p_expresion_so_forth(p):
    '''soForth : number TkSoForth number
               | word TkSoForth word
               | number TkSoForth word
               | word TkSoForth number
               | number TkSoForth negative
               | word TkSoForth negative
               | negative TkSoForth number
               | negative TkSoForth word
               | negative TkSoForth negative'''

    p[0] = TwoSoFort("TkSoForth: ", p[1], p[3])


# Produccion para detectar el bucle for   
def p_for(p):
    "for : TkFor in TkArrow expresion TkRof"
    p[0] = Loop_For("For: ", p[2], p[4])


# Produccion para detectar condicion In
def p_expresion_in(p):
    '''in : number TkIn to
          | negative TkIn to
          | word TkIn to
          | readArray TkIn to '''

    p[0] = Loop_For("In ", p[1], p[3])


# Produccion para detectar condicion To
def p_expresion_to(p):
    '''to :  expresion TkTo expresion'''
    
    p[0] = Loop_For("To ", p[1], p[3])

    
# Produccion para detectar el bucle do
def p_do(p):
    "do : TkDo guard TkOd"
    p[0] = Loop_Do("Do: ", p[2])


# Produccion para detectar el if
def p_if(p):
    "if : TkIf guard TkFi"
    print(p)
    p[0] = Condition_If("If", p[2])


# Produccion que detecta las guardas
def p_guard(p):
    ''' guard : arrow TkGuard guard
              | arrow'''
    if(len(p) > 2):
        p[0] = Guard("Guard", p[1], p[3])
    else:
        p[0] = p[1]


# Produccion para detectar -->
def p_arrow(p):
    '''arrow : proposition TkArrow expresion
             | reserved TkArrow expresion'''
    p[0] = Arrow("Then", p[1], p[3])

# Produccion para detectar un print
def p_expresion_print(p):
    '''print : TkPrint sentence'''

    p[0] = Print("Print", p[2])

# Produccion para detectar la expresion terminal de un identificador
def p_expresion_sentence(p):
    '''sentence : sentence TkConcat sentence
                | readArray
                | writeArray
                | word
                | number
                | string'''
    if len(p) > 2:
        p[0] = Concat("Concat: ", p[1], p[3])
    else:
        p[0] = Transicion("", p[1])


# Produccion para detectar la expresion terminal una palabra
def p_string(p):
    "string : TkString"
    p[0] = Atom("String: ",p[1])


# Produccion para detectar la expresion terminal de un numero
def p_number(p):
    "number : TkNum"
    p[0] = Atom("Number: ",p[1])


# Produccion para detectar la expresion terminal de un identificador
def p_expresion_id(p):
    "word : TkId"
    p[0] = Atom("Ident: ", p[1])


# Produccion para detectar la expresion terminal vacia o letra
def p_expresion_empty(p):
    "empty :" 
    p[0] = Atom("Empty" )


# Produccion para detectar la expresion comentario
def p_coment(p):
    '''expresion : TkComent'''
    pass

# Manejador de errores 
def p_error(p):
    print(f"Sintax error in row {p.lineno}")
    pass

#Inicializador del parser
parser = yacc.yacc()

#Clase para la creacion de nodos, con el fin de generar el arbol AST
class Atom:

    def __init__(self, type,value=None):
        self.type = type
        self.value = str(value)
       

    def print_AST(self, level=0):

        if (self.type == "Empty"):
            AST =""
        elif(self.value == None):
            AST = "-"*level + self.type
        else:
            AST = "-"*level + self.type +self.value
        print(AST)
      

    def print_AST_DQ(self, level=0):
        pila = deque()
        pila.append(self.value)
        return pila

class Reserved:

    def __init__(self, type, left=None, right=None, value=None):
        self.type = type
        self.value = value
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        if(self.value == None):
            AST = "-"*level + self.type
        else:
            AST = "-"*level + self.type +self.value
        print(AST)
    
    def print_AST_DQ(self):
        '''if (self.type == "Read"):
            pila = deque()
            pila.append('array')
            pila += self.rigth.print_AST_DQ()
            return pila
        else:'''
        pila = deque()
        pila.append(self.value)
        return pila

class Secuencia:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        pila = deque()
        ret = "-"*level + self.type 
        print(ret)
        pila.append(self.left)
        if(self.right.type == "Secuencia"):
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)
        while(len(pila)>0):
            x = pila.popleft()
            if x is None:
                continue
            x.print_AST(level+1)

    def print_AST_DQ(self,level=0):
        pila = deque()
        ret = "-"*level + self.type 
        print(ret)
        pila.append(self.left)
        if (self.right.type == "Secuencia"):
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)
        return pila
    
class TwoPoints:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        if (self.right.type == "Space"):
            pila = deque()
            AST = "-"*level
            pila += self.left.print_AST_DQ()
            pila.append(":")
            pila += self.right.left.print_AST_DQ()
            AST += " ".join(pila)
            print(AST)
            self.right.right.print_AST(level)
        elif (self.right.type == "ReadArray" or self.right.value == "int" or self.right.value == "bool" 
            or self.right.value == "array"): 
            pila = deque()
            AST = "-"*level
            pila += self.left.print_AST_DQ()
            pila.append(":")
            pila += self.right.print_AST_DQ()
            AST += " ".join(pila)
            print(AST)
        else:
            AST = "-"*level+self.type
            print(AST)
            self.left.print_AST(level+1)
            self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append(":")
        pila += self.right.print_AST_DQ()
        return pila

class Asignation:

    def __init__(self, type, left=None, right=None, value=0):
        self.type = type
        self.left = left
        self.right = right
        self.value = value

    def print_AST(self, level=0):
        AST = "-"*level + self.type
        print(AST)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        pila = deque()
        pila.append(self.left)
        if (self.right.type == "Secuencia"):
            pila += self.right.print_AST_DQ(level+1)
        return pila


class Space_Declare:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        
        self.left.print_AST(level)
        if (self.right.type == "Secuencia"):
            self.right.print_AST(level+1)
        else:
            self.right.print_AST(level)

    def print_AST_DQ(self,level=0):
        pila = deque()
        pila.append(self.left)
        
        if (self.right.type == "Secuencia"):
            pila += self.right.print_AST_DQ(level+1)
        return pila

class Comma:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        AST = "-"*level + self.type
        print(AST)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append(",")
        pila += self.right.print_AST_DQ()
        return pila  
    
class Concat:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        AST = "-"*level + self.type
        print(AST)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        pila = deque()
        pila += self.left.print_AST_DQ(level)
        pila.append(".")
        pila += self.right.print_AST_DQ(level)
        return pila
     
class Aritmetic:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        if (self.type == "UMINUS"):
            AST = "-"*level + 'Minus'
            print(AST)
            self.left.print_AST(level+1)
        else:
            AST = "-"*level + self.type
            print(AST)
            self.left.print_AST(level+1)
            self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        if (self.type == "UMINUS"):
            pila = deque()
            pila.append("-")
            pila += self.left.print_AST_DQ()
            return pila
        else:
            pila = deque()
            pila += self.left.print_AST_DQ()
            pila.append(",")
            pila += self.right.print_AST_DQ()
            return pila
  
class Print:  
    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        if self is not None:
            if(self.right is not None):
                pila = deque()
                pila += self.left.print_AST_DQ(level+1)
                level+= len(pila)
                AST = "-"*level + self.type
                print(AST)
                print(pila)
                while(len(pila)>0):
                    x = pila.popleft()
                    x.print_AST(level+1)
            else:
                AST = "-"*level + self.type
                print(AST)
                self.left.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        pila = deque()
        if self.left is not None:
            pila += self.left.print_AST_DQ(level)
        if self.right is not None:
            pila += self.right.print_AST_DQ(level)
        return pila  
      
class Array:

    def __init__(self, type, value=0, right=None):
        self.type = type
        self.right = right
        self.value = value

    def print_AST_DQ(self, level=0):
        pila = deque()
        pila.append(self.value)
        pila += self.right.print_AST_DQ(level)
        return pila

    
class ReadArray:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        if (self.left.type == "TwoPoints"):
            ret = "-"*level
            ret+= " ".join(self.print_AST_DQ())
            print(ret)
        else:
            ret = "-"*level + self.type
            print(ret)
            self.left.print_AST(level+1)
            self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append("[")
        pila += self.right.print_AST_DQ()
        pila.append("]")
        return pila
    
class WriteArray:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        ret = "-"*level + self.type+":"
        print(ret)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        pila = deque()
        pila.append("(")
        pila += self.left.print_AST_DQ(level)
        pila.append(")")
        return pila
class Not:

    def __init__(self, type, children):
        self.type = type
        self.children = children

    def print_AST(self, level=0):
        ret = "-"*level + self.type
        print(ret)
        self.children.print_AST(level+1)
        
class Condition_If:

    def __init__(self,type, children = None,level = 0 ):
        self.type = type
        self.level = level
        self.children = children

    def print_AST(self, level=0):
        ret = "-"*level + self.type 
        print(ret)
        self.children.print_AST(level+1)
    

class Guard:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        pila = deque()
        ret = "-"*level + self.type 
        print(ret)
        pila.append(self.left)
        if(self.right.type == "Guard"):
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)

            
        while(len(pila)>0):
            x = pila.popleft()
            x.print_AST(level+1)

    def print_AST_DQ(self,level=0):
        pila = deque()
        ret = "-"*level + self.type 
        print(ret)
        pila.append(self.left)
        if (self.right.type == "Guard"):
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)
        return pila

class Arrow:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

class Condition:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)
class Loop_For:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

class Loop_Do:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1)

  
class TwoSoFort:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        ret = "-"*level + self.type+":"
        print(ret)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

    def print_AST_DQ(self):
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append("..")
        pila += self.right.print_AST_DQ()
        return pila

class Declare:

    def __init__(self, type, children = None,level = 0 ):
        self.type = type
        self.level = level
        self.children = children

    def print_AST(self, level=0):
        ret = "-"*level + self.type
        print(ret)
        self.children.print_AST(level+1) 

class Block:

    def __init__(self,type, children = None,level = 0 ):
        self.type = type
        self.level = level
        self.children = children

    def print_AST(self, level=0):
        ret = "-"*level + self.type 
        print(ret)
        self.children.print_AST(level+1)

class Transicion:

    def __init__(self, type, children = None,level = 0, value=0):
        self.type = type
        self.level = level
        self.children = children
        self.value = value

    def print_AST(self, level=0):
        if(self.type == "Print"):
            AST = "-"*level + self.type
            print(AST)

            self.children.print_AST(level+1)
        else:
            self.children.print_AST(level)

    def print_AST_DQ(self):
        return self.children.print_AST_DQ()

#Clase para manejo de errores.     
class SyntaxErrorException(Exception):
    def __init__(self, message, lineno):
        super().__init__(message)
        self.lineno = lineno

    def __str__(self):
        return f"{self.args[0]} (line {self.lineno})"
    
if __name__ == "__main__":
    try:
        if len(sys.argv) != 2:
            raise ValueError("Debe proporcionar el nombre del archivo a analizar.")
        nombre_archivo = sys.argv[1]
        with open(nombre_archivo, "r") as f:
            assert f.name.endswith('.gcl') # Verifica que sea un .gcl
            contenido = ' '.join(f.readlines())
        result = parser.parse(contenido)
        result.print_AST()

    except:
        if f.name.endswith('.gcl') == False: # Verifica que sea un .gcl
            print("Archivo no encontrado o no es de extensión .gcl, indique un archivo para analizar")
        else:
            pass