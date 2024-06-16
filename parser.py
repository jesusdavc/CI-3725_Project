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
    ('left', 'TkNot'),
    ('left', 'TkTwoPoints'),
    ('left', 'TkPlus', 'TkMinus'),
    ('left', 'TkMult'),
    ('left', 'UMINUS'),
    ('left', 'TkEqual', 'TkNEqual'),
    ('left', 'TkComma'),
    ('left', 'TkFor'),
    ('left', 'TkDo'),
    ('left', 'TkIf'),
    ('left', 'TkGuard'),
    ('left', 'TkArrow'),
    ('left', 'TkIn'),
    ('left', 'TkTo'),
    ('left', 'TkAnd', 'TkOr'),
    ('left', 'TkLess', 'TkLeq', 'TkGeq', 'TkGreater'),
    ('left', 'TkOBracket', 'TkCBracket'),
    ('left', 'TkOpenPar', 'TkClosePar'),
    ('left', 'TkSoForth'),
    ('left', 'TkPrint'),
    ('left', 'TkString'),
    ('left', 'TkConcat'),
    ('left', 'TkId', 'TkNum'),
)

# Produccion para detectar un programa en GCL
def p_program(p):
    '''program : TkOBlock declare TkCBlock  
               | TkOBlock print TkCBlock'''
    p[0] = Block("Block", p[2], 0)
    print("block: "+ p[2].type)

# Produccion para detectar la expresion terminal declare
def p_expresion_declare(p):
    '''declare : TkDeclare expresion'''
    
    p[0] = Declare("declare", p[2])
    print("declare: " + p[2].type )

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
                 | string
                 | concat
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
    print("space: "+p[1].type +","+ p[2].type) 

def p_expresion_semicolon(p):
    '''semicolon : expresion TkSemicolon expresion
                 | print TkSemicolon expresion
                 | expresion TkSemicolon print''' 
    
    p[0] = Secuencia("Secuencia", p[1],p[3])
    print("semicolon nivel: "+p[1].type + ";" + p[3].type)

def p_expresion_asig(p):
    '''asig : expresion TkAsig expresion
            | expresion TkAsig reserved'''
    
    p[0] = Asignation("Asignacion: ", p[1], p[3])
    print("asignacion nivel: "+p[1].type +","+str(p[3].type))

# Produccion para detectar un valor negativo
def p_expresion_uminus(p):
    '''negative : TkMinus number %prec UMINUS
                | TkMinus word %prec UMINUS
                | TkMinus TkOpenPar aritmetic TkClosePar %prec UMINUS'''
    p[0] = Aritmetic("UMINUS",p[2])

# Produccion para detectar aritmetica
def p_expresion_aritmetic(p):
    '''aritmetic : expresion TkPlus expresion
                | expresion TkMinus expresion
                | expresion TkMult expresion'''
    
    if(p[2] == '+'):
        p[0] = Aritmetic("Plus", p[1], p[3])
    elif(p[2] == '-'):
        p[0] = Aritmetic("Minus", p[1], p[3])
    else:
        p[0] = Aritmetic("Mult", p[1], p[3])
    
    print("Aritmetica: "+p[1].type +","+ p[3].type)

# Produccion para detectar la expresion no terminal TwoPoints
def p_expresion_two_point(p):
    '''twoPoints : expresion TkTwoPoints expresion
                 | expresion TkTwoPoints reserved
                 | number TkTwoPoints expresion'''
    
    p[0] = TwoPoints("TwoPoints", p[1], p[3])
    print("dos puntos nivel: "+p[1].type +","+ p[3].type)

# Produccion para detectar la expresion no terminal Comma
def p_expresion_comma(p):
    "comma : expresion TkComma expresion"

    p[0] = Comma("Comma", p[1], p[3])
    print("Coma nivel: "+p[1].type +","+ p[3].type) 

def p_reserved(p):
    '''reserved : TkInt
                | TkBool
                | TkTrue
                | TkFalse
                | TkArray
                | TkSkip'''
    
    if (p[1] == 'array'):
        p[0] = Reserved("array", value = p[1])
        print("Array: "+ p[1])
    elif (p[1] == 'int'):
        p[0] = Reserved("int", value = p[1])
        print("Reservado TkInt: "+ p[1])
    elif (p[1] == 'true'):
        p[0] = Reserved("Literal:", value = p[1])
        print("Reservado true: "+ p[1])
    elif (p[1] == 'false'):
        p[0] = Reserved("Literal:", value = p[1])
        print("Reservado false: "+ p[1])
    else:
        p[0] = Reserved("Literal:", value = p[1])
        print("Reservado bool: "+ p[1])

# Produccion del ! 
def p_not(p):
    '''not : TkNot not
           | TkNot word
           | TkNot TkOpenPar proposition TkClosePar'''
    
    if (p[1] == '!'):
        p[0] = Not("Not: ", p[2])
        print("Not: "+p[2].type)
    else:
        p[0] = Not("Not: ", p[3])
        print("Not: "+p[1].type)

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
                   | number
                   | readArray
                   | reserved
                   | word'''
    
    if(len(p) > 2):
        if (p[2] == '/\\'):
            p[0] = Condition("And: ", p[1], p[3])
            print("And: "+p[1].type +","+ p[3].type)
        elif (p[2] == '\\/'):
            p[0] = Condition("Or: ", p[1], p[3])
            print("Or: "+p[1].type +","+ p[3].type)
        elif (p[2] == '<'):
            p[0] = Condition("Less: ", p[1], p[3])
            print("Less: "+p[1].type +","+ p[3].type)
        elif (p[2] == '<='):
            p[0] = Condition("Leq: ", p[1], p[3])
            print("Leq: "+p[1].type +","+ p[3].type)
        elif (p[2] == '>='):
            p[0] = Condition("Geq: ", p[1], p[3])
            print("Geq: "+p[1].type +","+ p[3].type)
        elif (p[2] == '>'):
            p[0] = Condition("Greater: ", p[1], p[3])
            print("Greater: "+p[1].type +","+ p[3].type)
        elif (p[2] == '=='):
            p[0] = Condition("Equal: ", p[1], p[3])
            print("Equal: "+p[1].type +","+ p[3].type)
        elif (p[2] == '!='):
            p[0] = Condition("NEqual: ", p[1], p[3])
            print("NEqual: "+p[1].type +","+ p[3].type)
        elif(p[1] == '('):
            p[0] = p[2]
            print("Parentesis: "+p[2].type)
    else:
        p[0] = p[1]
        print("Reserved: "+p[1].type)

# Produccion para leer un array
def p_read_array(p):
    '''readArray : reserved TkOBracket soForth TkCBracket
                 | reserved TkOBracket number TkCBracket
                 | word TkOBracket word TkCBracket
                 | word TkOBracket number TkCBracket
                 | word TkOBracket aritmetic TkCBracket
                 | writeArray TkOBracket number TkCBracket
                 | writeArray TkOBracket word TkCBracket
                 | writeArray TkOBracket aritmetic TkCBracket
                 | readArray TkOBracket number TkCBracket
                 | readArray TkOBracket word TkCBracket
                 | readArray TkOBracket aritmetic TkCBracket'''
    
    p[0] = ReadArray("ReadArray", p[1], p[3])
    print("ReadArray x[]: "+ str(p[1].type) +".."+ str(p[3].type))

# Produccion para escribir un array
def p_write_array(p):
    '''writeArray : word TkOpenPar expresion TkClosePar
                  | TkOpenPar expresion TkClosePar'''
    
    if (len(p) > 4):
        p[0] = WriteArray("WriteArray", p[1], p[3])
        print("Write x(): "+ str(p[1].type) +".."+ str(p[3].type))
    else:
        p[0] = WriteArray("WriteArray", p[2])
        print("Write (): "+ str(p[2].type) )

# Produccion para detectar la expresion no terminal SoForth
def p_expresion_so_forth(p):
    "soForth : number TkSoForth number"

    p[0] = TwoSoFort("TkSoForth: ", p[1], p[3])
    print(".. nivel: "+p[1].type +","+ p[3].type) 

# Produccion para detectar el bucle for   
def p_for(p):
    "for : TkFor in TkArrow expresion TkRof"
    p[0] = Loop_For("For: ", p[2], p[4])
    print("For: "+p[2].type +","+ p[4].type) 

# Produccion para detectar condicion In
def p_expresion_in(p):
    '''in : number TkIn to
          | word TkIn to
          | readArray TkIn to '''

    p[0] = Loop_For("In ", p[1], p[3])
    print("In: "+p[1].type +","+ p[3].type) 

# Produccion para detectar condicion To
def p_expresion_to(p):
    '''to :  expresion TkTo expresion'''
    
    p[0] = Loop_For("To ", p[1], p[3])
    print("To: "+p[1].type +","+ p[3].type)
    
# Produccion para detectar el bucle do
def p_do(p):
    "do : TkDo guard TkOd"
    p[0] = Loop_Do("Do: ", p[2])
    print("Do: "+p[2].type) 

# Produccion para detectar el if
def p_if(p):
    "if : TkIf guard TkFi"
    print(p)
    p[0] = Condition_If("If", p[2])
    print("If: "+p[2].type)

# Produccion que detecta las guardas
def p_guard(p):
    ''' guard : arrow TkGuard guard
              | arrow'''
    if(len(p) > 2):
        p[0] = Guard("Guard", p[1], p[3])
        print("Guard: "+p[1].type +","+ p[3].type)
    else:
        p[0] = p[1]
        print("Guard: "+p[1].type)

# Produccion para detectar -->
def p_arrow(p):
    '''arrow : proposition TkArrow expresion
             | reserved TkArrow expresion'''

    p[0] = Arrow("Then", p[1], p[3])
    print("Arrow: "+p[1].type +","+ p[3].type)

# Produccion para detectar la expresion no terminal Concat
def p_expresion_concat(p):
    "concat : expresion TkConcat expresion"
    
    p[0] = Concat("Concat: ", p[1], p[3])
    print("Concatenar nivel: "+p[1].type +","+ p[3].type) 

# Produccion para detectar un print
def p_expresion_print(p):
    '''print : TkPrint string
             | TkPrint concat'''
    p[0] = Print("Print", p[2])
    print("Print: "+ p[2].type)

# Produccion para detectar la expresion terminal de un identificador
def p_expresion_string(p):
    "string : TkString"
    p[0] = Atom("String: ", p[1])
    print("String: "+ p[1])  

# Produccion para detectar la expresion terminal de un numero
def p_number(p):
    "number : TkNum"
    p[0] = Atom("Number: ",p[1])
    print("Numero: "+  str(p[1]))

# Produccion para detectar la expresion terminal de un identificador
def p_expresion_id(p):
    "word : TkId"
    p[0] = Atom("Ident: ", p[1])
    print("Ident: "+ p[1])

# Produccion para detectar la expresion terminal vacia o letra
def p_expresion_empty(p):
    "empty :" 
    p[0] = Atom("Empty" )
    print("Vacio: "+ p[0].type) 

# Produccion para detectar la expresion comentario
def p_coment(p):
    '''expresion : TkComent'''
    pass

# Manejador de errores 
def p_error(p):
    print(p)
    print("Syntax error in input!")
    pass

#Inicializador del parser
parser = yacc.yacc()

#Clase para la creacion de nodos, con el fin de generar el arbol AST
class Atom:

    def __init__(self, type, value=None):
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
        #res = "Estoy en ATOM DQ, valor:"+ str(self.value) 
        #print(res)
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
        #res = "Estoy en ATOM DQ, valor:"+ str(self.value) 
        #print(res)
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
        #status(self)
        pila.append(self.left)
        #print(pila)
        #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
        if(self.right.type == "Secuencia"):
            #print("Resulto ser secuencia")
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)
        #print(len(pila))
        #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
        #for x in pila: print(x.type)
        #print(pila)
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
        #print("Este es el tipo del hijo derecho SECUENCIA DQ :"+str(self.right.type))
        #print("Este es el tipo del hijo izquierdo SECUENCIA DQ:"+str(self.left.type))
        if (self.right.type == "Secuencia"):
            #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)
        #print("pila en el nivel DQ "+ str(self.type)+": "+ str(pila))
        return pila
    
class TwoPoints:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        #status(self)
        if (self.right.type == "Space"):
            #print("Estoy en AST TwoPoint QD")
            pila = deque()
            AST = "-"*level
            pila += self.left.print_AST_DQ()
            pila.append(":")
            pila += self.right.left.print_AST_DQ()
            AST += " ".join(pila)
            print(AST)
            self.right.right.print_AST(level)
        elif (self.right.value == "int" or self.right.value == "bool" 
            or self.right.value == "array"): 
            #print("Estoy en AST TwoPoint QD")
            pila = deque()
            AST = "-"*level
            pila += self.left.print_AST_DQ()
            pila.append(":")
            pila += self.right.print_AST_DQ()
            AST += " ".join(pila)
            print(AST)
        else:
            #print("ESTOY EN ELSE")
            AST = "-"*level+self.type
            print(AST)
            self.left.print_AST(level+1)
            self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        #status(self)
        #print("Estoy en Twopoints")
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append(":")
        pila += self.right.print_AST_DQ()
        #print(pila)
        return pila

class Asignation:

    def __init__(self, type, left=None, right=None, value=0):
        self.type = type
        self.left = left
        self.right = right
        self.value = value

    def print_AST(self, level=0):
        AST = "-"*level + self.type
        #status(self)
        print(AST)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        #status(self)
        pila = deque()
        pila.append(self.left)
        if (self.right.type == "Secuencia"):
            #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
            pila += self.right.print_AST_DQ(level+1)
        #print("pila en el nivel DQ "+ str(self.type)+": "+ str(pila))
        return pila


class Space_Declare:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        
        #status(self)
        self.left.print_AST(level)
        if (self.right.type == "Secuencia"):
            self.right.print_AST(level+1)
        else:
            self.right.print_AST(level)

    def print_AST_DQ(self,level=0):
        pila = deque()
        #self.left.level = level
        pila.append(self.left)
        
        #status(self)
        if (self.right.type == "Secuencia"):
            #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
            pila += self.right.print_AST_DQ(level+1)
        #print("pila en el nivel DQ "+ str(self.type)+": "+ str(pila))
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
        #print("Estoy en AST ARRAY DQ")
        #print(self.value)
        #status(self)
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
        #print("Estoy en AST ARRAY DQ")
        #print(self.value)
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
        #print("Estoy en AST ARRAY DQ")
        #print(self.value)
        #status(self)
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
            #status(self)
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
        #print("Estoy en AST ARRAY DQ")
        #print(self.value)
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
            status(self)
            ret+= " ".join(self.print_AST_DQ())
            print(ret)
        else:
            ret = "-"*level + self.type
            print(ret)
            #print("Estoy en AST READ")
            #status(self)
            self.left.print_AST(level+1)
            self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        #print("Estoy en AST DQ READ")
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append("[")
        pila += self.right.print_AST_DQ()
        pila.append("]")
        #print(pila)
        return pila
    
class WriteArray:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        #status(self)
        #print("Estoy en AST READ")
        ret = "-"*level + self.type+":"
        print(ret)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

    def print_AST_DQ(self, level=0):
        #print("Estoy en AST DQ READ")
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
        #res = "Estoy en AST FOR, el hijo izquierdo es: "+ str(self.left)
        #print(res) 
        #res = "Estoy en AST FOR, el hijo derecho es: "+ str(self.right)
        #print(res)
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
        #status(self)
        pila.append(self.left)
        #print(pila)
        #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
        if(self.right.type == "Guard"):
            #print("Resulto ser secuencia")
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)
        #print(len(pila))
        #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
        #for x in pila: print(x.type)
            
        while(len(pila)>0):
            x = pila.popleft()
            x.print_AST(level+1)

    def print_AST_DQ(self,level=0):
        pila = deque()
        ret = "-"*level + self.type 
        print(ret)
        pila.append(self.left)
        #print("Este es el tipo del hijo derecho SECUENCIA DQ :"+str(self.right.type))
        #print("Este es el tipo del hijo izquierdo SECUENCIA DQ:"+str(self.left.type))
        if (self.right.type == "Guard"):
            #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)
        #print("pila en el nivel DQ "+ str(self.type)+": "+ str(pila))
        return pila

class Arrow:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        #res = "Estoy en AST FOR, el hijo izquierdo es: "+ str(self.left)
        #print(res) 
        #res = "Estoy en AST FOR, el hijo derecho es: "+ str(self.right)
        #print(res)
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
        #res = "Estoy en AST FOR, el hijo izquierdo es: "+ str(self.left)
        #print(res) 
        #res = "Estoy en AST FOR, el hijo derecho es: "+ str(self.right)
        #print(res)
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
        #res = "Estoy en AST FOR, el hijo izquierdo es: "+ str(self.left)
        #print(res) 
        #res = "Estoy en AST FOR, el hijo derecho es: "+ str(self.right)
        #print(res)
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
        #res = "Estoy en AST FOR, el hijo izquierdo es: "+ str(self.left)
        #print(res) 
        #res = "Estoy en AST FOR, el hijo derecho es: "+ str(self.right)
        #print(res)
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
        #status(self)
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append("..")
        pila += self.right.print_AST_DQ()
        #print(pila)
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
        #print("Estoy en Transicio con type:")
        #print(self.type)
        #print("Estoy en Transicio con hijo type:")
        #print(self.children.type)
        if(self.type == "Print"):
            AST = "-"*level + self.type
            print(AST)

            self.children.print_AST(level+1)
        else:
            self.children.print_AST(level)

    def print_AST_DQ(self):
        return self.children.print_AST_DQ()
    
def status(x):
        print("----------------------------")
        print(x.type)
        print("Este es el tipo del hijo izquierdo :"+str(x.left.type))
        print("Este es el tipo del hijo derecho :"+str(x.right.type))
        #print(x.level)
        print("----------------------------")
while True:
    f = open(sys.argv[1], "r")   
    assert f.name.endswith('.gcl') # Verifica que sea un .gcl
    content = ' '.join(f.readlines())
    f.close()
    print(content)
    result = parser.parse(content)
    print(f"Estos son los resultados:\n {result}")
    result.print_AST()
    break   