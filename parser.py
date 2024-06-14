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
    ('left', 'TkTwoPoints'),
    ('left', 'TkComma'),
    ('rigth', 'TkAsig'),
    ('rigth', 'TkSemicolon'),
)

# Produccion para detectar un programa en GCL
def p_program(p):
    "program : TkOBlock declare TkCBlock"
    p[0] = Block("Block", p[2], 0)
    print("block: "+ p[2].type)

# Produccion para detectar la expresion terminal declare
def p_expresion_declare(p):
    '''declare : TkDeclare expresion'''
    
    p[0] = Declare("declare", p[2])
    print("declare: " + p[2].type )

# Produccion para detectar la expresion las secuencias del programa
def p_expresion_semicolon(p):
    '''expresion : expresion TkSemicolon expresion
                 | expresion TkAsig expresion
                 | expresion TkAsig number
                 | soForth
                 | twoPoints
                 | comma 
                 | space
                 | Word
                 | number
                 | Empty '''
    
    if len(p) > 2:
        if (p[2] == ':='):
            p[0] = Asignation("Asignacion: ", p[1], p[3])
            print("asignacion nivel: "+p[1].type +","+str(p[3].type))

        elif (p[2] == ';'):
            p[0] = Secuencia("Secuencia", p[1],p[3])
            print("semicolon nivel: "+p[1].type + ";" + p[3].type)
    else:
        p[0] = toAtom("RA",p[1])


# Produccion para detectar la expresion no terminal TwoPoints
def p_expresion_two_point(p):
    '''twoPoints : expresion TkTwoPoints expresion
                 | expresion TkTwoPoints reserved
                 | number TkTwoPoints expresion'''
    
    p[0] = TwoPoints("TwoPoints: ", p[1], p[3])
    print("dos puntos nivel: "+p[1].type +","+ p[3].type) 

# Produccion para detectar la expresion no terminal Comma
def p_expresion_comma(p):
    '''comma : expresion TkComma expresion'''
    
    p[0] = Comma("Comma: ", p[1], p[3])
    print("Coma nivel: "+p[1].type +","+ p[3].type) 

# Produccion para detectar la expresion no terminal SoForth
def p_expresion_so_forth(p):
    '''soForth : number TkSoForth number'''

    p[0] = TwoSoFort("TkSoForth: ", p[1], p[3])
    print(".. nivel: "+p[1].type +","+ p[3].type) 

# Produccion para detectar el especio vacio en la declaracion
def p_expresion_space_empty(p):
    '''space : twoPoints expresion'''
    p[0] = Space_Declare("Space: ", p[1], p[2])
    print("space: "+p[1].type +","+ p[2].type) 


# Produccion para detectar las palabras reservadas
def p_reserved(p):
    '''reserved : TkInt
                | TkBool
                | TkArray readArray'''
    
    if (len(p) > 2):
        p[0] = Array("Array", p[1], p[2])
        print("Array: "+ p[1])
    elif (p[1] == 'int'):
        p[0] = Reserved("TkInt: ", value = p[1])
        print("Reservado TkInt: "+ p[1])
    else:
        p[0] = Reserved("TkBool: ", value = p[1])
        print("Reservado bool: "+ p[1])

# Produccion para leer un array
def p_read_array(p):
    '''readArray : expresion TkOBracket expresion TkCBracket
                 | TkOBracket expresion TkCBracket'''
    
    if (len(p) > 4):
        p[0] = ReadArray("ReadArray", p[1], p[3])
        print("ReadArray x[]: "+ str(p[1].type) +".."+ str(p[3].type))
    else:
        p[0] = ReadArray("ReadArray", p[2])
        print("ReadArray []: "+ str(p[2].type) )
# Produccion para detectar la expresion terminal de un numero
def p_number(p):
    '''number : TkNum'''
    p[0] = Atom("Number: ",p[1])
    print("Numero: "+  str(p[1]))

# Produccion para detectar la expresion terminal de un identificador
def p_expresion_id(p):
    '''Word : TkId'''
    p[0] = Atom("Ident: ", p[1])
    print("Ident: "+ p[1])

# Produccion para detectar la expresion terminal vacia o letra
def p_expresion_empty(p):
    '''Empty : '''
    p[0] = Atom("Empty" )
    print("Vacio: "+ p[0].type) 

# Produccion para detectar la expresion comentario
def p_coment(p):
    '''expresion : TkComent'''
    pass

# Manejador de errores 
def p_error(p):
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
            AST= ""
        elif(self.value == None):
            AST = "-"*level + self.type
        else:
            AST = "-"*level + self.type +self.value
        print(AST)

    def print_AST_DQ(self):
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
        #print("Este es el tipo del hijo:"+str(self.right.type))
        #print("Este es el tipo del hijo derecho :"+str(self.right.type))
        #print("Este es el tipo del hijo izquierdo :"+str(self.left.type))
        if (self.right.type == "Secuencia"):
            pila.append(self.left)
            #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
            pila += self.right.print_AST_DQ(level+1)
            #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
            while(len(pila)>0):
                level = len(pila)
                x = pila.popleft()
                if(x.value == None ):
                    continue
                else:
                    x.print_AST(level+1)
        else:
            self.left.print_AST(level+1)
            self.right.print_AST(level+1)

    def print_AST_DQ(self,level=0):
        pila = deque()
        ret = "-"*level + self.type 
        print(ret)
        #print("Este es el tipo del hijo derecho DQ:"+str(self.right.type))
        #print("Este es el tipo del hijo izquierdo DQ:"+str(self.left.type))
        if (self.left.type == "Secuencia"):
            #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
            pila += self.left.print_AST_DQ(level+1)
        else:
            pila.append(self.left)

        if (self.right.type == "Secuencia"):
            #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
            pila += self.left.print_AST_DQ(level+1)
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
        res = "Estoy en AST Twopoint, el hijo izquierdo es: "+ str(self.left.value)
        print(res) 
        res = "Estoy en AST Twopoint, el hijo derecho es: "+ self.right.type
        print(res)
        if (self.right.value == "int" or self.right.value == "bool" 
            or self.right.type == "Array"): 
            #print("Estoy en AST TwoPoint QD")
            pila = deque()
            AST = "-"*level
            pila += self.left.print_AST_DQ()
            pila.append(":")
            pila += self.right.print_AST_DQ()
            AST += " ".join(pila)
            print(AST)
        else:
            AST = "-"*level + self.type
            print(AST)
            self.left.print_AST(level+1)
            self.right.print_AST(level+1)


class Asignation:

    def __init__(self, type, left=None, right=None, value=0):
        self.type = type
        self.left = left
        self.right = right
        self.value = value

    def print_AST(self, level=0):
        AST = "-"*level + self.type
        #print("Estoy en Asignacion con valores I,D:")
        #print(self.left.type)
        #print(self.right.type)
        #print(self.right.value)
        print(AST)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

class Space_Declare:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

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
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append(",")
        pila += self.right.print_AST_DQ()
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
        pila += self.right.print_AST_DQ()
        return pila

    
class ReadArray:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0):
        #print("Estoy en AST READ")
        ret = "-"*level + self.type+":"
        print(ret)
        self.left.print_AST(level+1)
        self.right.print_AST(level+1)

    def print_AST_DQ(self):
        #print("Estoy en AST DQ READ")
        pila = deque()
        pila.append("[")
        pila += self.left.print_AST_DQ()
        pila.append("]")
        return pila

  
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

class toAtom:

    def __init__(self, type, children = None,level = 0, value=0):
        self.type = type
        self.level = level
        self.children = children
        self.value = value

    def print_AST(self, level=0):
        self.children.print_AST(level)

    def print_AST_DQ(self):
        return self.children.print_AST_DQ()
    
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