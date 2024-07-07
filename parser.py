# ------------------------------------------------------------
# parser.py
# Generador de arbol AST para el lenguaje GCL
# Carnet: 15-10345 y 19-10211
# ------------------------------------------------------------
import ply.lex as lex
import ply.yacc as yacc
import sys
from lexer import tokens, reserved, Error_Counter
from collections import deque
if ".." not in sys.path: sys.path.insert(0,"..") # type: ignore

# Precedencia de algunas expresiones de GCL
precedence = (
    ('left', 'TkSemicolon'),
    ('left', 'TkAsig'),
    ('left', 'TkTwoPoints'),
    ('left', 'TkPlus'),
    ('left', 'TkMinus'),
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

    table = SymbolTable()
    tables.append(table)
    p[0] = Block("Block", p[2], 0)
    print("block: "+ p[2].type)
    #print(n)

# Produccion para detectar la expresion terminal declare
def p_expresion_declare(p):
    '''declare : TkDeclare expresion'''


    p[0] = Declare("Symbol Table", p[2])
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
    if p[1].type == "ReadArray":
        p[0] = Space_Declare("SDeclare", p[1], p[2])
        print("space: "+p[1].type +","+ p[2].type) 
    else:
        p[0] = Space_Declare("SDeclare", p[1], p[2])
        print("space: "+p[1].type +","+ p[2].type) 

def p_expresion_semicolon(p):
    '''semicolon : expresion TkSemicolon expresion''' 
    
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
                | TkMinus readArray %prec UMINUS
                | TkMinus word %prec UMINUS
                | TkMinus TkOpenPar aritmetic TkClosePar %prec UMINUS'''
    p[0] = Aritmetic("UMINUS",p[2])

# Produccion para detectar aritmetica
def p_expresion_aritmetic(p):
    '''aritmetic : aritmetic TkPlus aritmetic
                | aritmetic TkMinus aritmetic
                | word TkMinus aritmetic
                | aritmetic TkMult aritmetic
                | TkOpenPar aritmetic TkClosePar
                | negative
                | number
                | readArray
                | word '''
    if(len(p) > 2 and p[1] != '('):
        if(p[2] == '+'):
            p[0] = Aritmetic("Plus", p[1], p[3])
            print("Aritmetica: "+p[1].type +","+ p[3].type)
        elif(p[2] == '-'):
            p[0] = Aritmetic("Minus", p[1], p[3])
            print("Aritmetica: "+p[1].type +","+ p[3].type)
        elif(p[2] == '*'):
            p[0] = Aritmetic("Mult", p[1], p[3])
            print("Aritmetica: "+p[1].type +","+ p[3].type)
    elif p[1] == '(':
            p[0] = p[2]
            print("Parentesis: "+p[2].type)
    else: 
        p[0] = p[1]
        print("Aritmetica: "+p[1].type)

# Produccion para detectar la expresion no terminal TwoPoints
def p_expresion_two_point(p):
    '''twoPoints : expresion TkTwoPoints expresion
                 | expresion TkTwoPoints reserved
                 | number TkTwoPoints expresion'''
    if (p[3].type == "int" or p[3].type == "ReadArray" or p[3].type == "bool"):
        p[0] = TwoPoints("Tpdeclare", p[1], p[3], 'declare')
        print("dos puntos nivel context: "+p[1].type +","+ p[3].type)
    else:
        p[0] = TwoPoints("TwoPoints", p[1], p[3])
        print("dos puntos nivel: "+p[1].type +","+ p[3].type)
        #print("Contexto: "+p[0].context)
    

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
    elif (p[1] == 'bool'):
        p[0] = Reserved("bool", value = p[1])
        print("Reservado bool: "+ p[1])
    else: 
        p[0] = Reserved("skip", value = p[1])
        print("Reservado bool: "+ p[1])

# Produccion del ! 
def p_not(p):
    '''not : TkNot not
           | TkNot word
           | TkNot proposition
           | TkNot TkOpenPar proposition TkClosePar'''
    
    if (p[1] == '!'):
        p[0] = Not("Not", p[2])
        print("Not: "+p[2].type)
    else:
        p[0] = Not("Not", p[3])
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
                   | aritmetic
                   | number
                   | readArray
                   | reserved
                   | word'''
    
    if(len(p) > 2):
        if (p[2] == '/\\'):
            p[0] = Condition("And", p[1], p[3])
            print("And: "+p[1].type +","+ p[3].type)
        elif (p[2] == '\\/'):
            p[0] = Condition("Or", p[1], p[3])
            print("Or: "+p[1].type +","+ p[3].type)
        elif (p[2] == '<'):
            p[0] = Condition("Less", p[1], p[3])
            print("Less: "+p[1].type +","+ p[3].type)
        elif (p[2] == '<='):
            p[0] = Condition("Leq", p[1], p[3])
            print("Leq: "+p[1].type +","+ p[3].type)
        elif (p[2] == '>='):
            p[0] = Condition("Geq", p[1], p[3])
            print("Geq: "+p[1].type +","+ p[3].type)
        elif (p[2] == '>'):
            p[0] = Condition("Greater", p[1], p[3])
            print("Greater: "+p[1].type +","+ p[3].type)
        elif (p[2] == '=='):
            p[0] = Condition("Equal", p[1], p[3])
            print("Equal: "+p[1].type +","+ p[3].type)
        elif (p[2] == '!='):
            p[0] = Condition("NEqual", p[1], p[3])
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
    if (p[3].type == 'TkSoForth'):
        p[0] = ReadArray("ReadArray", p[1], p[3])
        
        print("ReadArray x[]: "+ str(p[1].type) +".."+ str(p[3].type))
    else:
        p[0] = ReadArray("ReadArray", p[1], p[3])
        print("ReadArray x[]: "+ str(p[1].type) +".."+ str(p[3].type))

# Produccion para escribir un array
def p_write_array(p):
    '''writeArray : word TkOpenPar expresion TkClosePar
                  | writeArray TkOpenPar expresion TkClosePar
                  | TkOpenPar expresion TkClosePar'''
    
    if (len(p) > 4):
        p[0] = WriteArray("WriteArray", p[1], p[3])
        print("Write x(): "+ str(p[1].type) +".."+ str(p[3].type))
    else:
        p[0] = WriteArray("WriteArray", p[2])
        print("Write (): "+ str(p[2].type) )

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

    p[0] = TwoSoFort("TkSoForth", p[1], p[3])
    print(".. nivel: "+p[1].type +","+ p[3].type) 

# Produccion para detectar el bucle for   
def p_for(p):
    "for : TkFor in TkArrow expresion TkRof"
    p[0] = Loop_For("For: ", p[2], p[4])
    print("For: "+p[2].type +","+ p[4].type) 

# Produccion para detectar condicion In
def p_expresion_in(p):
    '''in : number TkIn to
          | negative TkIn to
          | word TkIn to
          | readArray TkIn to '''

    p[0] = Loop_For("In", p[1], p[3])
    print("In: "+p[1].type +","+ p[3].type) 

# Produccion para detectar condicion To
def p_expresion_to(p):
    '''to :  expresion TkTo expresion'''
    
    p[0] = Loop_For("To", p[1], p[3])
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

# Produccion para detectar un print
def p_expresion_print(p):
    '''print : TkPrint sentence'''

    p[0] = Print("Print", p[2])
    print("Print: "+ p[2].type)
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
        print("Concatenar nivel: "+p[1].type +","+ p[3].type) 
    else:
        p[0] = Transicion("", p[1])
        print("String: "+  str(p[1]))  

# Produccion para detectar la expresion terminal una palabra
def p_string(p):
    "string : TkString"
    p[0] = Atom("String: ",p[1])
    print("String: "+  str(p[1]))

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
    print(f"Error de sintaxis en la linea {p.lineno -1} columna:  + {str(find_column(p.lexer.lexdata,p))} : token inesperado: {p.value}")
    yacc.restart()


#Inicializador del parser
parser = yacc.yacc()


# Tabla de simbolos

tables = []

#Clase para la creacion de nodos, con el fin de generar el arbol AST
class Atom:
    def __init__(self, type,value=None, context = None):
        self.type = type
        self.value = str(value)
        self.context = context

    def print_AST(self, level=0, block=0):
        if (self.type == "Empty"):
            AST =""
        elif(self.value == None):
            AST = "-"*level + self.type
        else:
            AST = "-"*level + self.type +self.value + " | "+ "type: "+ self.context
        print(AST)
      

    def print_AST_DQ(self, level=0):
        pila = deque()
        pila.append(self.value)
        return pila
    
    def add_context(self, block=0):
        print("ESTOY EN ATOM CONTEXT")
        #print(self.value)
        if self.type == "Empty" or self.type == "String: ":
            self.context = ""
            return None
        elif es_entero(self.value):
            self.context = "int"
            print(f"El valor para '{self.value}' encontrado en el bloque {block} es {self.context}.")
            return "int"
        else:
            while block > -1 :
                # Suponiendo que 'tables' es un diccionario o una lista de diccionarios accesible en este contexto

                value = tables[block].get(self.value)

                if value is None:
                    value = tables[block].get_loop(self.value)
                if value is None:
                    # El valor obtenido es None, puedes realizar acciones específicas aquí
                    block -= 1
                else:
                    # El valor obtenido no es None, puedes usar 'value' aquí
                    print(f"El valor para '{self.value}' encontrado en el bloque {block} es {value}.")
                    self.context = value
                    return value
            print("---------------Variable not declared in ATOM")
        sys.exit(1)
        
def es_entero(s):
    try:
        int(s)  # Intenta convertir el str a int
        return True  # La conversión fue exitosa
    except ValueError:
        return False  # La conversión falló, s no es un int
    
class Reserved:

    def __init__(self, type, left=None, right=None, value=None, context=None):
        self.type = type
        self.value = value
        self.left = left
        self.right = right
        if self.value == "true" or self.value == "false":
            self.context = "bool"
        else:
            self.context = type

    def print_AST(self, level=0, block=0):
        if(self.value == None):
            AST = "-"*level + self.type
        else:
            AST = "-"*level + self.type +self.value +" | "
            + "type: "+ self.context
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
    
    def add_context(self, block=0):
        if(self.value == None):
            return None
        else:
            return self.context
        #print(AST+ "ESTOY EN RESERVED CONTEXT")
class Secuencia:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

        
    def print_AST(self, level=0, block =0):
        pila = deque()
        if not self.left.type == "Tpdeclare":
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
        #print("pila en el nivel "+ str(pila))
        while(len(pila)>0):
            x = pila.popleft()
            #print(x.type)
            if x is None:
                continue
            elif x.type is "Tpdeclare":
                continue
            x.print_AST(level+1, block)
            
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
    
    def add_context(self, block=0):
        pila = deque()
        #status(self)
        pila.append(self.left)
        print("Estoy en context secuencia")
        #print(pila)
        #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
        if(self.right.type == "Secuencia"):
            print("Resulto ser secuencia")
            pila += self.right.print_AST_DQ()
        else:
            pila.append(self.right)
        #print(len(pila))
        #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
        while(len(pila)>0):
            x = pila.popleft()
            if x is None:
                continue
            if (x.type == 'Block'):
                x.add_context(block+1)
            else:
                x.add_context(block)
class TwoPoints:

    def __init__(self, type, left=None, right=None, context=None, block=0):
        self.type = type
        self.left = left
        self.right = right
   
    def print_AST(self, level=0, block =0):
        #status(self)
        if (self.right.type == "Space" or self.right.type == "SDeclare"):
            #print(self.right.right.type)
            if self.right.right.type == "SDeclare":
                self.right.right.print_AST(0, block)
            else:
                self.right.right.print_AST(level, block)
        elif (self.right.type == "ReadArray" or self.right.value == "int" or self.right.value == "bool" 
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
            self.left.print_AST(level+1, block)
            self.right.print_AST(level+1, block)

    def print_AST_DQ(self, level=0):
        #status(self)
        #print("Estoy en Twopoints")
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append(":")
        pila += self.right.print_AST_DQ()
        #print(pila)
        return pila
    
    def add_context(self, block=0):
        status(self)
        print("*************** TWO POINT")

        if (self.right.type == "Space" or self.right.type == "SDeclare"):
            print("Estoy en context space TwoPoint QD")
            pila_left = self.left.print_AST_DQ()
            pila = self.right.print_AST_DQ()
            print(pila)
            pila_right = pila.popleft().print_AST_DQ()
            print(pila_right)
            long = None
            print(self.left.type)
            print(self.right.type)
            print("REVISANDO LONG"+ str(long))
            if (self.right.type == "ReadArray" or self.right.type == "array" 
                or self.right.type == "Space" or self.right.type == "SDeclare"):
                print("CONSTRUYENDO LONG Y ARRAY")
                cleaned = []
                negative = ""
                for element in pila_right:
                    if element is "-": 
                        negative = "-"
                        continue
                    if element.isdigit() and negative == "-":
                        element = negative+element
                        cleaned.append(element)
                        negative = ""
                    elif element.isdigit():
                        cleaned.append(element)
                if not (int(cleaned[0])) <= int(cleaned[1]):
                    print("---------------Invalid range")
                    sys.exit(1)
                else:
                    long = int(cleaned[1]) -(int(cleaned[0]))+1

            context = "".join(pila_right)
            #print(pila_left)
            for element in pila_left:
                if not tables[block].lookup(element):
                    tables[block].add(element, context)
                    if long != None:
                        tables[block].add_long(element, long)
                elif tables[block].lookup(element):
                    print("---------------Variable already declared")
    
            while len(pila) > 0:
                element = pila.popleft()
                print(element)
                if element.type == 'Block':
                    print("IMPRIMIENDO BLOQUE")
                    element.add_context(block+1)
                elif (element.type != 'Empty'):
                    print("NO IMPRIMIENDO BLOQUE")
                    element.add_context()

            #print("termine context space TwoPoint QD")
            #print(pila_right)
        elif (self.right.type == "ReadArray" or self.right.value == "int" or self.right.value == "bool" 
            or self.right.value == "array"): 
            print("Estoy en context reserved TwoPoint QD")
            pila_left = self.left.print_AST_DQ()
            pila_right = self.right.print_AST_DQ()
            long = None
            if (self.right.type == "ReadArray" or self.right.type == "array"):
                cleaned = []
                negative = ""
                for element in pila_right:
                    if element is "-": 
                        negative = "-"
                        continue
                    if element.isdigit() and negative == "-":
                        element = negative+element
                        cleaned.append(element)
                        negative = ""
                    elif element.isdigit():
                        cleaned.append(element)
                if not (int(cleaned[0])) <= int(cleaned[1]):
                    print("---------------Invalid range")
                    sys.exit(1)
                else:
                    long = int(cleaned[1]) -(int(cleaned[0]))+1
            context = "".join(pila_right)
            for element in pila_left:
                if not tables[block].lookup(element):
                    tables[block].add(element, context)
                    if long != None:
                        tables[block].add_long(element, long)
                elif tables[block].lookup(element):
                    print("---------------Variable already declared")
            print("termine context reserved TwoPoint QD")
        else:
            print("ESTOY EN ELSE")
            context_left = self.left.add_context(block)
            context_right = self.right.add_context(block) 

            if context_left == context_right:
                return context_left
            else:
                print("---------------Type mismatch two point")
                sys.exit(1)
        print("**************")

class Asignation:

    def __init__(self, type, left=None, right=None, value=0):
        self.type = type
        self.left = left
        self.right = right
        self.value = value

    def print_AST(self, level=0, block = 0):
        AST = "-"*level + self.type
        #status(self)
        print(AST)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def print_AST_DQ(self, level=0):
        #status(self)
        pila = deque()
        pila.append(self.left)
        if (self.right.type == "Secuencia"):
            #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
            pila += self.right.print_AST_DQ(level+1)
        #print("pila en el nivel DQ "+ str(self.type)+": "+ str(pila))
        return pila

    def add_context(self, block=0):
        status(self)
        print("****************")
        print("Estoy en contexto asignacion")
        context_left = self.left.add_context(block)
        if self.left.type is "ReadArray":
            print("---------------Type mismatch asig left")
            sys.exit(1)
        if "array" in context_left and self.right.type is "ReadArray":
            
            print("---------------Type 2mismatch asig left")
            sys.exit(1)
        if self.right.type == "Comma" and ("array" in context_left 
            and not(self.left.type is "ReadArray")):
            print("CONSEGUI UNA COMA ")
            level = self.right.level()
            print(level)
            item = self.left.value
            long = tables[block].get_long(item) - level
            print(long)
            if long < 0:
                print("----------------- long asig error, array pequeño")
                sys.exit(1)
            context_right = self.right.add_context(block,level)
        else:
            context_right = self.right.add_context(block)
        print(context_left)
        print(context_right)
        if "array" in context_right and self.right.type is "Ident: ":
            print("---------------Type mismatch asig right")
            sys.exit(1)
        
        if not (("array" in context_left) or ("array" in context_right) or 
                context_left == context_right):
            print("---------------Type mismatch asig")
            sys.exit(1)
        print("Termone contexto asignacion")
        print("****************")
class Space_Declare:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right
    def print_AST(self, level=0, block = 0):
        
        #status(self)
        if not self.type == "SDeclare":
            self.left.print_AST(level)
        if (self.right.type == "Secuencia"):
            self.right.print_AST(level+1, block)
        else:
            self.right.print_AST(level, block)

    def print_AST_DQ(self,level=0):
        status(self)
        pila = deque()
        #self.left.level = level
        pila.append(self.left)
        #print("Estoy space QD")
        #status(self)
        if (self.right.type == "Secuencia"):
            #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
            pila += self.right.print_AST_DQ(level+1)
        #print("pila en el nivel DQ "+ str(self.type)+": "+ str(pila))
        #print(pila)
        #OJOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
        pila.append(self.right)
        return pila
    
    def add_context(self, block=0):
        #status(self)
        print("**")
        print("Estoy en space context")
        self.left.add_context(block)
        print(tables[0].symbols)
        if (self.right.type == "Block"):
            self.right.add_context(block+1)
        else:
            self.right.add_context(block)
        print("**")
class Comma:

    def __init__(self, type, left=None, right=None, context=None):
        self.type = type
        self.left = left
        self.right = right
        self.context = context
    def print_AST(self, level=0, block = 0):
        AST = "-"*level + self.type+ " | "+ "type: "+ self.context
        print(AST)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def print_AST_DQ(self, level=0):
        #print("Estoy en AST ARRAY DQ")
        #print(self.value)
        #status(self)
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila += self.right.print_AST_DQ()
        return pila  
    
    def add_context(self, block=0, long=0):
        print("Estoy en coma")
        if (self.left.type == "Comma"):
            context_left=self.left.add_context(block, long-1)
        else:
            context_left=self.left.add_context(block)

        context_right=self.right.add_context(block) 
        if context_left == context_right or (("array" in context_left) 
            or ("array" in context_right)):
            self.context = "array with long "+str(long)
            return self.context
        print("Termine en coma")
    
    def level(self):
        level = 1
        status(self)
        if self.left.type == "Comma":
            level += self.left.level()
        else:
            level += 1
        return level
class Concat:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        AST = "-"*level + self.type
        print(AST)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def print_AST_DQ(self, level=0):
        #print("Estoy en AST ARRAY DQ")
        #print(self.value)
        pila = deque()
        pila += self.left.print_AST_DQ(level)
        pila.append(".")
        pila += self.right.print_AST_DQ(level)
        return pila
    
    def add_context(self, block=0):
        self.left.add_context(block)
        self.right.add_context(block) 

class Aritmetic:

    def __init__(self, type, left=None, right=None, context=None):
        self.type = type
        self.left = left
        self.right = right
        self.context = "int"
    def print_AST(self, level=0, block = 0):
        if (self.type == "UMINUS"):
            AST = "-"*level + 'Minus'
            print(AST)
            self.left.print_AST(level+1, block)
        else:
            AST = "-"*level + self.type+ " | "+ "type: "+ self.context
            print(AST)
            self.left.print_AST(level+1, block)
            self.right.print_AST(level+1, block)

    def print_AST_DQ(self, level=0):
        #print("Estoy en AST ARRAY DQ")
        #print(self.value)
        #status(self)
        if (self.type == "UMINUS"):
            #print("Estoy en uminus")
            pila = deque()
            pila.append("-")
            pila += self.left.print_AST_DQ()
            return pila
        else:
            #print("No estoy en uminus")
            pila = deque()
            pila += self.left.print_AST_DQ()
            pila.append(",")
            pila += self.right.print_AST_DQ()
            return pila
        
    def add_context(self, block=0):
        if (self.type == "UMINUS"):
            context = self.left.add_context(block)
            print("Tipo de uminus"+ self.left.type+context)
            #print(context)
            if self.left.type == "Ident: " and ("array" in context):
                print("---------------Type mismatch MINUS asu¿ig")
                sys.exit(1)
            else:
                return "int"
        else:
            status(self)
            context_left = self.left.add_context(block)
            context_right = self.right.add_context(block)
            if self.left.type == "Ident: " and "array" in context_left:
                print("---------------Type mismatch izquierda array")
                sys.exit(1)
            elif self.right.type == "Ident: " and "array" in context_right:
                print("---------------Type mismatch derecha array")
                sys.exit(1)
            print(context_left)
            print(context_right)
            if (context_left == context_right or ("array" in context_left and self.left.type is "ReadArray") 
                or ("array" in context_right and self.right.type is "ReadArray")):
                return self.context
            else:
                sys.exit(1)
class Print:  
    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
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
                    x.print_AST(level+1, block)
            else:
                AST = "-"*level + self.type
                print(AST)
                self.left.print_AST(level+1, block)

    def print_AST_DQ(self, level=0):
        pila = deque()
        if self.left is not None:
            pila += self.left.print_AST_DQ(level)
        if self.right is not None:
            pila += self.right.print_AST_DQ(level)
        return pila 
    
    def add_context(self, block=0):
        if self is not None:
            #status(self)
            if(self.right is not None):
                pila = deque()
                pila += self.left.print_AST_DQ()
                print(pila)
                while(len(pila)>0):
                    x = pila.popleft()
                    x.add_context()
            else:
                self.left.add_context(block) 
'''
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
'''
    
class ReadArray:

    def __init__(self, type, left=None, right=None, context=None):
        self.type = type
        self.left = left
        self.right = right
        self.context = context

    def print_AST(self, level=0, block = 0):
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
            self.left.print_AST(level+1, block)
            self.right.print_AST(level+1, block)

    def print_AST_DQ(self, level=0):
        #print("Estoy en AST DQ READ")
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append("[")
        pila += self.right.print_AST_DQ()
        pila.append("]")
        #print(pila)
        return pila
    
    def add_context(self, block=0):
        print("EStoy en READARRAY")
        status(self)
        context_left = self.left.add_context(block)
        contexto_right = self.right.add_context(block)
        if "array" in context_left or context_left == "int":
            if (contexto_right == "int"):
                self.context = "int"
                print("termine readArray")
                return context_left
            sys.exit(1)
        sys.exit(1) 
            
class WriteArray:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        #status(self)
        #print("Estoy en AST READ")
        ret = "-"*level + self.type+":"
        print(ret)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def print_AST_DQ(self, level=0):
        #print("Estoy en AST DQ READ")
        pila = deque()
        pila.append("(")
        pila += self.left.print_AST_DQ(level)
        pila.append(")")
        return pila
    
    def add_context(self, block=0):
        print("EStoy en WRITEARRAY")
        status(self)
        context_left = self.left.add_context(block)
        contexto_right = self.right.add_context(block)
        print(context_left)
        print(contexto_right)
        if "array" in context_left or context_left == "int":
            print("SI ES ARRAY WRITE")
            if (contexto_right == "int"):
                self.context = "int"
                print("termine writeARRAY")
                return context_left
            sys.exit(1)
        sys.exit(1)
class Not:

    def __init__(self, type, children):
        self.type = type
        self.children = children

    def print_AST(self, level=0, block = 0):
        #res = "Estoy en AST FOR, el hijo izquierdo es: "+ str(self.left)
        #print(res) 
        #res = "Estoy en AST FOR, el hijo derecho es: "+ str(self.right)
        #print(res)
        ret = "-"*level + self.type
        print(ret)
        self.children.print_AST(level+1, block)

    def add_context(self, block=0):
        print("ESTOY EN NOT CONTEXT")
        context = self.children.add_context(block)
        print("TERMINE NOT CONTEXT")
        return context
class Condition_If:

    def __init__(self,type, children = None,level = 0 ):
        self.type = type
        self.level = level
        self.children = children

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type 
        print(ret)
        self.children.print_AST(level+1, block)
    
    def add_context(self, block=0):
        print("ESTOY EN IF CONTEXT")
        print(self.children.type)
        self.children.add_context(block)
        print("TERMINE IF CONTEXT")
class Guard:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        pila = deque()
        ret = "-"*level + self.type 
        print(ret)
        #status(self)
        pila.append(self.left)
        #print(pila)
        #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
        if(self.right.type == "Guard"):
            #print("Resulto ser secuencia")
            pila += self.right.print_AST_DQ(level+1, block)
        else:
            pila.append(self.right)
        #print(len(pila))
        #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
        #for x in pila: print(x.type)
            
        while(len(pila)>0):
            x = pila.popleft()
            x.print_AST(level+1, block)

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
    
    def add_context(self, block=0):
        print("ESTOY EN GUARD CONTEXT-------------------")
        pila = deque()
        pila.append(self.left)
        #print(pila)
        #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
        status(self)
        if(self.right.type == "Guard"):
            #print("Resulto ser secuencia")
            pila += self.right.print_AST_DQ()
        else:
            pila.append(self.right)
        #print(len(pila))
        #print("pila en el nivel "+ str(self.type)+": "+ str(pila))
        #for x in pila: print(x.type)
            
        while(len(pila)>0):
            x = pila.popleft()
            if(x.type == 'Block'):
                x.add_context(block+1)
            else:
                x.add_context(block)
        print("TERMINE GUARD CONTEXT-----------------------")

class Arrow:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        #res = "Estoy en AST FOR, el hijo izquierdo es: "+ str(self.left)
        #print(res) 
        #res = "Estoy en AST FOR, el hijo derecho es: "+ str(self.right)
        #print(res)
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def add_context(self, block=0):
        print("ESTOY EN ARROW CONTEXT")
        status(self)
        context_left = self.left.add_context(block)
        if context_left != "bool":
            print(context_left)
            print("------------------ERROR EN EL ARROW lef")
            sys.exit(1)
        self.right.add_context(block)
        print("TERMINE GUARD CONTEXT")

class Condition:

    def __init__(self, type, left=None, right=None, context=None):
        self.type = type
        self.left = left
        self.right = right
        self.context = context
    def print_AST(self, level=0, block = 0):
        #res = "Estoy en AST FOR, el hijo izquierdo es: "+ str(self.left)
        #print(res) 
        #res = "Estoy en AST FOR, el hijo derecho es: "+ str(self.right)
        #print(res)
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def add_context(self, block=0):
        context_left = self.left.add_context(block)
        context_right = self.right.add_context(block)
        print("estoy en condition")
        print(self.type)
        print(self.left.type)
        print(self.right.type)
        print(context_left)
        print(context_right)
        print("termine condition")
        if context_left == context_right:
            self.context = "bool"
            return self.context
        else:
            return "ERROR"
class Loop_For:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        #res = "Estoy en AST FOR, el hijo izquierdo es: "+ str(self.left)
        #print(res) 
        #res = "Estoy en AST FOR, el hijo derecho es: "+ str(self.right)
        #print(res)
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def add_context(self, block=0):
        status(self)
        if self.type == "In":
            if not self.left.type == "Ident: ":
                print("ERRRRRRRRRPORRRRRRRR  en for in, muchas variables")
            x = self.left.print_AST_DQ()
            print("ESTPY EN IN")
            print(x)
            x = "".join(x)
            print(x)
            if not tables[block].lookup(x):
                print("NO ESTOY EN LA TABLA")
                tables[block].add_loop(x)    
                print(tables[block].loop)
                context_right = self.right.add_context(block)
                if not "int" == context_right:
                    print("---------------------ERROR EN EL FOR IN")
                self.left.context = "int"
            else :
                print("---------------Variable for in already declared en for in")
        elif self.type == "To":  
            context_left = self.left.add_context(block)
            context_right = self.right.add_context(block)
            if not context_left and context_right == "int":
                print("---------------------ERROR EN EL FOR TO")
            else :
                return "int"
        else:
            self.left.add_context(block)
            self.right.add_context(block)
            print(tables[block].loop)
            tables[block].loop.clear()
            print("Termine loop for")
class Loop_Do:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        #res = "Estoy en AST FOR, el hijo izquierdo es: "+ str(self.left)
        #print(res) 
        #res = "Estoy en AST FOR, el hijo derecho es: "+ str(self.right)
        #print(res)
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1, block)

    def add_context(self, block=0):
        self.left.add_context(block)
class TwoSoFort:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type+":"
        print(ret)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def print_AST_DQ(self):
        #status(self)
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append("..")
        pila += self.right.print_AST_DQ()
        #print(pila)
        return pila
    
    def add_context(self, block=0):
        self.left.add_context(block)
        self.right.add_context(block)

class Declare:

    def __init__(self, type, children = None,level = 0):
        self.type = type
        self.level = level
        self.children = children

    def print_AST(self, level=0, block = 0):
        tables[block].print_AST(level)
        self.children.print_AST(level+1, block) 

    def add_context(self, block=0):
        print("*****************")
        print("Declare:")
        self.children.add_context(block)
        print("*****************")

class Block:
    def __init__(self,type, children = None,level = 0):
        self.type = type
        self.level = level
        self.children = children

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type 
        print(ret)
        self.children.print_AST(level+1, block)

    def add_context(self, block=0):
        print("******************")
        print("Block:")
        self.children.add_context(block)
        print("******************")
class Transicion:

    def __init__(self, type, children = None,level = 0, value=0):
        self.type = type
        self.level = level
        self.children = children
        self.value = value

    def print_AST(self, level=0, block = 0):
        #print("Estoy en Transicio con type:")
        #print(self.type)
        #print("Estoy en Transicio con hijo type:")
        #print(self.children.type)
        if(self.type == "Print"):
            AST = "-"*level + self.type
            print(AST)

            self.children.print_AST(level+1, block)
        else:
            self.children.print_AST(level, block)

    def print_AST_DQ(self):
        return self.children.print_AST_DQ()
    
    def add_context(self, block=0):
        print("*******")
        print("trasicion:")
        self.children.add_context(block)
        print("********") 
class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.long ={}
        self.loop = {}
    def add(self, name, type):
        if name in self.symbols:
            raise Exception(f"Error: Redeclaration of variable '{name}'")
        self.symbols[name] = type

    def add_long(self, name, long):
        if name in self.long:
            raise Exception(f"Error: Redeclaration long of variable '{name}'")
        self.long[name] = long

    def add_loop(self, name):
        self.loop[name] = "int"

    def lookup(self, name):
        if name in self.symbols or name in self.loop:
            return True
        else:
            False
            #raise Exception(f"Error: Undeclared variable '{name}'")
    def get(self, name):
        return self.symbols.get(name)
    def get_long(self, name):
        return self.long.get(name)
    def get_loop(self, name):
        return self.loop.get(name)          
    def print_AST(self, level=0):
        print("-"*level + "Symbol Table")
        for name, type in self.symbols.items():
            print("-"*(level+1) + f"{name}: {type}")
   
#Clase para manejo de errores.     
class SyntaxErrorException(Exception):
    def __init__(self, message, lineno):
        super().__init__(message)
        self.lineno = lineno

    def __str__(self):
        return f"{self.args[0]} (line {self.lineno})"
def find_column(input,token):
  ultimoSalto = input.rfind('\n',0,token.lexpos)
  if ultimoSalto < 0:
    ultimoSalto = 0
  column = (token.lexpos - ultimoSalto) + 1
  return column   
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
    result.add_context()
    result.print_AST()
    print(tables)
    for table in tables:
        print(table.symbols)
        print(table.long)
        print(table.loop)
    break   