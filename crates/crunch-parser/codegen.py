import random, string

def indent(indentlev: int) -> str:
    s = ""
    for _ in range(0, indentlev):
        s += "    "
    return s

def rand_ident(length: int) -> str:
   letters = string.ascii_lowercase
   return ''.join(random.choice(letters) for i in range(length))

def rand_ty() -> str:
    types = ["unit", "bool", "int", "str", "float", ""]
    
    ty = random.choice(types)
    
    if ty == "":
        ty = rand_ident(random.randint(1, 20))
        
    return ty

def rand_params() -> str:
    params = ""
    
    for _ in range(0, random.randint(0, 50)):
        params += f" {rand_ident(random.randint(20, 50))} : {rand_ty()} , "
        
    return params

def rand_call_params() -> str:
    params = " "
    
    for _ in range(0, random.randint(0, 50)):
        params += rand_ident(random.randint(20, 50))
        params += " , "
        
    return params

def expr(paren: bool) -> str:
    exprs = []
    if paren:
        exprs = ["func", "binop", "var", "paren"]
    else:
        exprs = ["func", "binop", "var"]
    binops = ["+", "-", "/", "*", "&", "**", "^", "%", "|"]
    
    exp = random.choice(exprs)
    
    if exp == "func":
        exp = rand_ident(random.randint(2, 20))
        exp += f"({rand_call_params()}) "
    
    elif exp == "var":
        exp = rand_ident(random.randint(2, 20))
        exp += " "
        
    elif exp == "paren":
        exp = "( "
        exp += expr(True)
        exp += " )"
        exp += " "
    
    elif exp == "binop":
        exp = rand_ident(random.randint(2, 20))
        exp += f" {random.choice(binops)} "
        exp += expr(True)
        exp += " "
        
    return exp

def statement(indentlev: int) -> str:
    stmts = ["if", "let", "func", "return"]
    # "match", "for", "while", "loop", 
    
    stmt = random.choice(stmts)
    
    if stmt == "return":
        s = indent(indentlev)
        s += stmt
        s += "\n"
        return s
    
    elif stmt == "if":
        stmt = indent(indentlev)
        stmt += "if "
        
        stmt += expr(True)
        stmt += " \n"
        indentlev += 1
        
        for _ in range(0, random.randint(1, 5)):
            stmt += statement(indentlev)
        
        indentlev -= 1
        stmt += indent(indentlev)
        stmt += "end\n"
        
        return stmt
    
    elif stmt == "let":
        stmt = indent(indentlev)
        stmt += "let "
        stmt += rand_ident(random.randint(2, 100))
        stmt += " = "
        stmt += expr(True)
        stmt += " "
        stmt += "\n"
        
        return stmt
    
    elif stmt == "func":
        stmt = indent(indentlev)
        stmt += rand_ident(random.randint(5, 15))
        stmt += "( "
        stmt += rand_call_params()
        stmt += " )\n"
        
        return stmt

def function(indentlev: int) -> str:
    func = f"fn {rand_ident(random.randint(5, 15))}({rand_params()}) -> {rand_ty()}\n"
    indentlev += 1
    
    for _ in range(10, random.randint(20, 50)):
        func += statement(indentlev)
    
    indentlev -= 1
    func += "end\n\n"
    return func

def generate() -> str:
    output = ""
    indentlev = 0
    
    for _ in range(30, random.randint(40, 100)):
        output += function(indentlev)
        
    return output
        

def main():
    with open("test.crunch", "w+") as f:
        f.truncate(0)
        f.write(generate())

if __name__ == "__main__":
    main()