module Parse (parse_functions) 
where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language 
import Data.Maybe
import Data.Complex

import ParseTypes

parse_functions input 	= 
			do { 
			; let functions =  map complete (filter invalid (map alter (lines input)))
			; let keywords 	=  map tail (filter nonkeyword (lines input))
			; return (functions, keywords)
			}	
			where
				alter n = (ns a) ++ (ns b)
					where 
					(a,b) = break ('='==) n
					ns x  = filter (/=' ') x

				invalid n = and [n /= "", (head n) /= '=', (head n) /= '-' , (last n) /= '=', '=' `elem` n]
				nonkeyword n = and [n /= "", (head n) == '!']
				
				complete n = NameF fndefresult fnexpr
					where 
					(a,b) = break ('='==) n
					fnexpr = partial $ tail b
					fndefresult  = fndef a

					partial n = parse	(do{ whiteSpace
								; x <- sepBy function (many newline)
								; eof
								; return x
								})
								"" n
					fndef n   = parse	(do{ whiteSpace
								; x <- funcdef
								; return x
								})
								"" n


		
-- create LanguageDef
funcDef	=
	emptyDef
	{ caseSensitive = True 
        , reservedOpNames = ["*","/","+","-","^"]
	}

-- new lexer
lexer :: P.TokenParser ()
lexer = P.makeTokenParser funcDef

-- bind to toplevel
whiteSpace	= P.whiteSpace lexer
lexeme    	= P.lexeme lexer
parens		= P.parens lexer
reserved  	= P.reserved lexer
identifier	= P.identifier lexer
reservedOp	= P.reservedOp lexer
float 		= P.float lexer
natural		= P.natural lexer


-- real part of number
real	=  
	try(float)
	<|>
	lexeme(do	{ x <- natural
			; return ((fromInteger x)::Double)
			})
	<?> "real part"

-- imaginary part
imaginary =
	try(lexeme(do 	{ x <- float
			; char 'i'
			; return x
			}))
	<|>
	lexeme(do	{ x <- natural
			; char 'i'
			; return ((fromInteger x)::Double)
			})
	<?> "imaginary part"

-- return complex number (also converts real to complex)
-- handles four types of complex number notation a+i, a+bi, a-i, a-bi + real numbers
number	=
	try(lexeme(do	{ r <- real
			; char '+'
			; char 'i'
			; return $ ConstE (r :+ 1)
			}))
	<|>
	try(lexeme(do	{ r <- real
			; char '-'
			; char 'i'
			; return $ ConstE (r :+ (-1))
			}))
	<|>
	try(lexeme(do	{ r <- real
			; char '+'
			; i <- imaginary
			; return $ ConstE (r :+ i)
			}))
	<|>
	try(lexeme(do	{ r <- real
			; char '-'
			; i <- imaginary
			; return $ ConstE (r :+ (-i))
			}))
	<|>
	try(lexeme(do	{ r <- real
			; return $ ConstE (r :+ 0)
			}))
	<?> "number"	

-- variable
variable = 
	try(lexeme(do	{ v <- identifier 
			; return $ VarE $ v
			}))
	<?> "variable"

-- function with unilimited number of parameters - sub-expressions
subfunction = 
	try(lexeme(do	{ fn <- identifier
			; char '('
			; params <- sepBy expr (char ',')
			; char ')'
			; return $ NaryF fn params
			}))
	<|>
	try(lexeme(do	{ fn <- char '-'
					; param <- expr
					; return $ NaryF "minus" [param]
					}))
	<?> "subfunction"

-- function definition (before =)
funcdef = lexeme(do	{ fn <- identifier
			; char '('
			; params <- sepBy identifier (char ',')
			; char ')'
			; return $ (fn, params)
			})
	<?> "function definition"


-- main expression parsed
expr    = buildExpressionParser table factor
	<?> "expression"

-- operator wrapper
op_add x y = NaryF "+" [x,y]
op_sub x y = NaryF "-" [x,y]
op_mul x y = NaryF "*" [x,y]
op_div x y = NaryF "/" [x,y]
op_exp x y = NaryF "^" [x,y]
op_min x   = NaryF "minus" [x]

-- operator table, handles operator priority
table   = [[op "^" op_exp AssocLeft]
	 	  ,[op "*" op_mul AssocLeft, op "/" op_div AssocLeft]
          ,[op "+" op_add AssocLeft, op "-" op_sub AssocLeft]
          ]
        where
          op s f assoc
             = Infix (do{ reservedOp s; return f } <?> "operator") assoc

-- parenthesis handler
factor  = parens expr
     <|> number 
     <|> subfunction
     <|> variable
     <?> "parenthesis"


function = lexeme(expr)
