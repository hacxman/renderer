module MathInput where
import Data.Maybe
import Data.Complex
import Control.Parallel
import qualified Parse as P
import System.IO
import ParseTypes

add' [x,y]	 = x + y
sub' [x,y]	 = x - y
mul' [x,y]	 = x * y
div' [x,y]	 = x / y
sin' [x]	 = (sin (realPart x)) :+ 0
cos' [x]	 = (cos (realPart x)) :+ 0
tan'  [x]	 = (tan (realPart x)) :+ 0
abs' [x]	 = abs x
exp' [x,y]	 = if x == (0:+0) then (0:+0) else x**y
imag' [x:+y]  = 0 :+ y 
real' [x:+y]  = x :+ 0
cmp' [x,y]	  = if x == y then 1:+0 else 0:+0
gtr' [x,y]	 = if realPart x >= realPart y then 1.0:+0.0 else 0.0:+0.0
mag' [x]	= (magnitude x) :+ 0
log' [base,x] = (logBase (realPart base) (realPart x)) :+ 0.0
minus' [x]	 = -x
nan' _		= 0.0/0.0
max' [x,y]	= (max (realPart x) (realPart y)) :+ 0.0

type NaryType = ([Char], ([Complex Double] -> Complex Double, Int))
primits :: [NaryType]
primits		= [("+",	(add',	2)),
			   ("-",	(sub',	2)),
			   ("*",	(mul',	2)),
			   ("/",	(div',	2)),
			   ("sin", 	(sin',	1)),
			   ("cos",  (cos',  1)),
			   ("tan",  (tan',  1)),
			   ("abs",  (abs',  1)),
			   ("^",	(exp',  2)),
			   ("Im",	(imag',	1)),
			   ("Re",	(real', 1)),
			   ("is",	(cmp', 	2)),
			   ("isGreater", (gtr', 2)),
			   ("mag",	(mag',	1)),
			   ("log",	(log',	2)),
			   ("minus",(minus',1)),
			   ("nan",	(nan',	0)),
			   ("max",	(max',	2))
			   ]

show_nars [] = []
show_nars ((fname, (_, arity)):t) = (fname++"/"++show arity):show_nars t

isNaryDefined naries name = e $ lookup name naries
			where
				e (Just _) 	= True
				e Nothing 	= False

--evalE :: Exp -> ((String, Complex Double),(String, (([Complex Double] -> Complex Double), Int))) -> Complex Double
evalE (ConstE x)			(p,	naries) = x
evalE (VarE name)			(p,	naries) = e (lookup name p)
									where
										e (Just x) = x
										e Nothing = error ("Variable '"++name++"' is not defined")
evalE (NaryF fun pars)		(p,	naries) = g (e (lookup fun naries))
			where
					e (Just x) = x
					e Nothing = error ("Call to undefined function '"++fun++"'")
					g (fn, arity) | length pars == arity 	= fn $ map (`evalE`(p, naries)) pars
								| otherwise = error ("Function '"++fun++"' has "++show arity++" parameters, but "++show(length pars)++" given.")

obal exp pars naries p 	= evalE exp ((zip pars p), naries)

eval_func nars name p = (fst (e $ lookup name nars)) p
				where
					e (Just x) = x
					e Nothing = error ("Function '"++name++"' is not defined")

parseFile fhandle = do
				cont <- hGetContents fhandle
				parsed <- P.parse_functions cont
--				print parsed
				return $ g parsed primits
		where
			g ([],modifs) nars = (nars,modifs)
			g ((NameF (Left e) _):_, _) _ = error $ show e
			g ((NameF _ (Left e)):_, _) _ = error $ show e
			g ((NameF (Right (fname,fpars)) (Right [e])):x, modifs) nars = g (x,modifs) (nars++[(fname, (obal e fpars nars, length fpars))])

