module Main where

import TGA
import System.IO
import Data.Complex
import System.Environment
import System.Exit
import Control.Parallel
import Data.Array

import MathInput

render :: (Ord a, Num a) => (a, a) -> (a -> a -> (Int,Int,Int)) -> [(Int,Int,Int)]
render (x,y) f = render' (x,y) (x,y) f
	where
		render' (0,1) 	_ 		_ 	= []
		render' (0,y) 	max@(u,v) 	f 	= render' (u,y-1) max f
		render' (x,y) 	max@(u,v) 	f 	= (p2 `par` (p1 `seq` (p1:p2)))
									where 
										p1 = f (u-x) (v-y)
										p2 = render' (x-1,y) max f

data Config = Config {viewx::Double, viewy::Double, sizex::Double, sizey::Double, 
						rpal::Array Int Int, gpal::Array Int Int, bpal::Array Int Int}

-- ALGORITMY

univ_fr :: [NaryType] -> Complex Double -> Complex Double -> Complex Double -> Int -> Int -> (Int, Bool, Complex Double)
univ_fr fn z c z' rec mrec | rec == mrec 								= (rec, True, z) --(1.0:+0.0) == eval_func "max_recursion", z)
						   | (1.0:+0.0) == eval_func fn "ending" [z]	= (rec, False, z) --(1.0:+0.0) == 
						   | otherwise									= univ_fr fn evl c z' (rec+1) mrec
						   		where evl = eval_func fn "fractal" [z,c,z']

-- ROVNICA PLOCHY
norm_to_circle u v x y vp = ((sizex vp)*( (fromIntegral x)/(fromIntegral u) - 0.5 ) + (viewx vp)):+
							((sizey vp)*( (fromIntegral y)/(fromIntegral v) - 0.5 ) + (viewy vp))

-- DOSADZUJUCA FUNKCIA
m (fn, modifs) (pl,plm) (u,v) iters vp x y = let point = norm_to_circle u v x y vp;
							(a,b,z) = 
									if "mandelbrot" `elem` modifs 
										then univ_fr fn (0.0:+0.0) point (0:+0) 0 iters
										else univ_fr fn point (0:+0) (0:+0) 0 iters
			in
				if b 	then coloring pl ("use_incoloring_palette" `elem` plm) "incoloring"  (u,v) z point x y a iters vp 
						else coloring pl ("use_outcoloring_palette" `elem` plm) "outcoloring" (u,v) z point x y a iters vp

konst = 64000.0
konst' = 64000

coloring pl pal_rend typ (u,v) z c x y iters max_its vp =
									if not pal_rend then (
										if typ=="incoloring" && isNaN(evl (typ++"_r")) then (0,0,0)
											else
											((round (255 * evl (typ++"_r")) `mod` 255),
											 (round (255 * evl (typ++"_g")) `mod` 255),
											 (round (255 * evl (typ++"_b")) `mod` 255))   )
										else
											((rpal vp)!(round (konst * evl typ) `mod` konst'),
											 (gpal vp)!(round (konst * evl typ) `mod` konst'),
											 (bpal vp)!(round (konst * evl typ) `mod` konst'))

					where
						evl t = realPart (eval_func pl t [(fromIntegral iters):+0,(fromIntegral max_its):+0,z,c])

g (u,v) x y = 
		(round (255/(sqrt 2)*sqrt(
				((fromIntegral x)/(fromIntegral u))**2 +
				((fromIntegral y)/(fromIntegral v))**2))
			,0,0)

genpal f 	= array (0,konst') [
						(idx, 
							( round (255 * realPart (f [(fromIntegral idx / konst):+0]) ))
						) 
					| idx <- [0..konst']
					]

main :: IO ()
main = do
	args <- getArgs
	progname <- getProgName
	if length args < 6 then putStrLn (progname ++ " width height iterations fractal_file palette_file output_file.tga")
		else let
				sx = read (args!!0)
				sy = read (args!!1)
				itrs = read (args!!2) 
				fract_file = args!!3
				palet_file = args!!4
				fname = (args!!5) 
					in do
										
					p_f <- openFile palet_file ReadMode
					putStrLn "Loading palette definition file"
					(pal_funs, pal_mods) <- parseFile p_f
					
					putStrLn "Loading fractal definition file"
					f_f <- openFile fract_file ReadMode
					skript@(f_funs,f_mods) <- parseFile f_f


					iters <- return $ if isNaryDefined f_funs "set_iterations" 	then round (realPart (eval_func f_funs "set_iterations" []))
																		else itrs
					
					
					putStrLn ("Res: " ++ show sx ++ "x" ++ show sy ++ ", " ++ show iters ++ " iterations, output = " ++ fname)
					putStrLn ("Using " ++ (if "mandelbrot" `elem` f_mods then "mandelbrot" else "julia") ++ " mode")
					putStrLn ("Incoloring  mode: " ++ if "use_incoloring_palette" `elem` pal_mods then "palette" else "true-color")
					putStrLn ("Outcoloring mode: " ++ if "use_outcoloring_palette" `elem` pal_mods then "palette" else "true-color")

					if length args == 7 && "--dump" == args!!6 
							then do {
								 putStrLn "Definitions from palette file:";
							     putStrLn $ foldr (\x y -> x++", "++y) "" $ show_nars pal_funs ;
								 putStrLn "Definitions from fractal file:";
								 putStrLn $ foldr (\x y -> x++", "++y) "" $ show_nars f_funs}
							else return ()

				
					let vp = Config {viewx = realPart (eval_func f_funs "center" []), viewy = imagPart (eval_func f_funs "center" []), 
								sizex = realPart (eval_func f_funs "size" []), sizey = realPart (eval_func f_funs "size" []),
								rpal = genpal (eval_func pal_funs "r"),
								gpal = genpal (eval_func pal_funs "g"),
								bpal = genpal (eval_func pal_funs "b")} 
						in do

							f <- createTGA fname (sx,sy)
							putTGApixels f (render (sx,sy) (m skript (pal_funs,pal_mods) (sx,sy) iters vp)) 0 (sx*sy)
							hClose f
