{-# LANGUAGE MultiWayIf #-}


machine_epsilon_order = 1000


find_eps :: Double -> Double
find_eps eps
    | (eps / 2 + 1) > 1 = find_eps (eps / 2)
    | otherwise = eps * machine_epsilon_order
        


eps = find_eps 1


data Fstruct = Val {
    value     :: Double ,
    iteration :: Int
}


derivative :: (Double -> Double) -> (Double -> Double)
derivative f x = (f (x + eps) - f (x - eps)) / (2 * eps)


second_derivative :: (Double -> Double) -> (Double -> Double)
second_derivative f x = 
    let
        epps = sqrt (sqrt eps)      -- may be unsafe
    in
        (f (x + 2 * epps) - 2 * f x + f (x - 2 * epps)) / (4 * epps * epps)


eq1 :: Double -> Double             -- first equation to solve
eq1 x = 3 * x - 14 + (exp x) - (exp ((-1) * x))


mapping_eq1 :: Double -> Double     -- compression mapping 
mapping_eq1 x = (8 * x + (exp ((-1) * x)) - (exp x) + 14) / 11


eq2 :: Double -> Double             -- second equation to solve
eq2 x = sqrt (1 - x) - tan x


mapping_eq2 :: Double -> Double     -- compression mapping
mapping_eq2 x = (2 * x + (1 - tan x) * (1 + tan x)) / 3


dichotomy :: Double
             -> Double
             -> (Double -> Double)
             -> Int
             -> Fstruct
dichotomy a b func iter_number
    | abs(a - b) >= eps = if
        | (func  a) * (func half) > 0 -> dichotomy half b func (iter_number + 1)
        | (func half) * (func  b) > 0 -> dichotomy a half func (iter_number + 1)
        | otherwise -> answer
    | otherwise = answer

    where
        half   = (a + b) / 2        
        answer = Val {
            value     = half          ,
            iteration = iter_number
        }


simple_iter_cond :: Double          -- convergence condition 
                 -> (Double -> Double)
                 -> Bool
simple_iter_cond x f =
    let
        f' x = derivative f x
    in
        (abs (f' x)) < 1


newton_cond :: Double               -- convergence condition
               -> (Double -> Double)
               -> Bool
newton_cond x f =
    let
        f'  x = derivative f x
        f'' x = second_derivative f x
        val1  = (f'' x) * (f  x)
        val2  = (f'  x) * (f' x)
    in
        (abs val1) < val2


simple_iter_process :: Double
                       -> (Double -> Double)
                       -> Double
simple_iter_process x f = f x


newton_process :: Double
                  -> (Double -> Double)
                  -> Double
newton_process x f =
    let
        f' x = derivative f x
    in
        x - (f x) / (f' x)


common_process :: Fstruct
                  -> (Double -> Double)
                  -> (Double -> (Double -> Double) -> Double)
                  -> Fstruct
common_process answer f process
    | abs (next - (value answer)) >= eps = common_process new_answer f process
    | otherwise = new_answer

    where
        next       = process (value answer) f
        new_answer = Val { value = next, iteration = 1 + (iteration answer) }


print_answer :: String
                -> Fstruct
                -> IO ()
print_answer method_name method_answer =
    do
        putStr (method_name ++ "\n\n")

        if (iteration method_answer) == -1 then
            putStr "Error code 1: equation does not fit convergence condition\n\n\n"
        else
            putStr ("Answer: "               ++ (show $     (value method_answer)) ++ "\n\n" ++ 
                    "Number of iterations: " ++ (show $ (iteration method_answer)) ++ "\n\n\n")


check_convergence_error :: (Double -> (Double -> Double) -> Bool)
                           -> (Double -> (Double -> Double) -> Double)
                           -> Fstruct
                           -> Double
                           -> (Double -> Double)
                           -> Fstruct
check_convergence_error method_cond method_process start_args check_point func
    | method_cond check_point func = common_process start_args func method_process
    | otherwise = Val {value = 0, iteration = -1} 


solve_equation :: Double
                  -> Double
                  -> String
                  -> String
                  -> (Double -> Double)
                  -> (Double -> Double)
                  -> IO ()
solve_equation a b func given_answer equation mapping_equation =
    let
        half               = (a + b) / 2
        start_args         = Val {value = half, iteration = 0}
        
        newton_answer      = check_convergence_error newton_cond      newton_process      start_args half equation

        simple_iter_answer = check_convergence_error simple_iter_cond simple_iter_process start_args half mapping_equation 
        
        dichotomy_answer   = dichotomy a b equation 0
    in
        do
            putStr "************************************************************\n\n"

            putStr ("Equation " ++ func ++ "\n\n\n")
            
            print_answer "Newton method"           newton_answer

            print_answer "Simple iteration method" simple_iter_answer

            print_answer "Dichotomy method"        dichotomy_answer

            putStr ("Given answer: " ++ given_answer ++ "\n\n\n")

            putStr "************************************************************\n\n"


main :: IO ()
main = do
    solve_equation 1 3 "3x-14+e^x-e^(-x)" "2.0692" eq1 mapping_eq1
    solve_equation 0 1 "(1-x)^0.5-tgx"    "0.5768" eq2 mapping_eq2

