module Tree.Test

import Tree.Fold

main : IO ()
main = do
  putStrLn name
  putStrLn ""
  putStrLn "=== Test Cases ==="
  putStrLn ""
  
  putStrLn "1. Pure numbers - should fully evaluate"
  putStrLn ("Original: " ++ show (Prod (Sum (Num 5) (Num 3)) (Sum (Num 2) (Num 4))))
  let (s1 ** n1) = normalize Number (Prod (Sum (Num 5) (Num 3)) (Sum (Num 2) (Num 4)))
  putStrLn ("Normalized: " ++ show n1 ++ " (Status: " ++ show s1 ++ ")")
  putStrLn ""
  
  putStrLn "2. Number with one variable"
  putStrLn ("Original: " ++ show (Sum (Prod (Num 2) (Num 3)) (Var X)))
  let (s2 ** n2) = normalize Mix (Sum (Prod (Num 2) (Num 3)) (Var X))
  putStrLn ("Normalized: " ++ show n2 ++ " (Status: " ++ show s2 ++ ")")
  putStrLn ""
  
  putStrLn "3. Mixed with nested operations"
  putStrLn ("Original: " ++ show (Prod (Sum (Num 5) (Num 3)) (Sum (Var X) (Num 2))))
  let (s3 ** n3) = normalize Mix (Prod (Sum (Num 5) (Num 3)) (Sum (Var X) (Num 2)))
  putStrLn ("Normalized: " ++ show n3 ++ " (Status: " ++ show s3 ++ ")")
  putStrLn ""
  
  putStrLn "4. All variables"
  putStrLn ("Original: " ++ show (Prod (Var X) (Var Y)))
  let (s4 ** n4) = normalize Variable (Prod (Var X) (Var Y))
  putStrLn ("Normalized: " ++ show n4 ++ " (Status: " ++ show s4 ++ ")")
  putStrLn ""
  
  putStrLn "5. Deep nesting - fully evaluate"
  putStrLn ("Original: " ++ show (Prod (Prod (Num 2) (Num 3)) (Prod (Num 4) (Num 5))))
  let (s5 ** n5) = normalize Number (Prod (Prod (Num 2) (Num 3)) (Prod (Num 4) (Num 5)))
  putStrLn ("Normalized: " ++ show n5 ++ " (Status: " ++ show s5 ++ ")")
  putStrLn ""
  
  putStrLn "6. Variable subtree blocked from evaluation"
  putStrLn ("Original: " ++ show (Prod (Prod (Var X) (Var Y)) (Prod (Num 5) (Num 3))))
  let (s6 ** n6) = normalize Mix (Prod (Prod (Var X) (Var Y)) (Prod (Num 5) (Num 3)))
  putStrLn ("Normalized: " ++ show n6 ++ " (Status: " ++ show s6 ++ ")")
  putStrLn ""
  
  putStrLn "7. Multiple variables scattered"
  putStrLn ("Original: " ++ show (Sum (Prod (Num 2) (Var X)) (Prod (Num 3) (Var Y))))
  let (s7 ** n7) = normalize Mix (Sum (Prod (Num 2) (Var X)) (Prod (Num 3) (Var Y)))
  putStrLn ("Normalized: " ++ show n7 ++ " (Status: " ++ show s7 ++ ")")
  putStrLn ""
  
  putStrLn "8. Nested variables with numbers"
  putStrLn ("Original: " ++ show (Prod (Sum (Var X) (Num 1)) (Sum (Var Y) (Num 2))))
  let (s8 ** n8) = normalize Mix (Prod (Sum (Var X) (Num 1)) (Sum (Var Y) (Num 2)))
  putStrLn ("Normalized: " ++ show n8 ++ " (Status: " ++ show s8 ++ ")")
  putStrLn ""
  
  putStrLn "9. All numbers addition and multiplication"
  putStrLn ("Original: " ++ show (Sum (Prod (Num 2) (Num 3)) (Prod (Num 4) (Num 5))))
  let (s9 ** n9) = normalize Number (Sum (Prod (Num 2) (Num 3)) (Prod (Num 4) (Num 5)))
  putStrLn ("Normalized: " ++ show n9 ++ " (Status: " ++ show s9 ++ ")")
  putStrLn ""
  
  putStrLn "10. Deep nesting with variable at leaf"
  putStrLn ("Original: " ++ show (Prod (Prod (Num 2) (Num 3)) (Prod (Num 4) (Var X))))
  let (s10 ** n10) = normalize Mix (Prod (Prod (Num 2) (Num 3)) (Prod (Num 4) (Var X)))
  putStrLn ("Normalized: " ++ show n10 ++ " (Status: " ++ show s10 ++ ")")
  putStrLn ""
  
  pure()
  putStrLn "=== Basic Cases ==="
  putStrLn ""
  
  putStrLn "Example 1: Pure numbers - should fully evaluate"
  putStrLn ("Original: " ++ show (Prod (Sum (Num 5) (Num 3)) (Sum (Num 2) (Num 4))))
  let (s1 ** n1) = normalize Number (Prod (Sum (Num 5) (Num 3)) (Sum (Num 2) (Num 4)))
  putStrLn ("Normalized: " ++ show n1 ++ " (Status: " ++ show s1 ++ ")")
  putStrLn ""
  
  putStrLn "Example 2: Number with one variable"
  putStrLn ("Original: " ++ show (Sum (Prod (Num 2) (Num 3)) (Var X)))
  let (s2 ** n2) = normalize Mix (Sum (Prod (Num 2) (Num 3)) (Var X))
  putStrLn ("Normalized: " ++ show n2 ++ " (Status: " ++ show s2 ++ ")")
  putStrLn ""
  
  putStrLn "Example 3: Mixed with nested operations"
  putStrLn ("Original: " ++ show (Prod (Sum (Num 5) (Num 3)) (Sum (Var X) (Num 2))))
  let (s3 ** n3) = normalize Mix (Prod (Sum (Num 5) (Num 3)) (Sum (Var X) (Num 2)))
  putStrLn ("Normalized: " ++ show n3 ++ " (Status: " ++ show s3 ++ ")")
  putStrLn ""
  
  putStrLn "Example 4: All variables"
  putStrLn ("Original: " ++ show (Prod (Var X) (Var Y)))
  let (s4 ** n4) = normalize Variable (Prod (Var X) (Var Y))
  putStrLn ("Normalized: " ++ show n4 ++ " (Status: " ++ show s4 ++ ")")
  putStrLn ""
  
  putStrLn "=== Complex Cases ==="
  putStrLn ""
  
  putStrLn "Example 5: Deep nesting - should fully evaluate"
  putStrLn ("Original: " ++ show (Prod (Prod (Num 2) (Num 3)) (Prod (Num 4) (Num 5))))
  let (s5 ** n5) = normalize Number (Prod (Prod (Num 2) (Num 3)) (Prod (Num 4) (Num 5)))
  putStrLn ("Normalized: " ++ show n5 ++ " (Status: " ++ show s5 ++ ")")
  putStrLn ""
  
  putStrLn "Example 6: Deep nesting with variable at leaf"
  putStrLn ("Original: " ++ show (Prod (Prod (Num 2) (Num 3)) (Prod (Num 4) (Var X))))
  let (s6 ** n6) = normalize Mix (Prod (Prod (Num 2) (Num 3)) (Prod (Num 4) (Var X)))
  putStrLn ("Normalized: " ++ show n6 ++ " (Status: " ++ show s6 ++ ")")
  putStrLn ""
  
  putStrLn "Example 7: Variable subtree blocked from evaluation"
  putStrLn ("Original: " ++ show (Prod (Prod (Var X) (Var Y)) (Prod (Num 5) (Num 3))))
  let (s7 ** n7) = normalize Mix (Prod (Prod (Var X) (Var Y)) (Prod (Num 5) (Num 3)))
  putStrLn ("Normalized: " ++ show n7 ++ " (Status: " ++ show s7 ++ ")")
  putStrLn ""
  
  putStrLn "Example 8: Multiple variables scattered"
  putStrLn ("Original: " ++ show (Sum (Prod (Num 2) (Var X)) (Prod (Num 3) (Var Y))))
  let (s8 ** n8) = normalize Mix (Sum (Prod (Num 2) (Var X)) (Prod (Num 3) (Var Y)))
  putStrLn ("Normalized: " ++ show n8 ++ " (Status: " ++ show s8 ++ ")")
  putStrLn ""
  
  putStrLn "Example 9: All numbers with addition and multiplication"
  putStrLn ("Original: " ++ show (Sum (Prod (Num 2) (Num 3)) (Prod (Num 4) (Num 5))))
  let (s9 ** n9) = normalize Number (Sum (Prod (Num 2) (Num 3)) (Prod (Num 4) (Num 5)))
  putStrLn ("Normalized: " ++ show n9 ++ " (Status: " ++ show s9 ++ ")")
  putStrLn ""
  
  putStrLn "Example 10: Nested variables with numbers"
  putStrLn ("Original: " ++ show (Prod (Sum (Var X) (Num 1)) (Sum (Var Y) (Num 2))))
  let (s10 ** n10) = normalize Mix (Prod (Sum (Var X) (Num 1)) (Sum (Var Y) (Num 2)))
  putStrLn ("Normalized: " ++ show n10 ++ " (Status: " ++ show s10 ++ ")")
  putStrLn ""
  
  putStrLn "=== Edge Cases ==="
  putStrLn ""
  
  putStrLn "Example 11: Single number"
  putStrLn ("Original: " ++ show (Num 42))
  let (s11 ** n11) = normalize Number (Num 42)
  putStrLn ("Normalized: " ++ show n11 ++ " (Status: " ++ show s11 ++ ")")
  putStrLn ""
  
  putStrLn "Example 12: Single variable"
  putStrLn ("Original: " ++ show (Var Z))
  let (s12 ** n12) = normalize Variable (Var Z)
  putStrLn ("Normalized: " ++ show n12 ++ " (Status: " ++ show s12 ++ ")")
  putStrLn ""
  
  putStrLn "Example 13: Sum of two variables"
  putStrLn ("Original: " ++ show (Sum (Var X) (Var Y)))
  let (s13 ** n13) = normalize Variable (Sum (Var X) (Var Y))
  putStrLn ("Normalized: " ++ show n13 ++ " (Status: " ++ show s13 ++ ")")
  putStrLn ""
  
  putStrLn "Example 14: Addition of pure numbers with zero"
  putStrLn ("Original: " ++ show (Sum (Num 0) (Num 5)))
  let (s14 ** n14) = normalize Number (Sum (Num 0) (Num 5))
  putStrLn ("Normalized: " ++ show n14 ++ " (Status: " ++ show s14 ++ ")")
  putStrLn ""
  
  putStrLn "Example 15: Multiplication by zero with variable"
  putStrLn ("Original: " ++ show (Prod (Num 0) (Var X)))
  let (s15 ** n15) = normalize Mix (Prod (Num 0) (Var X))
  putStrLn ("Normalized: " ++ show n15 ++ " (Status: " ++ show s15 ++ ")")
  putStrLn ""
  
  putStrLn "Example 16: Deep nesting with mixed variables"
  putStrLn ("Original: " ++ show (Prod (Sum (Prod (Num 2) (Var X)) (Num 3)) (Sum (Var Y) (Num 4))))
  let (s16 ** n16) = normalize Mix (Prod (Sum (Prod (Num 2) (Var X)) (Num 3)) (Sum (Var Y) (Num 4)))
  putStrLn ("Normalized: " ++ show n16 ++ " (Status: " ++ show s16 ++ ")")
  putStrLn ""
  
  putStrLn "Example 17: Right-associated nesting"
  putStrLn ("Original: " ++ show (Prod (Num 2) (Prod (Num 3) (Prod (Num 4) (Var X)))))
  let (s17 ** n17) = normalize Mix (Prod (Num 2) (Prod (Num 3) (Prod (Num 4) (Var X))))
  putStrLn ("Normalized: " ++ show n17 ++ " (Status: " ++ show s17 ++ ")")
  putStrLn ""
  
  putStrLn "Example 18: Left-associated nesting"
  putStrLn ("Original: " ++ show (Prod (Prod (Prod (Num 2) (Num 3)) (Num 4)) (Var X)))
  let (s18 ** n18) = normalize Mix (Prod (Prod (Prod (Num 2) (Num 3)) (Num 4)) (Var X))
  putStrLn ("Normalized: " ++ show n18 ++ " (Status: " ++ show s18 ++ ")")
  putStrLn ""
  
  pure()

