import System.Process
import System.Exit

main = do
  bug@(bugcode, bugout, bugerr) <- readProcessWithExitCode "dist/build/bug/bug" [] ""
  nobug@(nobugcode, nobugout, nobugerr) <- readProcessWithExitCode "dist/build/nobug/nobug" [] ""
  case bug == nobug of
    True -> exitWith ExitSuccess
    False -> putStrLn ("Expected:\n  " ++ show nobug ++ "\nActual:\n  " ++ show bug) >> exitWith (ExitFailure 1)
