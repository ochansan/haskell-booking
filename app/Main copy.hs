module Main where
    
--import Control.Monad.State (StateT, MonadState (get), MonadTrans (lift), execStateT)
import System.Console.ANSI

clearScreen 

main :: IO ()
main = homemenu

viewmenu :: IO()
viewmenu = do 
    putStrLn "View Schedule"
    putStrLn "============="
    putStrLn "(1) View Doctor"
    putStrLn "(2) View Consultant"
    putStrLn "(b) back to menu"
    pilihan <- getLine
    case pilihan of 
        "1" -> doctormenu
        "2" -> consultantmenu
        "b" -> homemenu
        _   -> do
            invalidmenu
            viewmenu
            return()
    return ()

doctormenu :: IO()
doctormenu = do 
    putStrLn "View Doctor Schedule"
    putStrLn "============="
    putStrLn "(b) back to menu"
    putStrLn "(h) back to homemenu"
    pilihan <- getLine
    case pilihan of 
        "b" -> viewmenu
        "h" -> homemenu
        _   -> do
            invalidmenu
            doctormenu
            return()
    return ()

consultantmenu :: IO()
consultantmenu = do 
    putStrLn "View Consultant Schedule"
    putStrLn "============="
    putStrLn "(b) back to menu"
    putStrLn "(h) back to homemenu"
    pilihan <- getLine
    case pilihan of 
        "b" -> viewmenu
        "h" -> homemenu
        _   -> do
            invalidmenu
            consultantmenu
            return()
    return ()

bookmenu :: IO()
bookmenu = do 
    putStrLn "Book Appointment"
    putStrLn "============="
    putStrLn "(1) Book Doctor"
    putStrLn "(2) Book Consultant"
    putStrLn "(b) back to menu"
    pilihan <- getLine
    case pilihan of 
        "1" -> bookdoctormenu
        "2" -> bookconsultantmenu
        "b" -> homemenu
        _   -> do
            invalidmenu
            bookmenu
            return()
    return ()

bookdoctormenu :: IO()
bookdoctormenu = do 
    putStrLn "Book Doctor"
    putStrLn "============="
    -- list available doctr
    putStrLn "(b) back to menu"
    putStrLn "(h) back to home menu"
    pilihan <- getLine
    case pilihan of 
        "1" -> bookdoctormenu
        "2" -> bookconsultantmenu
        "b" -> bookmenu
        "h" -> homemenu
        _   -> do
            invalidmenu
            bookdoctormenu
            return()
    return ()

bookconsultantmenu :: IO()
bookconsultantmenu = do 
    putStrLn "Book Consultant"
    putStrLn "============="
    -- list available consultant
    putStrLn "(b) back to menu"
    putStrLn "(h) back to home menu"
    pilihan <- getLine
    case pilihan of 
        "b" -> bookmenu
        "h" -> homemenu
        _   -> do
            invalidmenu
            bookconsultantmenu
            return()
    return ()

myappointmentmenu :: IO()
myappointmentmenu = do 
    putStrLn "My Appointment"
    putStrLn "============="
    -- appointment list
    putStrLn "(b) back to menu"
    pilihan <- getLine
    case pilihan of 
        "1" -> bookdoctormenu
        "2" -> bookconsultantmenu
        "b" -> homemenu
        _   -> do
            invalidmenu
            myappointmentmenu
            return()
    return ()

adminmenu :: IO()
adminmenu = do 
    putStrLn "Admin Only"
    putStrLn "=========="
    putStrLn "(1) Add Schedule"
    putStrLn "(2) Remove Schedule"
    putStrLn "(b) back to menu"
    pilihan <- getLine
    case pilihan of 
        "1" -> adminaddmenu
        "2" -> adminremovemenu
        "b" -> homemenu
        _   -> do 
            invalidmenu
            adminmenu
            return()
    return ()

adminaddmenu :: IO()
adminaddmenu = do 
    putStrLn "Admin Only - Add Schedule"
    putStrLn "========================="
    putStrLn "Do you want to add [D]octor or [C]onsultant schedule ?"
    pilihan <- getLine
    putStrLn "Enter Date"
    pilihan <- getLine
    putStrLn "Enter Month"
    pilihan <- getLine
    putStrLn "Select Timeslot"
    putStrLn "[1] 9AM - 10AM"
    putStrLn "[2] 10AM - 11AM"
    putStrLn "[3] 11AM - 12nn"
    putStrLn "[4] 2PM - 3PM"
    putStrLn "[5] 3PM - 4PM"
    putStrLn "[6] 4PM - 5PM"
    pilihan <- getLine
    putStrLn "\ESC[32mInput Saved"
    putStrLn "\ESC[0m"
    putStrLn "(b) back to menu"
    putStrLn "(h) back to home menu"
    pilihan <- getLine
    case pilihan of 
        "b" -> adminmenu
        "h" -> homemenu
        _   -> do 
            invalidmenu
            adminaddmenu
            return()
    return ()

adminremovemenu :: IO()
adminremovemenu = do 
    putStrLn "Admin Only - Remove Schedule"
    putStrLn "========================="
    putStrLn "Do you want to remove [D]octor or [C]onsultant schedule ?"
    pilihan <- getLine
    putStrLn "Choose To Remove"
    putStrLn "[1] 9AM - 10AM"
    putStrLn "[2] 10AM - 11AM"
    putStrLn "[3] 11AM - 12nn"
    putStrLn "[4] 2PM - 3PM"
    putStrLn "[5] 3PM - 4PM"
    putStrLn "[6] 4PM - 5PM"
    pilihan <- getLine
    putStrLn "\ESC[35mSchedule Removed"
    putStrLn "\ESC[0m"
    putStrLn "(b) back to menu"
    putStrLn "(h) back to home menu"
    pilihan <- getLine
    case pilihan of 
        "b" -> adminmenu
        "h" -> homemenu
        _   -> do 
            invalidmenu
            adminremovemenu
            return()
    return ()

invalidmenu :: IO()
invalidmenu = do
    putStrLn "\ESC[31mIncorrect input, please choose from menu"
    putStrLn "\ESC[0m"
    return ()

homemenu :: IO()
homemenu = do 
    putStrLn "Welcome to Appointment Booking Apps"
    putStrLn "==================================="
    putStrLn "Choose Menu: "
    putStrLn "(1) View Schedule"
    putStrLn "(2) Book Appointment"
    putStrLn "(3) View My Appointment"
    putStrLn "(4) Admin Only"
    putStrLn "(q) Quit"
    pilihan <- getLine
    case pilihan of 
        "1" -> viewmenu
        "2" -> bookmenu
        "3" -> myappointmentmenu
        "4" -> adminmenu
        "q" -> do
            putStrLn "\ESC[35mThank You for using our service. Have a nice day"
            putStrLn "\ESC[0m"
            return ()
        _ -> do 
            invalidmenu
            homemenu
            return()
    return ()