module Main where

import Control.Monad.State (StateT, MonadState (get), MonadTrans (lift), execStateT)
--import System.Process

main :: IO ()
main = do
    putStr "\ESC[2J" 
    homemenu
    return ()

cetakState :: StateT String IO ()
cetakState = do 
    state <- get
    lift $ putStrLn (state)
    return()

viewmenu :: IO()
viewmenu = do 
    execStateT cetakState "View Schedule"
    execStateT cetakState "============="
    execStateT cetakState "(1) View Doctor"
    execStateT cetakState "(2) View Consultant"
    execStateT cetakState "(b) back to menu"
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
    execStateT cetakState "View Doctor Schedule"
    execStateT cetakState "============="
    execStateT cetakState "(b) back to menu"
    execStateT cetakState "(h) back to homemenu"
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
    execStateT cetakState "View Consultant Schedule"
    execStateT cetakState "============="
    execStateT cetakState "(b) back to menu"
    execStateT cetakState "(h) back to homemenu"
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
    execStateT cetakState "Book Appointment"
    execStateT cetakState "============="
    execStateT cetakState "(1) Book Doctor"
    execStateT cetakState "(2) Book Consultant"
    execStateT cetakState "(b) back to menu"
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
    execStateT cetakState "Book Doctor"
    execStateT cetakState "============="
    -- list available doctr
    execStateT cetakState "(b) back to menu"
    execStateT cetakState "(h) back to home menu"
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
    execStateT cetakState "Book Consultant"
    execStateT cetakState "============="
    -- list available consultant
    execStateT cetakState "(b) back to menu"
    execStateT cetakState "(h) back to home menu"
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
    execStateT cetakState "My Appointment"
    execStateT cetakState "============="
    -- appointment list
    execStateT cetakState "(b) back to menu"
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
    execStateT cetakState "Admin Only"
    execStateT cetakState "=========="
    execStateT cetakState "(1) Add Schedule"
    execStateT cetakState "(2) Remove Schedule"
    execStateT cetakState "(b) back to menu"
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
    execStateT cetakState "Admin Only - Add Schedule"
    execStateT cetakState "========================="
    execStateT cetakState "Do you want to add [D]octor or [C]onsultant schedule ?"
    pilihan <- getLine
    execStateT cetakState "Enter Date"
    pilihan <- getLine
    execStateT cetakState "Enter Month"
    pilihan <- getLine
    execStateT cetakState "Select Timeslot"
    execStateT cetakState "[1] 9AM - 10AM"
    execStateT cetakState "[2] 10AM - 11AM"
    execStateT cetakState "[3] 11AM - 12nn"
    execStateT cetakState "[4] 2PM - 3PM"
    execStateT cetakState "[5] 3PM - 4PM"
    execStateT cetakState "[6] 4PM - 5PM"
    pilihan <- getLine
    execStateT cetakState "\ESC[32mInput Saved"
    execStateT cetakState "\ESC[0m"
    execStateT cetakState "(b) back to menu"
    execStateT cetakState "(h) back to home menu"
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
    execStateT cetakState "Admin Only - Remove Schedule"
    execStateT cetakState "========================="
    execStateT cetakState "Do you want to remove [D]octor or [C]onsultant schedule ?"
    pilihan <- getLine
    execStateT cetakState "Choose To Remove"
    execStateT cetakState "[1] 9AM - 10AM"
    execStateT cetakState "[2] 10AM - 11AM"
    execStateT cetakState "[3] 11AM - 12nn"
    execStateT cetakState "[4] 2PM - 3PM"
    execStateT cetakState "[5] 3PM - 4PM"
    execStateT cetakState "[6] 4PM - 5PM"
    pilihan <- getLine
    execStateT cetakState "\ESC[35mSchedule Removed"
    execStateT cetakState "\ESC[0m"
    execStateT cetakState "(b) back to menu"
    execStateT cetakState "(h) back to home menu"
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
    execStateT cetakState "Welcome to Appointment Booking Apps"
    execStateT cetakState "==================================="
    execStateT cetakState "Choose Menu: "
    execStateT cetakState "(1) View Schedule"
    execStateT cetakState "(2) Book Appointment"
    execStateT cetakState "(3) View My Appointment"
    execStateT cetakState "(4) Admin Only"
    execStateT cetakState "(q) Quit"
    pilihan <- getLine
    case pilihan of 
        "1" -> viewmenu
        "2" -> bookmenu
        "3" -> myappointmentmenu
        "4" -> adminmenu
        "q" -> do
            execStateT cetakState "\ESC[35mThank You for using our service. Have a nice day"
            execStateT cetakState "\ESC[0m"
            return ()
        _ -> do 
            invalidmenu
            homemenu
            return()
    return ()