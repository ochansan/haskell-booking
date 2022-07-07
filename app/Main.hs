{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad.State (StateT, MonadState (get), MonadTrans (lift), execStateT)
import Data.Char ( isNumber, isAlpha, isPunctuation )
import Data.Time
import Control.Exception
import Control.DeepSeq

data People = People { nama :: String, mobileNumber :: String, status :: Bool}
    deriving (Show)

data ListAvailable = ListAvailable {availableserver :: People, avaialablewaktu :: String}
    deriving (Show)
data ListBooking = ListBooking {bookingserver :: People, bookingwaktu :: String, pasien :: People}
    deriving (Show)

data ModeView = Viewing | Booking | Patient deriving Show

type ListDoctors = [People]
type ListConsultants = [People]
type ListPatients = [People] 
type ListAvailables = [ListAvailable] -- Doctor/Consultant schedule
type ListBookings = [ListBooking]  -- Doctor/Consulant Patient

getDoctors :: ListDoctors -> Int -> ModeView -> IO()
getDoctors (x:xs) urut modeview = do
    let no = urut + 1
    getOneLine x no modeview 
    getDoctors xs no modeview 
getDoctors _ _ _ = putStrLn "-" 

getConsultants :: ListConsultants -> Int-> ModeView -> IO()
getConsultants (x:xs) urut modeview = do
    let no = urut + 1
    getOneLine x no modeview
    getConsultants xs no modeview
getConsultants _ _ _ = putStrLn "-"

getPatients :: ListPatients -> Int -> ModeView -> IO()
getPatients (x:xs) urut modeview = do
    let no = urut + 1
    getOneLine x no modeview
    getPatients xs no modeview
getPatients _ _ _ = putStrLn "-"

getOneLine :: People -> Int  -> ModeView -> IO()
getOneLine People { nama = vnama, mobileNumber = vhp, status = vstatus} n modeview = do
    case modeview of 
            Viewing -> do
                            if vstatus == True then
                                execStateT cetakState ((show n) ++ "\t" ++ vnama ++ "\t" ++ vhp ++ "\t" ++  " available")
                                else
                                    execStateT cetakState ("*" ++ "\t" ++ vnama ++ "\t" ++ vhp ++ "\t" ++  " not available")
                            return()
            Booking ->  do
                            if vstatus == True then 
                                execStateT cetakState ((show n) ++ "\t" ++ vnama ++ "\t" ++ vhp ++ "\t") 
                                else 
                                    execStateT cetakState ("") 
                            return()
            Patient ->  do
                            if vstatus == True then 
                                execStateT cetakState ((show n) ++ "\t" ++ vnama ++ "\t" ++ vhp ++ "\t") 
                                else 
                                    execStateT cetakState ("") 
                            return()
    return()

getPeopleName :: People -> String 
getPeopleName People { nama = vnama, mobileNumber = vhp, status = vstatus} = vnama

getListAvailable :: ListAvailables -> Int -> ModeView -> IO()
getListAvailable (x:xs) urut modeview = do
    let no = urut + 1
    getOneLineAvailable x no modeview 
    getListAvailable xs no modeview 
getListAvailable _ _ _ = putStrLn "-" 

getOneLineAvailable :: ListAvailable -> Int  -> ModeView -> IO()
getOneLineAvailable ListAvailable {availableserver = vserver, avaialablewaktu = vwaktu} n modeview = do
    case modeview of 
        Viewing -> do
            execStateT cetakState (getPeopleName vserver ++ "\t" ++ vwaktu)
            return()
        Booking ->  do
            execStateT cetakState ""
            return()
        Patient ->  do
            execStateT cetakState ""
            return()
    return()

getListBooking :: ListBookings -> Int -> ModeView -> IO()
getListBooking (x:xs) urut modeview = do
    let no = urut + 1
    getOneLineBooking x no modeview 
    getListBooking xs no modeview 
getListBooking _ _ _ = putStrLn "-" 

getOneLineBooking :: ListBooking -> Int  -> ModeView -> IO()
getOneLineBooking ListBooking {bookingserver = vserver, bookingwaktu = vwaktu} n modeview = do
    case modeview of 
        Viewing -> do
            execStateT cetakState (getPeopleName vserver ++ "\t" ++ vwaktu)
            return()
        Booking ->  do
            execStateT cetakState ""
            return()
        Patient ->  do
            execStateT cetakState ""
            return()
    return()


availableDoctors :: [People]
availableDoctors = [
    (People { nama = "Dr Jon" , mobileNumber = "081512345678931", status = True}),
    (People { nama = "Dr Dre" , mobileNumber = "081512345678932", status = False}),
    (People { nama = "Dr Tom" , mobileNumber = "081512345678933", status = True})
    ]

availableConsultants :: [People]
availableConsultants = [
    (People { nama = "Mr Hawk" , mobileNumber = "081512345678921", status = True}),
    (People { nama = "Ms Jean" , mobileNumber = "081512345678922", status = True}),
    (People { nama = "Mr Hunt" , mobileNumber = "081512345678923", status = False})
    ]

bookingPatients :: [People]
bookingPatients = [
    (People { nama = "Mr Lee" , mobileNumber = "081512345678911", status = True}),
    (People { nama = "Ms Tee" , mobileNumber = "081512345678912", status = True}),
    (People { nama = "Mr Man" , mobileNumber = "081512345678913", status = True})
    ]

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
    execStateT cetakState "===================="
    getDoctors availableDoctors 0 Viewing
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
    execStateT cetakState "========================"
    getDoctors availableConsultants 0 Viewing
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
    execStateT cetakState "================"
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
    getDoctors availableDoctors 0 Booking
    execStateT cetakState "Choose:"
    pilihandokter <- getLine
    patientname <- getLine
    appendBooking (pilihandokter ++ patientname)
    execStateT cetakState "\ESC[32mInput Saved"
    execStateT cetakState "\ESC[0m"
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
    execStateT cetakState "==============="
    getDoctors availableConsultants 0 Booking
    execStateT cetakState "Choose:"
    pilihanconnsultant <- getLine
    execStateT cetakState "Enter Patient Name:"
    patientname <- getLine
    appendBooking (pilihanconnsultant ++ patientname)
    execStateT cetakState "\ESC[32mInput Saved"
    execStateT cetakState "\ESC[0m"
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
    execStateT cetakState "Appointment"
    execStateT cetakState "=============="
    getPatients bookingPatients 0 Patient 
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

getPassword :: IO (Maybe String)
getPassword = do 
    s <- getLine
    if isValid s then return $ Just s 
    else return Nothing 
    
isValid :: String -> Bool
isValid s = if s == "inipassword" then True else False

adminmenu :: IO()
adminmenu = do 
    execStateT cetakState "Please enter Admin password"
    maybe_value <- getPassword 
    case maybe_value of
        Just value -> do putStrLn "Password accepted"
        Nothing -> do
            putStrLn "Password invalid"
            adminmenu
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

appendLog :: String -> IO()
appendLog s = do 
    let namafile = "log.txt " 
    let isifile =  "Log" ++ s ++ "\n"
    appendFile namafile isifile
    return()

appendSchedule :: String -> IO()
appendSchedule s = do 
    let namafile = "schedules.txt " 
    let isifile =  s ++ "\n"
    appendFile namafile isifile
    return()

appendBooking :: String -> IO()
appendBooking s = do 
    let namafile = "booking.txt " 
    let isifile =  s ++ "\n"
    appendFile namafile isifile
    return()

getMonth :: String -> String
getMonth s = case s of
                "1" -> "Jan"
                "2" -> "Feb"
                "3" -> "Mar"
                "4" -> "Apr"
                "5" -> "May"
                "6" -> "Jun"
                "7" -> "Jul"
                "8" -> "Aug"
                "9" -> "Sep"
                "10" -> "Oct"
                "11" -> "Nov"
                "12" -> "Dec"
                _ -> ""

getSlot :: String -> String
getSlot s = case s of 
            "1" -> "9AM - 10AM"
            "2" -> "10AM - 11AM"
            "3" -> "11AM - 12nn"
            "4" -> "2PM - 3PM"
            "5" -> "3PM - 4PM"
            "6" -> "4PM - 5PM"
            _ -> ""

adminaddmenu :: IO()
adminaddmenu = do 
    execStateT cetakState "Admin Only - Add Schedule"
    execStateT cetakState "========================="
    execStateT cetakState "Do you want to add [D]octor or [C]onsultant schedule ?"
    pilihanTipe <- getLine
    case pilihanTipe of 
      [] -> invalidmenu
      c : s -> if c == 'd' || c == 'D' then 
        getDoctors availableDoctors 0 Viewing else 
        getConsultants availableConsultants 0 Viewing 
    let savetipe = case pilihanTipe of
            "D" -> "Doctor"
            "C" -> "Consultant"
            _ -> ""
    execStateT cetakState "Choose : "
    pilihanJadwal <- getLine
    execStateT cetakState "Enter Month (1-12)"
    pilihanMonth <- getLine
    let savemonth = getMonth pilihanMonth 
            
    execStateT cetakState "Enter Date (1-31)"
    pilihanDate <- getLine
    execStateT cetakState "Select Timeslot"
    execStateT cetakState "[1] 9AM - 10AM"
    execStateT cetakState "[2] 10AM - 11AM"
    execStateT cetakState "[3] 11AM - 12nn"
    execStateT cetakState "[4] 2PM - 3PM"
    execStateT cetakState "[5] 3PM - 4PM"
    execStateT cetakState "[6] 4PM - 5PM"
    pilihanSlot <- getLine
    let saveslot = getSlot pilihanSlot

    appendSchedule (savetipe ++ "\t" ++ pilihanJadwal ++ "\t" ++ pilihanDate ++ "\t" ++ savemonth ++ "\t" ++ saveslot)
    appendLog "Add New Schedule"
    execStateT cetakState "\ESC[32mInput Saved"
    execStateT cetakState "\ESC[0m"
    execStateT cetakState "(b) back to menu"
    execStateT cetakState "(h) back to home menu"
    pilihan <- getLine
    appendLog "Add schedule"
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
    appendLog "Remove schedule"
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

-- getZonedTime :: IO ZonedTime

homemenu :: IO()
homemenu = do 
    -- execStateT cetakState Date.getCurTime
    appendLog "Starting"
    execStateT cetakState "\ESC[35m===================================\ESC[0m"
    execStateT cetakState "Welcome to Appointment Booking Apps" 
    execStateT cetakState "\ESC[35m===================================\ESC[0m"
    execStateT cetakState "Choose Menu: "
    execStateT cetakState "(1) View Schedule"
    execStateT cetakState "(2) Book Appointment"
    execStateT cetakState "(3) View Appointment"
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
    