import Control.Exception
import System.IO
import System.IO.Error
import System.Process

type Players = [Player]
type Name = String
type Points = Int
type Table = [Char]
type V = Int
data Player = Player Name Points
                deriving(Show, Read)

fileLoad:: IO()
fileLoad = do
        { catch (read_file) error_handle }
        where
          read_file = do
                        file <- openFile "data.txt" ReadMode;
                        data_file <- hGetLine file;
                        hClose file;
                        menu (read data_file);
                        return ();
          error_handle error = if isDoesNotExistError error then
            do
              file <- openFile "datas.txt" WriteMode;
              hPutStrLn file "[]";
              hClose file;
              menu [];
              return ();
          else
            ioError error

menu :: Players -> IO Players
menu _data = do
              system "clear"
              putStrLn "Tic Tae Toe Hs"
              putStrLn "\n Type 1 to register user"
              putStrLn "\n Type 2 to play"
              putStrLn "\n Type 3 to show raking"
              putStrLn "\n Type 0 to exit"
              putStrLn "\n Options:"
              op <- getChar
              executeOptions _data op

executeOptions :: Players -> Char -> IO Players
executeOptions _data '0' = do
                            putStrLn "\n Bye"
                            return _data
executeOptions _data _ = do
                            putStrLn "\n Invalid option, try again..."
                            putStr "\nPress <Enter> to back to the menu..."
                            getChar
                            menu _data
