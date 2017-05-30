import Control.Exception
import System.IO
import System.IO.Error

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
menu data_file = do
              putStrLn "Tic Tae Toe Hs"
              return data_file
