import qualified Network.Socket as S
import qualified System.IO as I
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Chan as Ch
import Control.Monad.Fix (fix)

type Msg = String

main ::  IO ()
main = do
  sock <- S.socket S.AF_INET S.Stream 0
  S.setSocketOption sock S.ReuseAddr 1
  S.bindSocket sock (S.SockAddrInet 4242 S.iNADDR_ANY)
  S.listen sock 2
  chan <- Ch.newChan
  mainLoop sock chan

mainLoop :: S.Socket -> Ch.Chan Msg -> IO ()
mainLoop sock chan = do
  conn <- S.accept sock
  newChan <- Ch.dupChan chan
  C.forkIO (runConn conn newChan)
  mainLoop sock chan

runConn :: (S.Socket, S.SockAddr) -> Ch.Chan Msg -> IO ()
runConn (sock, _) chan = do
  hdl <- S.socketToHandle sock I.ReadWriteMode
  I.hSetBuffering hdl I.NoBuffering
  C.forkIO $ loop $ writeToSock chan hdl
  C.forkIO $ loop $ writeToChan chan hdl
  return ()

writeToSock :: Ch.Chan Msg -> I.Handle -> IO ()
writeToSock chan pipe = do
  msg <- Ch.readChan chan
  I.hPutStrLn pipe msg

writeToChan :: Ch.Chan Msg -> I.Handle -> IO () 
writeToChan chan pipe = do
    line <- I.hGetLine pipe
    Ch.writeChan chan line

loop :: IO () -> IO ()
loop thingToDo = fix (\loopa -> thingToDo >> loopa)
