module Pal.Shader where

import Pal.Types

import Graphics.GL

import Control.Monad
import Control.Monad.Trans

import Foreign
import Foreign.C.String

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import Data.Text (Text)

useProgram :: MonadIO m => Program -> m ()
useProgram (Program prog) = glUseProgram (fromIntegral prog)

---------------
-- Load shaders
---------------

-- | Takes the raw source of a pair of shader programs. 
-- Useful when getting shader source from somewhere other than a file,
-- or for munging shader source before compiling it.
createShaderProgramFromSources :: String -> Text -> String -> Text -> IO Program
createShaderProgramFromSources vertexShaderName vertexShaderSource fragmentShaderName fragmentShaderSource = do 
  
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  compileShaderSource vertexShaderName vertexShaderSource vertexShader

  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  compileShaderSource fragmentShaderName fragmentShaderSource fragmentShader

  attachProgram vertexShader fragmentShader



createShaderProgram :: FilePath -> FilePath -> IO Program
createShaderProgram vertexShaderPath fragmentShaderPath = do 
  
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  compileShaderAtPath vertexShaderPath vertexShader

  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  compileShaderAtPath fragmentShaderPath fragmentShader

  attachProgram vertexShader fragmentShader



attachProgram :: GLuint -> GLuint -> IO Program
attachProgram vertexShader fragmentShader = do

  prog <- glCreateProgram

  glAttachShader prog vertexShader
  glAttachShader prog fragmentShader

  glLinkProgram prog

  checkLinkStatus prog
  
  return (Program prog)




compileShaderAtPath :: FilePath -> GLuint -> IO ()
compileShaderAtPath path shader = do

  src <- Text.readFile path

  compileShaderSource path src shader 

compileShaderSource :: String -> Text -> GLuint -> IO ()
compileShaderSource path src shader = do
  
  BS.useAsCString (Text.encodeUtf8 src) $ \ptr ->
    withArray [ptr] $ \srcs ->
      glShaderSource shader 1 srcs nullPtr

  glCompileShader shader
  
  checkCompileStatus path shader




getShaderAttribute :: Program -> String -> IO AttributeLocation
getShaderAttribute (Program prog) attributeName = do

  location <- withCString attributeName $ \attributeNameCString -> 
    glGetAttribLocation prog attributeNameCString

  when (location == -1) $ 
    putStrLn $ "Couldn't bind attribute: " ++ attributeName 
      ++ " - ignoring since it might have just been optimized out"

  return (AttributeLocation location)



getShaderUniform :: Program -> String -> IO UniformLocation
getShaderUniform (Program prog) uniformName = do

  location <- withCString uniformName $ \uniformNameCString -> 
    glGetUniformLocation prog uniformNameCString

  when (location == -1) $ 
    putStrLn $ "Couldn't bind uniform: " ++ uniformName 
      ++ " - ignoring since it might have just been optimized out"

  return (UniformLocation location)

glGetErrors :: IO ()
glGetErrors = do

  code <- glGetError

  case code of

    GL_NO_ERROR -> return ()

    e -> do

      case e of

        GL_INVALID_ENUM                   -> putStrLn "* Invalid Enum"
        GL_INVALID_VALUE                  -> putStrLn "* Invalid Value"
        GL_INVALID_OPERATION              -> putStrLn "* Invalid Operation"
        GL_INVALID_FRAMEBUFFER_OPERATION  -> putStrLn "* Invalid Framebuffer Operation"
        GL_OUT_OF_MEMORY                  -> putStrLn "* OOM"
        GL_STACK_UNDERFLOW                -> putStrLn "* Stack underflow"
        GL_STACK_OVERFLOW                 -> putStrLn "* Stack overflow"

        _ -> return ()

      glGetErrors




checkLinkStatus :: GLuint -> IO ()
checkLinkStatus prog = do

  linked <- overPtr (glGetProgramiv prog GL_LINK_STATUS)

  when (linked == GL_FALSE) $ do

    maxLength <- overPtr (glGetProgramiv prog GL_INFO_LOG_LENGTH)

    logLines <- allocaArray (fromIntegral maxLength) $ \p ->

            alloca $ \lenP -> do

              glGetProgramInfoLog prog maxLength lenP p
              len <- peek lenP
              peekCStringLen (p, fromIntegral len)

    putStrLn logLines


checkCompileStatus :: String -> GLuint -> IO ()
checkCompileStatus path shader = do

  compiled <- overPtr (glGetShaderiv shader GL_COMPILE_STATUS)

  when (compiled == GL_FALSE) $ do

    maxLength <- overPtr (glGetShaderiv shader GL_INFO_LOG_LENGTH)

    logLines <- allocaArray (fromIntegral maxLength) $ \p ->

            alloca $ \lenP -> do 

              glGetShaderInfoLog shader maxLength lenP p
              len <- peek lenP
              peekCStringLen (p, fromIntegral len)

    putStrLn ("In " ++ path ++ ":")
    
    putStrLn logLines