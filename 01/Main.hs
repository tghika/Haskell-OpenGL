import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad.Fix


main :: IO ()
main = do
  let 
    scrWidth = 800
    scrHeight = 600
  
  GLFW.init
  
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 0
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Any
   
  winMaybe <- GLFW.createWindow scrWidth scrHeight "LearnOpenGL" Nothing Nothing

  case winMaybe of
    Nothing -> do
      putStrLn "Failed to create GLFW window"
      GLFW.terminate
    
    Just window -> do
      GLFW.makeContextCurrent (Just window)
      GLFW.setFramebufferSizeCallback window (Just framebufferSizeCallback)

      fix $ \action -> do

        b <- GLFW.windowShouldClose window
        
        if b then
          GLFW.terminate
          
        else do
          processInput window
          
          GL.clearColor $= Color4 0.5 0.8 0.9 1
          GL.clear [GL.ColorBuffer]
          
          GLFW.swapBuffers window
          GLFW.pollEvents
          
          action


processInput :: Window -> IO ()
processInput window = do
  keySt <- GLFW.getKey window GLFW.Key'Escape
  if (keySt == GLFW.KeyState'Pressed) then
    GLFW.setWindowShouldClose window True
  else
    return ()

framebufferSizeCallback :: GLFW.FramebufferSizeCallback
framebufferSizeCallback window width height = 
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
