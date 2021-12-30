import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Monad.Fix
import qualified Data.ByteString as B


main :: IO ()
main = do
  let 
    scrWidth  = 800
    scrHeight = 600

  vertexShaderSource <- B.readFile "./shader.vs"
  fragmentShaderSource <- B.readFile "./shader.fs"
  
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

      let
        checkIfSuccess obj = case obj of
          Left shaderObj -> do
            success <- get $ GL.compileStatus shaderObj
            if success then
              return ()
            else do
              infoLog <- get $ GL.shaderInfoLog shaderObj
              putStrLn $ "ERROR::COMPILATION_FAILED\n" ++ infoLog

          Right shaderProgramObj -> do
            success <- get $ GL.linkStatus shaderProgramObj
            if success then
              return ()
            else do
              infoLog <- get $ GL.programInfoLog shaderProgramObj
              putStrLn $ "ERROR::LINKING_FAILED\n" ++ infoLog

        compileShaderSrc src t = do
          shaderObj <- GL.createShader t
          GL.shaderSourceBS shaderObj $= src
          GL.compileShader shaderObj
          return shaderObj

      vertexShader <- compileShaderSrc vertexShaderSource GL.VertexShader
      checkIfSuccess $ Left vertexShader
      
      fragmentShader <- compileShaderSrc fragmentShaderSource GL.FragmentShader
      checkIfSuccess $ Left fragmentShader
      
      shaderProgram <- GL.createProgram
      GL.attachShader shaderProgram vertexShader
      GL.attachShader shaderProgram fragmentShader
      GL.linkProgram shaderProgram
      checkIfSuccess $ Right shaderProgram
      
      deleteObjectName vertexShader
      deleteObjectName fragmentShader

      GL.currentProgram $= Just shaderProgram
      attribLocation_0 <- get $ GL.attribLocation shaderProgram "aPos"
      attribLocation_1 <- get $ GL.attribLocation shaderProgram "aColor"
      GL.currentProgram $= Nothing

      let
        vertices = 
          [ (0.5*) $ cos $ (90+120*0)*(pi/180), (0.5*) $ sin $ (90+120*0)*(pi/180), 0.0, 1.0, 0.0, 0.0
          , (0.5*) $ cos $ (90+120*1)*(pi/180), (0.5*) $ sin $ (90+120*1)*(pi/180), 0.0, 0.0, 1.0, 0.0
          , (0.5*) $ cos $ (90+120*2)*(pi/180), (0.5*) $ sin $ (90+120*2)*(pi/180), 0.0, 0.0, 0.0, 1.0] :: [GLfloat]

        v_layout = do
          d <- return [3,3]
          x <- (0:) . foldr (\a b -> zipWith (+) (repeat a) (0:b)) [] $ d
          return $ x * (fromIntegral . sizeOf $ vertices!!0)

        indices = [0, 1, 2] :: [GLuint]

        bufferListData t l u =
          withArray l $ \ptr2Data -> do
            let sizeOfData = fromIntegral $ (length l) * (sizeOf $ l!!0)
            GL.bufferData t $= (sizeOfData, ptr2Data, u)
      
      vao <- genObjectName
      GL.bindVertexArrayObject $= Just vao

      vbo <- genObjectName
      GL.bindBuffer GL.ArrayBuffer $= Just vbo
      bufferListData GL.ArrayBuffer vertices GL.StaticDraw

      ebo <- genObjectName
      GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
      bufferListData GL.ElementArrayBuffer indices GL.StaticDraw

      GL.vertexAttribPointer attribLocation_0
        $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (v_layout!!2) (plusPtr nullPtr . fromIntegral $ v_layout!!0))
      GL.vertexAttribArray attribLocation_0 $= GL.Enabled

      GL.vertexAttribPointer attribLocation_1
        $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (v_layout!!2) (plusPtr nullPtr . fromIntegral $ v_layout!!1))
      GL.vertexAttribArray attribLocation_1 $= GL.Enabled

      GL.bindBuffer GL.ArrayBuffer $= Nothing
      -- GL.bindBuffer GL.ElementArrayBuffer $= Nothing
      GL.bindVertexArrayObject $= Nothing

      -- GL.polygonMode $= (GL.Line, GL.Line)

      fix $ \rec -> do

        b <- GLFW.windowShouldClose window
        
        if b then
          return ()
          
        else do
          processInput window
          
          GL.clearColor $= GL.Color4 (0.5*0.5) (0.8*0.5) (0.9*0.5) 1
          GL.clear [GL.ColorBuffer]
          
          GL.currentProgram $= Just shaderProgram
          GL.bindVertexArrayObject $= Just vao
          GL.drawElements GL.Triangles (fromIntegral . length $ indices) GL.UnsignedInt nullPtr
          -- GL.bindVertexArrayObject $= Nothing

          GLFW.swapBuffers window
          GLFW.pollEvents
          
          rec

      deleteObjectName vao
      deleteObjectName vbo
      deleteObjectName ebo
      deleteObjectName shaderProgram

      GLFW.terminate

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
