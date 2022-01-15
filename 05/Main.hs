import Codec.Picture
import Codec.Picture.Metadata as JP
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Codec.Picture
import Linear.Vector
import Linear.Matrix
import Linear.Metric
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Lens
import Control.Monad.Fix
import qualified Data.ByteString as B


main :: IO ()
main = do
  let 
    scrWidth = 800
    scrHeight = 800

  vertexShaderSource <- B.readFile "./shader.vs"
  fragmentShaderSource<- B.readFile "./shader.fs"
  
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
      attribLocation_2 <- get $ GL.attribLocation shaderProgram "aTexCoord"

      uniformLocation_0 <- get $ GL.uniformLocation shaderProgram "texture1"
      uniformLocation_1 <- get $ GL.uniformLocation shaderProgram "transform"

      GL.currentProgram $= Nothing

      let
        vertices = 
          [   0.5,    0.5,  0,   1, 0, 0,   1, 1
          ,   0.5,  (-0.5), 0,   0, 1, 0,   1, 0
          , (-0.5), (-0.5), 0,   0, 0, 1,   0, 0
          , (-0.5),   0.5,  0,   1, 0, 0,   0, 1] :: [GLfloat]

        v_layout = do
          d <- return [3,3,2]
          x <- (0:) . foldr (\a b -> zipWith (+) (repeat a) (0:b)) [] $ d
          return $ x * (fromIntegral . sizeOf $ vertices!!0)

        indices =
          [0, 1, 3
          ,1, 2, 3] :: [GLuint]

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
        $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (v_layout!!3) (plusPtr nullPtr . fromIntegral $ v_layout!!0))
      GL.vertexAttribArray attribLocation_0 $= GL.Enabled

      GL.vertexAttribPointer attribLocation_1
        $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (v_layout!!3) (plusPtr nullPtr . fromIntegral $ v_layout!!1))
      GL.vertexAttribArray attribLocation_1 $= GL.Enabled

      GL.vertexAttribPointer attribLocation_2
        $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (v_layout!!3) (plusPtr nullPtr . fromIntegral $ v_layout!!2))
      GL.vertexAttribArray attribLocation_2 $= GL.Enabled

      texture <- genObjectName
      GL.textureBinding GL.Texture2D $= Just texture

      GL.textureFilter GL.Texture2D
        $= ((GL.Linear', Just GL.Linear'), GL.Linear')
      GL.textureWrapMode GL.Texture2D GL.S
        $= (GL.Repeated, GL.Repeat)
      GL.textureWrapMode GL.Texture2D GL.T
        $= (GL.Repeated, GL.Repeat)

      img' <- readImageWithMetadata "./img1.jpg"

      case img' of
        Left s -> putStrLn $ "Error: " ++ s
        
        Right (img, metaData) -> do
          t <- return $ do
            w <- (JP.lookup Width metaData)
            h <- (JP.lookup Height metaData)
            return (w,h)
    
          case t of
            Nothing -> putStrLn "Failed to read meta-data"
            Just (imgWidth, imgHeight) -> do
              (width, height) <- return (fromIntegral imgWidth, fromIntegral imgHeight)
              imgRGB <- return $ convertRGB8 img
    
              ptr <- mallocArray (width*height*3)
    
              ($(0,0)) . fix $ \rec (i,j) ->
                if i < height then do
                  let
                    (PixelRGB8 r g b) = pixelAt imgRGB j i
                    rbg_GLubyte = (fmap fromIntegral $ [r,g,b])::[GLubyte]
    
                  pokeElemOff ptr (3*(width*(height-1-i)+j) + 0) (rbg_GLubyte!!0)
                  pokeElemOff ptr (3*(width*(height-1-i)+j) + 1) (rbg_GLubyte!!1)
                  pokeElemOff ptr (3*(width*(height-1-i)+j) + 2) (rbg_GLubyte!!2)
    
                  (return $ if (succ j) >= width then (succ i, 0) else (i, succ j))
                    >>= rec
    
                else
                  return ()

              GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB'
                (TextureSize2D (fromIntegral imgWidth) (fromIntegral imgHeight)) 0
                  (GL.PixelData GL.RGB GL.UnsignedByte ptr)
              GL.generateMipmap' GL.Texture2D
              GL.uniform uniformLocation_0 $= (0::GLint)

              free ptr

      GL.bindBuffer GL.ArrayBuffer $= Nothing
      GL.bindVertexArrayObject $= Nothing

      fix $ \rec -> do

        b <- GLFW.windowShouldClose window
        
        if b then
          return ()
          
        else do
          processInput window

          t_maybe <- getTime
          case t_maybe of
            Just t -> do
              trans <- (GL.newMatrix GL.ColumnMajor $
                mat4x4ToList $ ((0.5 *!! (identity :: M44 GLfloat)) !*!
                  (mkTransformation (axisAngle (V3 0.2 0.2 1) (realToFrac t)) $ V3 0 0 0) !*!
                    (mat4_translate $ V3 (-0.2) (-0.2) 0.1))
                    ) :: IO (GLmatrix GLfloat)
              GL.uniform uniformLocation_1 $= trans
            Nothing -> do
              trans <- (GL.newMatrix GL.ColumnMajor $
                mat4x4ToList $ (identity :: M44 GLfloat)) :: IO (GLmatrix GLfloat)
              GL.uniform uniformLocation_1 $= trans

          GL.clearColor $= GL.Color4 (0.5*0.5) (0.8*0.5) (0.9*0.5) 1
          GL.clear [GL.ColorBuffer]
          
          GL.currentProgram $= Just shaderProgram
          GL.activeTexture $= GL.TextureUnit 0
          GL.textureBinding GL.Texture2D $= Just texture
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

mat4x4ToList :: M44 a -> [a]
mat4x4ToList (V4 (V4 a11 a12 a13 a14) (V4 a21 a22 a23 a24) (V4 a31 a32 a33 a34) (V4 a41 a42 a43 a44)) =
  [a11,a21,a31,a41,a12,a22,a32,a42,a13,a23,a33,a43,a14,a24,a34,a44]

mat4_translate :: (Num a) => V3 a -> M44 a
mat4_translate (V3 r1 r2 r3) =
  (V4 (V4 1 0 0 r1) (V4 0 1 0 r2) (V4 0 0 1 r3) (V4 0 0 0 1))
