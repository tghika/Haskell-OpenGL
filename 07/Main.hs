import Codec.Picture
import Codec.Picture.Metadata as JP
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Linear.Epsilon
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
import Data.StateVar
import qualified Data.ByteString as B


main :: IO ()
main = do
  
  GLFW.init
  
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 0
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Any
   
  winMaybe <- GLFW.createWindow 640 480 "LearnOpenGL" Nothing Nothing

  case winMaybe of
    Nothing -> do
      putStrLn "Failed to create GLFW window"
      GLFW.terminate
    
    Just window -> do
      GLFW.makeContextCurrent (Just window)
      GLFW.setFramebufferSizeCallback window (Just framebufferSizeCallback)

      shaderProgram <- buildShaderProgram window ["./shader.vs", "./shader.fs"]

      GL.currentProgram $= Just shaderProgram

      attribLocation_0 <- get $ GL.attribLocation shaderProgram "aPos"
      -- attribLocation_1 <- get $ GL.attribLocation shaderProgram "aColor"
      attribLocation_2 <- get $ GL.attribLocation shaderProgram "aTexCoord"

      uniformLocation_0 <- get $ GL.uniformLocation shaderProgram "texture1"
      uniformLocation_1 <- get $ GL.uniformLocation shaderProgram "transform"

      GL.currentProgram $= Nothing

      let
        vertices = model_cube

        v_layout = do
          d <- return [3,2]
          x <- (0:) . foldr (\a b -> zipWith (+) (repeat a) (0:b)) [] $ d
          return $ x * (fromIntegral . sizeOf $ vertices!!0)

        bufferListData t l u =
          withArray l $ \ptr2Data -> do
            let sizeOfData = fromIntegral $ (length l) * (sizeOf $ l!!0)
            GL.bufferData t $= (sizeOfData, ptr2Data, u)
      
      vao <- genObjectName
      GL.bindVertexArrayObject $= Just vao

      vbo <- genObjectName
      GL.bindBuffer GL.ArrayBuffer $= Just vbo
      bufferListData GL.ArrayBuffer vertices GL.StaticDraw

      GL.vertexAttribPointer attribLocation_0
        $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (v_layout!!2) (plusPtr nullPtr . fromIntegral $ v_layout!!0))
      GL.vertexAttribArray attribLocation_0 $= GL.Enabled

      GL.vertexAttribPointer attribLocation_2
        $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (v_layout!!2) (plusPtr nullPtr . fromIntegral $ v_layout!!1))
      GL.vertexAttribArray attribLocation_2 $= GL.Enabled

      texture <- genObjectName
      GL.textureBinding GL.Texture2D $= Just texture

      GL.textureFilter GL.Texture2D
        $= ((GL.Linear', Just GL.Linear'), GL.Linear')
      GL.textureWrapMode GL.Texture2D GL.S
        $= (GL.Repeated, GL.Repeat)
      GL.textureWrapMode GL.Texture2D GL.T
        $= (GL.Repeated, GL.Repeat)

      withImg "./img1.jpg" $ \ptr imgWidth imgHeight -> do
        GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB'
          (TextureSize2D imgWidth imgHeight) 0
            (GL.PixelData GL.RGB GL.UnsignedByte ptr)
        GL.generateMipmap' GL.Texture2D
        GL.uniform uniformLocation_0 $= (0::GLint)

      GL.bindBuffer GL.ArrayBuffer $= Nothing
      GL.bindVertexArrayObject $= Nothing

      GL.depthFunc $= Just GL.Less
      
      myState <- initMyStateVars

      let
        deltaTime = getMyStateVar_f myState DeltaTime
        lastFrame = getMyStateVar_f myState LastFrame
        yaw       = getMyStateVar_f myState Yaw
        pitch     = getMyStateVar_f myState Pitch
        cameraPos = getMyStateVar_3f myState CameraPos
        cameraDir = getMyStateVar_3f myState CameraDir
        cameraUp  = getMyStateVar_3f myState CameraUp
        lightPos  = getMyStateVar_3f myState LightPos

      deltaTime $= 0
      lastFrame $= 0
      yaw $= 0
      pitch $= 0
      
      cameraPos $= V3 0 0 3
      cameraDir $= calcDirectionVector 0 0
      cameraUp  $= V3 0 1 0
      lightPos  $= V3 1.2 1 2

      fix $ \rec -> do

        b <- GLFW.windowShouldClose window
        
        if b then
          return ()
          
        else do
          (_, (Size scrWidth scrHeight)) <- get $ GL.viewport

          t_maybe <- getTime
          case t_maybe of
            Just t -> do
              currentCameraPos <- get cameraPos
              currentCameraDir <- get cameraDir
              currentCameraUp  <- get cameraUp


              processInput window myState (realToFrac t)


              let
                model = 
                  mat4_rotate (V3 0.3 0.2 0.1) (realToFrac t * radians (-55))
                view = 
                  mat4_lookAt currentCameraPos
                    (currentCameraPos + currentCameraDir) currentCameraUp
                projection =
                  mat4_perspective (radians 45) 
                    ((fromIntegral scrWidth)/(fromIntegral scrHeight)) 0.1 100
                    
              trans <- (GL.newMatrix GL.ColumnMajor $
                mat4ToList $ (projection !*! view !*! model)) :: IO (GLmatrix GLfloat)
              GL.uniform uniformLocation_1 $= trans
              
            Nothing -> do
              trans <- (GL.newMatrix GL.ColumnMajor $
                mat4ToList $ (identity :: M44 GLfloat)) :: IO (GLmatrix GLfloat)
              GL.uniform uniformLocation_1 $= trans

          GL.clearColor $= GL.Color4 (0.5*0.5) (0.8*0.5) (0.9*0.5) 1
          GL.clear [GL.ColorBuffer, GL.DepthBuffer]
          
          GL.currentProgram $= Just shaderProgram
          GL.activeTexture $= GL.TextureUnit 0
          GL.textureBinding GL.Texture2D $= Just texture
          GL.bindVertexArrayObject $= Just vao

          GL.drawArrays GL.Triangles 0 36

          GLFW.swapBuffers window
          GLFW.pollEvents
          
          rec

      freeMyStateVars myState

      deleteObjectName vao
      deleteObjectName vbo
      deleteObjectName shaderProgram

      GLFW.terminate


data MyState =
  MyState (Ptr GLfloat) (Ptr (V3 GLfloat))
    (StateVar GLfloat) (StateVar GLfloat)
      (StateVar GLfloat) (StateVar GLfloat)
        (StateVar (V3 GLfloat)) (StateVar (V3 GLfloat)) (StateVar (V3 GLfloat))
          (StateVar (V3 GLfloat))


data MyStateName =
  DeltaTime | LastFrame | Yaw | Pitch | CameraPos | CameraDir | CameraUp | LightPos

initMyStateVars :: IO MyState
initMyStateVars = do
  p1 <- mallocArray 4 :: IO (Ptr GLfloat)
  p2 <- mallocArray 4 :: IO (Ptr (V3 GLfloat))
  let
    deltaTime =
      makeStateVarFromPtr $ p1 `plusPtr` (0*sizeOf(undefined::GLfloat))
    lastFrame =
      makeStateVarFromPtr $ p1 `plusPtr` (1*sizeOf(undefined::GLfloat))
    yaw =
      makeStateVarFromPtr $ p1 `plusPtr` (2*sizeOf(undefined::GLfloat))
    pitch =
      makeStateVarFromPtr $ p1 `plusPtr` (3*sizeOf(undefined::GLfloat))
    cameraPos = 
      makeStateVarFromPtr $ p2 `plusPtr` (0*sizeOf(undefined::(V3 GLfloat)))
    cameraDir = 
      makeStateVarFromPtr $ p2 `plusPtr` (1*sizeOf(undefined::(V3 GLfloat)))
    cameraUp  = 
      makeStateVarFromPtr $ p2 `plusPtr` (2*sizeOf(undefined::(V3 GLfloat)))
    lightPos  = 
      makeStateVarFromPtr $ p2 `plusPtr` (3*sizeOf(undefined::(V3 GLfloat)))

      
  return (MyState p1 p2 deltaTime lastFrame yaw pitch cameraPos cameraDir cameraUp lightPos)


getMyStateVar_f :: MyState -> MyStateName -> StateVar GLfloat
getMyStateVar_f (MyState _ _ v1 v2 v3 v4 v5 v6 v7 v8) varName = do
  case varName of
    DeltaTime -> v1
    LastFrame -> v2
    Yaw       -> v3
    Pitch     -> v4
    _         -> makeStateVarFromPtr nullPtr

getMyStateVar_3f :: MyState -> MyStateName -> StateVar (V3 GLfloat)
getMyStateVar_3f (MyState _ _ v1 v2 v3 v4 v5 v6 v7 v8) varName = do
  case varName of
    CameraPos -> v5
    CameraDir -> v6
    CameraUp  -> v7
    LightPos  -> v8
    _         -> makeStateVarFromPtr nullPtr

freeMyStateVars :: MyState -> IO ()
freeMyStateVars (MyState p1 p2 _ _ _ _ _ _ _ _) = do
  free p1
  free p2


processInput :: Window -> MyState -> GLfloat -> IO ()
processInput window myState t = do
  let
    sensitivity = 7
    deltaTime   = getMyStateVar_f  myState DeltaTime
    lastFrame   = getMyStateVar_f  myState LastFrame
    yaw         = getMyStateVar_f  myState Yaw
    pitch       = getMyStateVar_f  myState Pitch
    cameraPos   = getMyStateVar_3f myState CameraPos
    cameraDir   = getMyStateVar_3f myState CameraDir
    cameraUp    = getMyStateVar_3f myState CameraUp
    lightPos    = getMyStateVar_3f myState LightPos
    
  lastFrame_val <- get lastFrame

  let
    deltaTime = t
    cameraSpeed = 2.5 * (t - lastFrame_val)

  lastFrame $= t

  currentCameraDir <- get cameraDir
  currentCameraUp  <- get cameraUp
  GLFW.getKey window GLFW.Key'Escape >>= \s -> 
    if (s == GLFW.KeyState'Pressed) then
      GLFW.setWindowShouldClose window True
    else return ()
  GLFW.getKey window GLFW.Key'Right >>= \s -> 
    if (s == GLFW.KeyState'Pressed) then
      cameraPos $~ (+ (cameraSpeed *^ Linear.Metric.normalize(currentCameraDir `cross` currentCameraUp)))
    else return ()
  GLFW.getKey window GLFW.Key'Left >>= \s -> 
    if (s == GLFW.KeyState'Pressed) then
      cameraPos $~ (+ ((-1)*cameraSpeed *^ Linear.Metric.normalize(currentCameraDir `cross` currentCameraUp)))
    else return ()
  GLFW.getKey window GLFW.Key'Down >>= \s -> 
    if (s == GLFW.KeyState'Pressed) then
      cameraPos $~ (+ ((-1)*cameraSpeed *^ currentCameraDir))
    else return ()
  GLFW.getKey window GLFW.Key'Up >>= \s -> 
    if (s == GLFW.KeyState'Pressed) then
      cameraPos $~ (+ (cameraSpeed *^ currentCameraDir))
    else return ()


  yaw_val   <- get yaw
  pitch_val <- get pitch

  GLFW.getKey window GLFW.Key'W >>= \s -> 
    if (s == GLFW.KeyState'Pressed) then
      pitch $~ (+ (cameraSpeed * sensitivity))
    else return ()
  GLFW.getKey window GLFW.Key'S >>= \s -> 
    if (s == GLFW.KeyState'Pressed) then
      pitch $~ (+ ((-1)*cameraSpeed * sensitivity))
    else return ()
  GLFW.getKey window GLFW.Key'A >>= \s -> 
    if (s == GLFW.KeyState'Pressed) then
      yaw $~ (+ ((-1)*cameraSpeed * sensitivity))
    else return ()
  GLFW.getKey window GLFW.Key'D >>= \s -> 
    if (s == GLFW.KeyState'Pressed) then
      yaw $~ (+ (cameraSpeed * sensitivity))
    else return ()

  yaw_val   <- get yaw
  pitch_val <- get pitch

  if abs yaw_val > 360 then
    yaw $~ (\x -> (signum x) * (abs x - (fromIntegral.floor) (abs x/360) * 360))
  else
    return ()

  if abs pitch_val > 89 then
    pitch $= (signum pitch_val) * 89
  else
    return ()

  yaw_val'   <- get yaw
  pitch_val' <- get pitch

  cameraDir $= calcDirectionVector yaw_val' pitch_val'


framebufferSizeCallback :: GLFW.FramebufferSizeCallback
framebufferSizeCallback window width height = 
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

mat4ToList :: M44 a -> [a]
mat4ToList (V4 (V4 a11 a12 a13 a14) (V4 a21 a22 a23 a24) (V4 a31 a32 a33 a34) (V4 a41 a42 a43 a44)) =
  [a11,a21,a31,a41,a12,a22,a32,a42,a13,a23,a33,a43,a14,a24,a34,a44]

radians :: (Floating a) => a -> a
radians x = x * pi/180

mat4_translate :: (Num a) => V3 a -> M44 a
mat4_translate (V3 r1 r2 r3) =
  (V4 (V4 1 0 0 r1) (V4 0 1 0 r2) (V4 0 0 1 r3) (V4 0 0 0 1))

mat4_scale :: (Num a) => a -> a -> a -> M44 a
mat4_scale x y z =
  (V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 1))

mat4_rotate :: (Epsilon a, Floating a) => V3 a -> a -> M44 a
mat4_rotate axis angle =
  (mkTransformation (axisAngle axis angle) $ V3 0 0 0)

mat4_perspective :: (Floating a) => a -> a -> a -> a -> M44 a
mat4_perspective fovX aspect near far =
  (V4 (V4 (near/npWith_2) 0 0 0) (V4 0 (near/npHeight_2) 0 0)
    (V4 0 0 (-(far+near)/d) (-2*far*near/d)) (V4 0 0 (-1) 0))
  where
    d = far - near
    npWith_2 = near * tan (fovX/2)
    npHeight_2 = npWith_2 / aspect

mat4_lookAt :: (Epsilon a, Floating a) => V3 a -> V3 a -> V3 a -> M44 a
mat4_lookAt pos target up =
  pTrans !*! mat4_translate (-pos)
  where
    cDir   = Linear.Metric.normalize $ target - pos
    cRight = Linear.Metric.normalize $ cDir `cross` up
    cUp    = cRight `cross` cDir
    pTrans = mkTransformationMat (V3 cRight cUp (-cDir)) (V3 0 0 0)

model_cube :: [GLfloat]
model_cube =
  [(-0.5), (-0.5), (-0.5), 0, 0
  ,0.5, (-0.5), (-0.5), 1, 0
  ,0.5, 0.5, (-0.5), 1, 1
  ,0.5, 0.5, (-0.5), 1, 1
  ,(-0.5), 0.5, (-0.5), 0, 1
  ,(-0.5), (-0.5), (-0.5), 0, 0

  ,(-0.5), (-0.5), 0.5, 0, 0
  ,0.5, (-0.5), 0.5, 1, 0
  ,0.5, 0.5, 0.5, 1, 1
  ,0.5, 0.5, 0.5, 1, 1
  ,(-0.5), 0.5, 0.5, 0, 1
  ,(-0.5), (-0.5), 0.5, 0, 0

  ,(-0.5), 0.5, 0.5, 1, 0
  ,(-0.5), 0.5, (-0.5), 1, 1
  ,(-0.5), (-0.5), (-0.5), 0, 1
  ,(-0.5), (-0.5), (-0.5), 0, 1
  ,(-0.5), (-0.5), 0.5, 0, 0
  ,(-0.5), 0.5, 0.5, 1, 0

  ,0.5, 0.5, 0.5, 1, 0
  ,0.5, 0.5, (-0.5), 1, 1
  ,0.5, (-0.5), (-0.5), 0, 1
  ,0.5, (-0.5), (-0.5), 0, 1
  ,0.5, (-0.5), 0.5, 0, 0
  ,0.5, 0.5, 0.5, 1, 0

  ,(-0.5), (-0.5), (-0.5), 0, 1
  ,0.5, (-0.5), (-0.5), 1, 1
  ,0.5, (-0.5), 0.5, 1, 0
  ,0.5, (-0.5), 0.5, 1, 0
  ,(-0.5), (-0.5), 0.5, 0, 0
  ,(-0.5), (-0.5), (-0.5), 0, 1

  ,(-0.5), 0.5, (-0.5), 0, 1
  ,0.5, 0.5, (-0.5), 1, 1
  ,0.5, 0.5, 0.5, 1, 0
  ,0.5, 0.5, 0.5, 1, 0
  ,(-0.5), 0.5, 0.5, 0, 0
  ,(-0.5), 0.5, (-0.5), 0, 1]

makeStateVarFromPtr :: Storable a => Ptr a -> StateVar a
makeStateVarFromPtr p = makeStateVar (peek p) (poke p)

calcDirectionVector :: GLfloat -> GLfloat -> V3 GLfloat
calcDirectionVector yaw_deg pitch_deg = 
  Linear.Metric.normalize $ V3 x y z
  where
    yaw   = radians (yaw_deg - 90)
    pitch = radians pitch_deg
    x = cos yaw * cos pitch
    y = sin pitch
    z = sin yaw * cos pitch

withImg :: String -> (Ptr GLubyte -> GLsizei -> GLsizei -> IO ()) -> IO Bool 
withImg imgSrc action = do
    img' <- readImageWithMetadata imgSrc

    case img' of
      Left s -> (putStrLn $ "Error: " ++ s) >> return False
      
      Right (img, metaData) -> do
        t <- return $ do
          w <- (JP.lookup Width metaData)
          h <- (JP.lookup Height metaData)
          return (w,h)
      
        case t of
          Nothing -> (putStrLn "Failed to read meta-data") >> return False
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
  
            action ptr (fromIntegral imgWidth) (fromIntegral imgHeight)
            free ptr
            return True



buildShaderProgram :: Window -> [String] -> IO Program
buildShaderProgram window shaders = do
  
  vertexShaderSource <- B.readFile $ selectShaderSrc Shader_Vert shaders
  fragmentShaderSource <- B.readFile $ selectShaderSrc Shader_Frag shaders

  GLFW.makeContextCurrent (Just window)

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
  
  return shaderProgram


data ShaderType = Shader_Vert | Shader_Frag | Shader_Unknown deriving Eq


selectShaderSrc t xs = do
  let xs' = zip (fmap getShaderType $ xs) xs

  (shaderType, fileName) <- xs'
  if shaderType == t then
    fileName
  else
    []


getShaderType fileName =
  if ext == ".vs" then
    Shader_Vert
  else if ext == ".fs" then
    Shader_Frag
  else
    Shader_Unknown

  where ext = '.' : (reverse $ takeWhile (/='.') $ reverse fileName)


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


