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

      GL.depthFunc $= Just GL.Less
      GL.cullFace  $= Just GL.Back

      lightingShader  <- buildShaderProgram window ["./colors.vs", "./colors.fs"]
      skyboxShader    <- buildShaderProgram window ["./skybox.vs", "./skybox.fs"]

      GL.currentProgram $= Just lightingShader
      -- ## AttribLocations
      aLoc_A_aPos        <- get $ GL.attribLocation  lightingShader "aPos"
      aLoc_A_aNormal     <- get $ GL.attribLocation  lightingShader "aNormal"
      aLoc_A_aTexCoords  <- get $ GL.attribLocation  lightingShader "aTexCoords"
      -- ## uniformLocations
      uLoc_A_material_diffuse    <- get $ GL.uniformLocation lightingShader "material.diffuse"
      uLoc_A_material_specular   <- get $ GL.uniformLocation lightingShader "material.specular"
      uLoc_A_material_shininess  <- get $ GL.uniformLocation lightingShader "material.shininess"
      uLoc_A_light_ambient       <- get $ GL.uniformLocation lightingShader "light.ambient"
      uLoc_A_light_diffuse       <- get $ GL.uniformLocation lightingShader "light.diffuse"
      uLoc_A_light_specular      <- get $ GL.uniformLocation lightingShader "light.specular"
      uLoc_A_light_position      <- get $ GL.uniformLocation lightingShader "light.position"
      uLoc_A_viewPos             <- get $ GL.uniformLocation lightingShader "viewPos"
      uLoc_A_model               <- get $ GL.uniformLocation lightingShader "model"
      uLoc_A_view                <- get $ GL.uniformLocation lightingShader "view"
      uLoc_A_projection          <- get $ GL.uniformLocation lightingShader "projection"
      uLoc_A_skybox              <- get $ GL.uniformLocation lightingShader "skybox"

      GL.currentProgram $= Just skyboxShader
      -- ## AttribLocation
      aLoc_B_aPos        <- get $ GL.attribLocation  skyboxShader "aPos"
      -- ## uniformLocations
      -- uLoc_B_model       <- get $ GL.uniformLocation skyboxShader "model"
      uLoc_B_view        <- get $ GL.uniformLocation skyboxShader "view"
      uLoc_B_projection  <- get $ GL.uniformLocation skyboxShader "projection"
      uLoc_B_skybox      <- get $ GL.uniformLocation skyboxShader "skybox"

      GL.currentProgram $= Nothing

      let
        cubeVertices    = model_cube
        sizeOfElm       = sizeOf (undefined::GLfloat)

      cubeVBO <- genObjectName
      GL.bindBuffer GL.ArrayBuffer $= Just cubeVBO
      bufferListData GL.ArrayBuffer cubeVertices GL.StaticDraw

      cubeVAO <- genObjectName
      GL.bindVertexArrayObject $= Just cubeVAO
      initAttribVars sizeOfElm [(3, Just aLoc_A_aPos), (3, Just aLoc_A_aNormal), (2, Just aLoc_A_aTexCoords)]


      skyboxVBO <- genObjectName
      GL.bindBuffer GL.ArrayBuffer $= Just skyboxVBO
      bufferListData GL.ArrayBuffer skyboxVertices GL.StaticDraw

      skyboxVAO <- genObjectName
      GL.bindVertexArrayObject $= Just skyboxVAO
      initAttribVars sizeOfElm [(3, Just aLoc_B_aPos)]

      diffuseMap  <- loadTexture2D $ "./container2.jpg"
      specularMap <- loadTexture2D $ "./container2_specular.jpg"
      (Just cubemapTexture) <- loadCubemap (fmap (++".jpg") $ ["right","left","top","bottom","front","back"])

      GL.currentProgram $= Just lightingShader
      GL.uniform uLoc_A_material_diffuse  $= (0::GLint)
      GL.uniform uLoc_A_material_specular $= (1::GLint)
      GL.uniform uLoc_A_skybox            $= (2::GLint)

      GL.currentProgram $= Just skyboxShader
      GL.uniform uLoc_B_skybox            $= (0::GLint)

      GL.currentProgram $= Nothing
      GL.bindBuffer GL.ArrayBuffer $= Nothing
      GL.bindVertexArrayObject     $= Nothing
      
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
      
      cameraPos $= V3 0 0 7
      cameraDir $= calcDirectionVector 0 0
      cameraUp  $= V3 0 1 0
      lightPos  $= (91 *^ V3 (-0.41925156) 0.49880368 (-0.75856644))

      fix $ \rec -> do

        b <- GLFW.windowShouldClose window
        
        if b then
          return ()
          
        else do
          (_, (Size scrWidth scrHeight)) <- get $ GL.viewport

          GL.clearColor $= GL.Color4 (0.5*0.19) (0.8*0.19) (0.9*0.19) 1
          GL.clear [GL.ColorBuffer, GL.DepthBuffer]
          
          t_maybe <- getTime
          case t_maybe of
            Just t -> do
              currentCameraPos <- get cameraPos
              currentCameraDir <- get cameraDir
              currentCameraUp  <- get cameraUp
              currentLightPos  <- get lightPos

              processInput window myState (realToFrac t)

              view <- mat4ToGLmatrix $
                mat4_lookAt currentCameraPos
                  (currentCameraPos + currentCameraDir) currentCameraUp
              view2 <- mat4ToGLmatrix $
                removeTranslation $ mat4_lookAt currentCameraPos
                  (currentCameraPos + currentCameraDir) currentCameraUp
              projection <- mat4ToGLmatrix $
                mat4_perspective (radians 45)
                  ((fromIntegral scrWidth)/(fromIntegral scrHeight)) 0.1 100

              model1 <- mat4ToGLmatrix $
                (identity :: M44 GLfloat)
              model2 <- mat4ToGLmatrix $
                mat4_translate currentLightPos !*!
                mat4_rotate (V3 0.3 0.2 0.1) (realToFrac t * radians (-55)) !*!
                mat4_scale 0.2 0.2 0.2

              GL.currentProgram $= Just lightingShader

              GL.uniform uLoc_A_viewPos            $= v3ToVector3 (currentCameraPos)
              GL.uniform uLoc_A_light_position     $= v3ToVector3 (currentLightPos)
              GL.uniform uLoc_A_light_ambient      $= (Vector3 0.2 0.2 0.2 :: Vector3 GLfloat)
              GL.uniform uLoc_A_light_diffuse      $= (Vector3 0.5 0.5 0.5 :: Vector3 GLfloat)
              GL.uniform uLoc_A_light_specular     $= (Vector3 1 1 1 :: Vector3 GLfloat)
              GL.uniform uLoc_A_material_shininess $= (64 :: GLfloat)
              GL.uniform uLoc_A_model              $= model1
              GL.uniform uLoc_A_view               $= view
              GL.uniform uLoc_A_projection         $= projection


              GL.bindVertexArrayObject $= Just cubeVAO
              GL.activeTexture $= GL.TextureUnit 0
              GL.textureBinding GL.Texture2D $= Just diffuseMap
              GL.activeTexture $= GL.TextureUnit 1
              GL.textureBinding GL.Texture2D $= Just specularMap
              GL.activeTexture $= GL.TextureUnit 2
              GL.textureBinding GL.TextureCubeMap $= Just cubemapTexture
              GL.drawArrays GL.Triangles 0 36
              GL.bindVertexArrayObject $= Nothing

              GL.depthFunc $= Just GL.Lequal
              GL.currentProgram $= Just skyboxShader

              -- GL.uniform uLoc_B_model       $= model2
              GL.uniform uLoc_B_view        $= view2
              GL.uniform uLoc_B_projection  $= projection

              GL.bindVertexArrayObject $= Just skyboxVAO
              GL.activeTexture $= GL.TextureUnit 0
              GL.textureBinding GL.TextureCubeMap $= Just cubemapTexture
              GL.drawArrays GL.Triangles 0 36
              GL.bindVertexArrayObject $= Nothing

              GL.depthFunc $= Just GL.Less
              
            Nothing -> return ()

          GLFW.swapBuffers window
          GLFW.pollEvents
          
          rec

      freeMyStateVars myState

      deleteObjectName cubeVAO
      deleteObjectName skyboxVAO
      deleteObjectName cubeVBO
      deleteObjectName skyboxVBO
      deleteObjectName lightingShader
      deleteObjectName skyboxShader

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

v3ToVector3 :: V3 GLfloat -> Vector3 GLfloat
v3ToVector3 (V3 x y z) = Vector3 x y z

mat4ToGLmatrix :: M44 GLfloat -> IO (GLmatrix GLfloat)
mat4ToGLmatrix d = (GL.newMatrix GL.ColumnMajor $ mat4ToList $ d)

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


removeTranslation ::  (Num a) => M44 a -> M44 a
removeTranslation (V4 (V4 a11 a12 a13 _) (V4 a21 a22 a23 _) (V4 a31 a32 a33 _) _) =
  (V4 (V4 a11 a12 a13 0) (V4 a21 a22 a23 0) (V4 a31 a32 a33 0) (V4 0 0 0 1))


skyboxVertices :: [GLfloat]
skyboxVertices =
  [(-1.0), 1.0, (-1.0)
  ,(-1.0), (-1.0), (-1.0)
  ,1.0, (-1.0), (-1.0)
  ,1.0, (-1.0), (-1.0)
  ,1.0, 1.0, (-1.0)
  ,(-1.0), 1.0, (-1.0)

  ,(-1.0), (-1.0), 1.0
  ,(-1.0), (-1.0), (-1.0)
  ,(-1.0), 1.0, (-1.0)
  ,(-1.0), 1.0, (-1.0)
  ,(-1.0), 1.0, 1.0
  ,(-1.0), (-1.0), 1.0

  ,1.0, (-1.0), (-1.0)
  ,1.0, (-1.0), 1.0
  ,1.0, 1.0, 1.0
  ,1.0, 1.0, 1.0
  ,1.0, 1.0, (-1.0)
  ,1.0, (-1.0), (-1.0)

  ,(-1.0), (-1.0), 1.0
  ,(-1.0), 1.0, 1.0
  ,1.0, 1.0, 1.0
  ,1.0, 1.0, 1.0
  ,1.0, (-1.0), 1.0
  ,(-1.0), (-1.0), 1.0

  ,(-1.0), 1.0, (-1.0)
  ,1.0, 1.0, (-1.0)
  ,1.0, 1.0, 1.0
  ,1.0, 1.0, 1.0
  ,(-1.0), 1.0, 1.0
  ,(-1.0), 1.0, (-1.0)

  ,(-1.0), (-1.0), (-1.0)
  ,(-1.0), (-1.0), 1.0
  ,1.0, (-1.0), (-1.0)
  ,1.0, (-1.0), (-1.0)
  ,(-1.0), (-1.0), 1.0
  ,1.0, (-1.0), 1.0]

model_cube :: [GLfloat]
model_cube =
  [(-0.5), (-0.5), (-0.5), 0.0, 0.0, (-1.0), 0.0, 0.0
  ,(-0.5), 0.5, (-0.5), 0.0, 0.0, (-1.0), 0.0, 1.0
  ,0.5, 0.5, (-0.5), 0.0, 0.0, (-1.0), 1.0, 1.0
  ,0.5, 0.5, (-0.5), 0.0, 0.0, (-1.0), 1.0, 1.0
  ,0.5, (-0.5), (-0.5), 0.0, 0.0, (-1.0), 1.0, 0.0
  ,(-0.5), (-0.5), (-0.5), 0.0, 0.0, (-1.0), 0.0, 0.0

  ,(-0.5), (-0.5), 0.5, 0.0, 0.0, 1.0, 0.0, 0.0
  ,0.5, (-0.5), 0.5, 0.0, 0.0, 1.0, 1.0, 0.0
  ,0.5, 0.5, 0.5, 0.0, 0.0, 1.0, 1.0, 1.0
  ,0.5, 0.5, 0.5, 0.0, 0.0, 1.0, 1.0, 1.0
  ,(-0.5), 0.5, 0.5, 0.0, 0.0, 1.0, 0.0, 1.0
  ,(-0.5), (-0.5), 0.5, 0.0, 0.0, 1.0, 0.0, 0.0

  ,(-0.5), 0.5, 0.5, (-1.0), 0.0, 0.0, 1.0, 0.0
  ,(-0.5), 0.5, (-0.5), (-1.0), 0.0, 0.0, 1.0, 1.0
  ,(-0.5), (-0.5), (-0.5), (-1.0), 0.0, 0.0, 0.0, 1.0
  ,(-0.5), (-0.5), (-0.5), (-1.0), 0.0, 0.0, 0.0, 1.0
  ,(-0.5), (-0.5), 0.5, (-1.0), 0.0, 0.0, 0.0, 0.0
  ,(-0.5), 0.5, 0.5, (-1.0), 0.0, 0.0, 1.0, 0.0

  ,0.5, 0.5, 0.5, 1.0, 0.0, 0.0, 1.0, 0.0
  ,0.5, (-0.5), 0.5, 1.0, 0.0, 0.0, 0.0, 0.0
  ,0.5, (-0.5), (-0.5), 1.0, 0.0, 0.0, 0.0, 1.0
  ,0.5, (-0.5), (-0.5), 1.0, 0.0, 0.0, 0.0, 1.0
  ,0.5, 0.5, (-0.5), 1.0, 0.0, 0.0, 1.0, 1.0
  ,0.5, 0.5, 0.5, 1.0, 0.0, 0.0, 1.0, 0.0

  ,(-0.5), (-0.5), (-0.5), 0.0, (-1.0), 0.0, 0.0, 1.0
  ,0.5, (-0.5), (-0.5), 0.0, (-1.0), 0.0, 1.0, 1.0
  ,0.5, (-0.5), 0.5, 0.0, (-1.0), 0.0, 1.0, 0.0
  ,0.5, (-0.5), 0.5, 0.0, (-1.0), 0.0, 1.0, 0.0
  ,(-0.5), (-0.5), 0.5, 0.0, (-1.0), 0.0, 0.0, 0.0
  ,(-0.5), (-0.5), (-0.5), 0.0, (-1.0), 0.0, 0.0, 1.0

  ,(-0.5), 0.5, (-0.5), 0.0, 1.0, 0.0, 0.0, 1.0
  ,(-0.5), 0.5, 0.5, 0.0, 1.0, 0.0, 0.0, 0.0
  ,0.5, 0.5, 0.5, 0.0, 1.0, 0.0, 1.0, 0.0
  ,0.5, 0.5, 0.5, 0.0, 1.0, 0.0, 1.0, 0.0
  ,0.5, 0.5, (-0.5), 0.0, 1.0, 0.0, 1.0, 1.0
  ,(-0.5), 0.5, (-0.5), 0.0, 1.0, 0.0, 0.0, 1.0]


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

withImg :: Bool -> String -> (Ptr GLubyte -> GLsizei -> GLsizei -> IO ()) -> IO Bool 
withImg upsidedown imgSrc action = do
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
      
            if upsidedown then
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

            else
              ($(0,0)) . fix $ \rec (i,j) ->
                if i < height then do
                  let
                    (PixelRGB8 r g b) = pixelAt imgRGB j i
                    rbg_GLubyte = (fmap fromIntegral $ [r,g,b])::[GLubyte]
        
                  pokeElemOff ptr (3*(width*i+j) + 0) (rbg_GLubyte!!0)
                  pokeElemOff ptr (3*(width*i+j) + 1) (rbg_GLubyte!!1)
                  pokeElemOff ptr (3*(width*i+j) + 2) (rbg_GLubyte!!2)
        
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


bufferListData t l u =
  withArray l $ \ptr2Data -> do
    let sizeOfData = fromIntegral $ (length l) * (sizeOf $ l !! 0)
    GL.bufferData t $= (sizeOfData, ptr2Data, u)  


loadTexture2D :: String -> IO TextureObject
loadTexture2D src = do
  texture <- genObjectName
  GL.textureBinding GL.Texture2D $= Just texture

  GL.textureFilter GL.Texture2D
    $= ((GL.Linear', Just GL.Linear'), GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.S
    $= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T
    $= (GL.Repeated, GL.Repeat)

  withImg True src $ \ptr imgWidth imgHeight -> do
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB'
      (TextureSize2D imgWidth imgHeight) 0
        (GL.PixelData GL.RGB GL.UnsignedByte ptr)
    GL.generateMipmap' GL.Texture2D

  return texture


initAttribVars :: Int -> [(GLint, Maybe AttribLocation)] -> IO ()
initAttribVars sizeOfElm v_layout = foldr (>>) (return ()) $ do
  i <- enumFromTo 0 (pred . length $ v_layout)
  return $ do
    let
      size       = v_layout1 !! i
      attribLoc' = v_layout2 !! i
      offset     = v_layout3 !! i
      stride     = last v_layout3
    case attribLoc' of
      Just attribLoc -> do
        GL.vertexAttribPointer attribLoc
          $= (GL.ToFloat, GL.VertexArrayDescriptor size GL.Float stride (plusPtr nullPtr . fromIntegral $ offset))
        GL.vertexAttribArray attribLoc $= GL.Enabled
      Nothing -> return ()

  where
    v_layout1 = fmap fst $ v_layout
    v_layout2 = fmap snd $ v_layout
    v_layout3 = do
      d <- return v_layout1
      x <- (0:) . foldr (\a b -> zipWith (+) (repeat a) (0:b)) [] $ d
      return $ x * (fromIntegral sizeOfElm)
  
loadCubemap :: [String] -> IO (Maybe TextureObject)
loadCubemap faces = do
  let
    labels =
      [ GL.TextureCubeMapPositiveX
      , GL.TextureCubeMapNegativeX
      , GL.TextureCubeMapPositiveY
      , GL.TextureCubeMapNegativeY
      , GL.TextureCubeMapPositiveZ
      , GL.TextureCubeMapNegativeZ ]

  if length faces == 6 then do
    texture <- genObjectName
    GL.textureBinding GL.TextureCubeMap $= Just texture
  
    ($0) $ fix $ \rec i -> do
      if i < 6 then do
        success <- withImg False (faces !! i) $ \ptr imgWidth imgHeight -> do
          GL.texImage2D (labels !! i) GL.NoProxy 0 GL.RGB'
            (TextureSize2D imgWidth imgHeight) 0
              (GL.PixelData GL.RGB GL.UnsignedByte ptr)
        if success then do
          -- print $ (faces !! i, labels !! i)
          rec $ succ i
        else do
          -- putStrLn "Failed to load images."
          return Nothing
      else do
        GL.textureFilter GL.TextureCubeMap
          $= ((GL.Linear', Nothing), GL.Linear')
        GL.textureWrapMode GL.TextureCubeMap GL.S
          $= (GL.Repeated, GL.ClampToEdge)
        GL.textureWrapMode GL.TextureCubeMap GL.T
          $= (GL.Repeated, GL.ClampToEdge)
        GL.textureWrapMode GL.TextureCubeMap GL.R
          $= (GL.Repeated, GL.ClampToEdge)
        return $ Just texture

  else do
    -- putStrLn "The number of pictures is inadequate."
    return Nothing
