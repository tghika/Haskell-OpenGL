import System.Random
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
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
   
  winMaybe <- GLFW.createWindow 1024 768 "LearnOpenGL" Nothing Nothing

  case winMaybe of
    Nothing -> do
      putStrLn "Failed to create GLFW window"
      GLFW.terminate
    
    Just window -> do
      GLFW.makeContextCurrent (Just window)
      GLFW.swapInterval 1
      GLFW.setFramebufferSizeCallback window (Just framebufferSizeCallback)

      GL.depthFunc $= Just GL.Less
      GL.cullFace  $= Just GL.Back

      geopassShader     <- buildShaderProgram window ["./g_buffer.vs", "./g_buffer.fs"]
      lightingShader    <- buildShaderProgram window ["./quad.vs", "./lighting.fs"]
      ssaoShader        <- buildShaderProgram window ["./quad.vs", "./ssao.fs"]
      ssaoBlurShader    <- buildShaderProgram window ["./quad.vs", "./ssao_blur.fs"]
      skyboxShader      <- buildShaderProgram window ["./skybox.vs", "./skybox.fs"]
      simpleDepthShader <- buildShaderProgram window ["./depth.vs", "./depth.fs"]



      -- ## Attriblocations (fixed)
      aLoc_aPos                  <- return $ GL.AttribLocation 0
      aLoc_aNormal               <- return $ GL.AttribLocation 1
      aLoc_aTexCoords            <- return $ GL.AttribLocation 2

      GL.currentProgram $= Just lightingShader
      -- ## uniformLocations
      uLoc_A_light_ambient       <- get $ GL.uniformLocation lightingShader "light.ambient"
      uLoc_A_light_diffuse       <- get $ GL.uniformLocation lightingShader "light.diffuse"
      uLoc_A_light_specular      <- get $ GL.uniformLocation lightingShader "light.specular"
      uLoc_A_light_position      <- get $ GL.uniformLocation lightingShader "light.position"
      uLoc_A_gPosition           <- get $ GL.uniformLocation lightingShader "gPosition"
      uLoc_A_gNormal             <- get $ GL.uniformLocation lightingShader "gNormal"
      uLoc_A_gAlbedo             <- get $ GL.uniformLocation lightingShader "gAlbedo"
      uLoc_A_ssao                <- get $ GL.uniformLocation lightingShader "ssao"

      GL.currentProgram $= Just skyboxShader
      -- ## uniformLocations
      uLoc_B_view                <- get $ GL.uniformLocation skyboxShader "view"
      uLoc_B_projection          <- get $ GL.uniformLocation skyboxShader "projection"
      uLoc_B_skybox              <- get $ GL.uniformLocation skyboxShader "skybox"
      
      GL.currentProgram $= Just simpleDepthShader
      -- ## uniformLocations
      uLoc_C_lightSpaceMatrix    <- get $ GL.uniformLocation simpleDepthShader "lightSpaceMatrix"
      uLoc_C_model               <- get $ GL.uniformLocation simpleDepthShader "model"
      
      GL.currentProgram $= Just geopassShader
      -- ## uniformLocations
      uLoc_D_material_diffuse    <- get $ GL.uniformLocation geopassShader "material.diffuse"
      uLoc_D_material_specular   <- get $ GL.uniformLocation geopassShader "material.specular"
      uLoc_D_material_shininess  <- get $ GL.uniformLocation geopassShader "material.shininess"
      uLoc_D_model               <- get $ GL.uniformLocation geopassShader "model"
      uLoc_D_view                <- get $ GL.uniformLocation geopassShader "view"
      uLoc_D_projection          <- get $ GL.uniformLocation geopassShader "projection"
      uLoc_D_lightSpaceMatrix    <- get $ GL.uniformLocation geopassShader "lightSpaceMatrix"
      uLoc_D_shadowMap           <- get $ GL.uniformLocation geopassShader "shadowMap"
      
      GL.currentProgram $= Just ssaoShader
      -- ## uniformLocations
      uLoc_E_gPosition           <- get $ GL.uniformLocation ssaoShader "gPosition"
      uLoc_E_gNormal             <- get $ GL.uniformLocation ssaoShader "gNormal"
      uLoc_E_texNoise            <- get $ GL.uniformLocation ssaoShader "texNoise"
      uLoc_E_projection          <- get $ GL.uniformLocation ssaoShader "projection"
      uLoc_E_samples <- foldr (\a b -> do{x<-a;xs<-b;return$x:xs}) (return []) $ do
        i <- [0..(64-1)]
        return $ get $ GL.uniformLocation ssaoShader ("samples[" ++ show i ++ "]")
      
      GL.currentProgram $= Just ssaoBlurShader
      -- ## uniformLocations
      uLoc_F_ssaoInput           <- get $ GL.uniformLocation ssaoBlurShader "ssaoInput"

      GL.currentProgram $= Nothing

      let
        cubeVertices    = model_cube
        sizeOfElm       = sizeOf (undefined::GLfloat)
        bufferWidth     = 1024
        bufferHeight    = 768


      gBuffer <- genObjectName
      GL.bindFramebuffer GL.Framebuffer $= gBuffer

      gPosition <- genObjectName
      GL.textureBinding GL.Texture2D $= Just gPosition
      GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA16F
        (TextureSize2D bufferWidth bufferHeight) 0
          (GL.PixelData GL.RGBA GL.Float nullPtr)
      GL.textureFilter GL.Texture2D
        $= ((GL.Nearest, Nothing), GL.Nearest)
      GL.textureWrapMode GL.Texture2D GL.S
        $= (GL.Repeated, GL.ClampToEdge)
      GL.textureWrapMode GL.Texture2D GL.T
        $= (GL.Repeated, GL.ClampToEdge)
      GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0)
        GL.Texture2D gPosition 0

      gNormal <- genObjectName
      GL.textureBinding GL.Texture2D $= Just gNormal
      GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA16F
        (TextureSize2D bufferWidth bufferHeight) 0
          (GL.PixelData GL.RGBA GL.Float nullPtr)
      GL.textureFilter GL.Texture2D
        $= ((GL.Nearest, Nothing), GL.Nearest)
      GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 1)
        GL.Texture2D gNormal 0

      gAlbedo <- genObjectName
      GL.textureBinding GL.Texture2D $= Just gAlbedo
      GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA'
        (TextureSize2D bufferWidth bufferHeight) 0
          (GL.PixelData GL.RGBA GL.Float nullPtr)
      GL.textureFilter GL.Texture2D
        $= ((GL.Nearest, Nothing), GL.Nearest)
      GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 2)
        GL.Texture2D gAlbedo 0

      GL.drawBuffers $= [GL.FBOColorAttachment 0, GL.FBOColorAttachment 1, GL.FBOColorAttachment 2]
      rboDepth <- genObjectName
      GL.bindRenderbuffer GL.Renderbuffer $= rboDepth
      GL.renderbufferStorage GL.Renderbuffer GL.DepthComponent' (GL.RenderbufferSize bufferWidth bufferHeight)
      GL.framebufferRenderbuffer GL.Framebuffer GL.DepthAttachment GL.Renderbuffer rboDepth

      GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject


      ssaoFBO <- genObjectName
      ssaoBlurFBO <- genObjectName

      GL.bindFramebuffer GL.Framebuffer $= ssaoFBO
      ssaoColorBuffer <- genObjectName
      GL.textureBinding GL.Texture2D $= Just ssaoColorBuffer
      GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.R16
        (TextureSize2D bufferWidth bufferHeight) 0
          (GL.PixelData GL.Red GL.Float nullPtr)
      GL.textureFilter GL.Texture2D
        $= ((GL.Nearest, Nothing), GL.Nearest)
      GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0)
        GL.Texture2D ssaoColorBuffer 0

      GL.bindFramebuffer GL.Framebuffer $= ssaoBlurFBO
      ssaoColorBufferBlur <- genObjectName
      GL.textureBinding GL.Texture2D $= Just ssaoColorBufferBlur
      GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.R16
        (TextureSize2D bufferWidth bufferHeight) 0
          (GL.PixelData GL.Red GL.Float nullPtr)
      GL.textureFilter GL.Texture2D
        $= ((GL.Linear', Nothing), GL.Linear')
      GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0)
        GL.Texture2D ssaoColorBufferBlur 0

      GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

      let
        kernelSize = 64
        rnds = take (kernelSize*4) $ (randomRs (0,1) (mkStdGen 90) :: [GLfloat])
      ssaoKernel <- return $ do
        i <- [0..(kernelSize-1)]
        let
          scale = fromIntegral i / fromIntegral kernelSize
          x1   = rnds!!(4*i+0)*2-1
          x2   = rnds!!(4*i+1)*2-1
          x3   = rnds!!(4*i+2)
          k    = rnds!!(4*i+3)
        return $ (lerp' 0.1 1 (scale*scale) * k) *^ (Linear.Metric.normalize $ V3 x1 x2 x3)
      -- print ssaoKernel
       
      let
        sizeOfNoiseTex = 4*4
        rnds2 = take (sizeOfNoiseTex*2) $ (randomRs (0,1) (mkStdGen 91) :: [GLfloat])
      ssaoNoise <- return $ do
        i <- [0..(sizeOfNoiseTex-1)]
        [rnds2!!(2*i+0)*2-1, rnds2!!(2*i+1)*2-1, 0]
      -- print ssaoNoise
    
      noiseTexture <- genObjectName
      GL.textureBinding GL.Texture2D $= Just noiseTexture
      withArray ssaoNoise $ \ptr -> do
        GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA32F
          (TextureSize2D 4 4) 0
            (GL.PixelData GL.RGB GL.UnsignedByte ptr)
      GL.textureFilter GL.Texture2D
        $= ((GL.Nearest, Nothing), GL.Nearest)
      GL.textureWrapMode GL.Texture2D GL.S
        $= (GL.Repeated, GL.Repeat)
      GL.textureWrapMode GL.Texture2D GL.T
        $= (GL.Repeated, GL.Repeat)

      cubeVBO <- genObjectName
      GL.bindBuffer GL.ArrayBuffer $= Just cubeVBO
      bufferListData GL.ArrayBuffer cubeVertices GL.StaticDraw
      cubeVAO <- genObjectName
      GL.bindVertexArrayObject $= Just cubeVAO
      initAttribVars sizeOfElm [(3, Just aLoc_aPos), (3, Just aLoc_aNormal), (2, Just aLoc_aTexCoords)]

      planeVBO <- genObjectName
      GL.bindBuffer GL.ArrayBuffer $= Just planeVBO
      bufferListData GL.ArrayBuffer planeVertices GL.StaticDraw
      planeVAO <- genObjectName
      GL.bindVertexArrayObject $= Just planeVAO
      initAttribVars sizeOfElm [(3, Just aLoc_aPos), (3, Just aLoc_aNormal), (2, Just aLoc_aTexCoords)]

      quadVBO <- genObjectName
      GL.bindBuffer GL.ArrayBuffer $= Just quadVBO
      bufferListData GL.ArrayBuffer quadVertices GL.StaticDraw
      quadVAO <- genObjectName
      GL.bindVertexArrayObject $= Just quadVAO
      initAttribVars sizeOfElm [(3, Just aLoc_aPos), (2, Just aLoc_aTexCoords)]

      skyboxVBO <- genObjectName
      GL.bindBuffer GL.ArrayBuffer $= Just skyboxVBO
      bufferListData GL.ArrayBuffer skyboxVertices GL.StaticDraw
      skyboxVAO <- genObjectName
      GL.bindVertexArrayObject $= Just skyboxVAO
      initAttribVars sizeOfElm [(3, Just aLoc_aPos)]

      diffuseMap   <- loadTexture2D $ "./container2.jpg"
      specularMap  <- loadTexture2D $ "./container2_specular.jpg"
      floorTexture <- loadTexture2D $ "./grass.jpg"
      (Just cubemapTexture) <- loadCubemap (fmap (++".bmp") $ ["right","left","top","bottom","front","back"])

      let 
        shadowWidth  = 1024
        shadowHeight = 1024

      depthMapFBO <- genObjectName
      depthMap    <- genObjectName
      GL.textureBinding GL.Texture2D $= Just depthMap
      GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.DepthComponent'
        (TextureSize2D shadowWidth shadowHeight) 0
          (GL.PixelData GL.DepthComponent GL.Float nullPtr)
      GL.textureFilter GL.Texture2D
        $= ((GL.Nearest, Nothing), GL.Nearest)
      GL.textureWrapMode GL.Texture2D GL.S
        $= (GL.Repeated, GL.ClampToBorder)
      GL.textureWrapMode GL.Texture2D GL.T
        $= (GL.Repeated, GL.ClampToBorder)
      GL.textureBorderColor GL.Texture2D $= Color4 1 1 1 1

      GL.bindFramebuffer GL.Framebuffer $= depthMapFBO
      GL.framebufferTexture2D GL.Framebuffer GL.DepthAttachment
        GL.Texture2D depthMap 0
      GL.drawBuffer $= GL.NoBuffers
      GL.readBuffer $= GL.NoBuffers
      GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject


      GL.currentProgram $= Just lightingShader
      GL.uniform uLoc_A_gPosition         $= (0::GLint)
      GL.uniform uLoc_A_gNormal           $= (1::GLint)
      GL.uniform uLoc_A_gAlbedo           $= (2::GLint)
      GL.uniform uLoc_A_ssao              $= (3::GLint)

      GL.currentProgram $= Just skyboxShader
      GL.uniform uLoc_B_skybox            $= (0::GLint)

      GL.currentProgram $= Just geopassShader
      GL.uniform uLoc_D_material_diffuse  $= (0::GLint)
      GL.uniform uLoc_D_material_specular $= (1::GLint)
      GL.uniform uLoc_D_shadowMap         $= (2::GLint)

      GL.currentProgram $= Just ssaoShader
      GL.uniform uLoc_E_gPosition         $= (0::GLint)
      GL.uniform uLoc_E_gNormal           $= (1::GLint)
      GL.uniform uLoc_E_texNoise          $= (2::GLint)
      foldr (>>) (return ()) $ do
        i <- [0..(64-1)]
        return $ GL.uniform (uLoc_E_samples !! i)  $= v3ToVector3 (ssaoKernel !! i)

      GL.currentProgram $= Just ssaoBlurShader
      GL.uniform uLoc_F_ssaoInput         $= (0::GLint)

      GL.currentProgram $= Nothing
      GL.bindBuffer GL.ArrayBuffer $= Nothing
      GL.bindVertexArrayObject     $= Nothing

      let
        material_B = MyMaterial2 (Just diffuseMap) (Just specularMap) 64
        modelData = (ModelData (makeListOfVAOs planeVAO cubeVAO) []) `addObject`
          (Obj_Plane, MyMaterial2 (Just floorTexture) Nothing 64, identity) `addObject`
          (Obj_Cube, material_B, identity) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 10.5 0 3)) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 2 0 (-4))) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 (-3) 0 (-3))) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 1 0 4.5)) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 7.2 0 (-1.4))) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 (-9) 0 (-1))) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 (-0.2) 0 (-1.19))) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 (-0.59) 0 1)) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 (-1.89) 0 0)) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 (-0.1) 1 (-0.5))) `addObject`
          (Obj_Cube, material_B, (mat4_translate (V3 (-3+0.59) ((1+sqrt 3)/4-0.5) (-3.5-((1+sqrt 3)/4)))) !*! mat4_rotate (V3 1 0 0) (radians (-60))) `addObject`
          (Obj_Cube, material_B, mat4_translate (V3 (-5) 0 2))
      
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
      yaw       $= 0
      pitch     $= 0
      
      cameraPos $= V3 0 0 7
      cameraDir $= calcDirectionVector 0 0
      cameraUp  $= V3 0 1 0
      lightPos  $= (21 *^ V3 (-0.41925156) 0.49880368 (-0.75856644))

      fix $ \rec -> do

        b <- GLFW.windowShouldClose window
        
        if b then
          return ()
          
        else do
          (_, (Size scrWidth scrHeight)) <- get $ GL.viewport

          -- GL.clearColor $= GL.Color4 (0.5*0.19) (0.8*0.19) (0.9*0.19) 1
          GL.clearColor $= GL.Color4 0 0 0 1
          
          t_maybe <- getTime
          case t_maybe of
            Just t -> do
              currentCameraPos <- get cameraPos
              currentCameraDir <- get cameraDir
              currentCameraUp  <- get cameraUp
              currentLightPos  <- get lightPos
              currentYaw       <- get yaw

              processInput window myState (realToFrac t)

              lightSpaceMatrix <- mat4ToGLmatrix $
                (mat4_ortho 20 20 1 27.5) !*!
                (mat4_lookAt ((8/21)*^currentLightPos) (V3 0 0 0) (V3 0 1 0))


              view_ <- return $ mat4_lookAt currentCameraPos (currentCameraPos + currentCameraDir) currentCameraUp
              view <- mat4ToGLmatrix $ view_
              view2 <- mat4ToGLmatrix $
                removeTranslation $ view_
              projection <- mat4ToGLmatrix $
                mat4_perspective (radians 45)
                  ((fromIntegral scrWidth)/(fromIntegral scrHeight)) 0.1 100

              model2 <- mat4ToGLmatrix $
                mat4_translate currentLightPos !*!
                mat4_rotate (V3 0.3 0.2 0.1) (realToFrac t * radians (-55)) !*!
                mat4_scale 0.2 0.2 0.2

              -- compute the depth data for shadow mapping 
              GL.cullFace  $= Just GL.Front
              GL.currentProgram $= Just simpleDepthShader

              GL.uniform uLoc_C_lightSpaceMatrix $= lightSpaceMatrix

              GL.viewport $= (GL.Position 0 0, GL.Size shadowWidth shadowHeight)
              GL.bindFramebuffer GL.Framebuffer $= depthMapFBO
              GL.clear [GL.DepthBuffer]
              renderScene modelData (uLoc_C_model, Nothing, Nothing, Nothing) 
              GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

              GL.cullFace  $= Just GL.Back
              GL.viewport $= (GL.Position 0 0, GL.Size bufferWidth bufferHeight)
              GL.clear [GL.ColorBuffer, GL.DepthBuffer]

              -- create gBuffers
              GL.bindFramebuffer GL.Framebuffer $= gBuffer
              GL.clear [GL.ColorBuffer, GL.DepthBuffer]
              GL.currentProgram $= Just geopassShader
              GL.uniform uLoc_D_view               $= view
              GL.uniform uLoc_D_projection         $= projection
              GL.uniform uLoc_D_lightSpaceMatrix   $= lightSpaceMatrix
              GL.activeTexture $= GL.TextureUnit 2
              GL.textureBinding GL.Texture2D $= Just depthMap
              renderScene modelData (uLoc_D_model, Nothing, Nothing, Just uLoc_D_material_shininess) 

              -- drawing skybox
              GL.depthFunc $= Just GL.Lequal
              GL.currentProgram $= Just skyboxShader

              GL.uniform uLoc_B_view        $= view2
              GL.uniform uLoc_B_projection  $= projection

              GL.bindVertexArrayObject $= Just skyboxVAO
              GL.activeTexture $= GL.TextureUnit 0
              GL.textureBinding GL.TextureCubeMap $= Just cubemapTexture
              GL.drawArrays GL.Triangles 0 36
              GL.bindVertexArrayObject $= Nothing

              GL.depthFunc $= Just GL.Less

              GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

              -- SSAO
              GL.bindFramebuffer GL.Framebuffer $= ssaoFBO
              GL.clear [GL.ColorBuffer]
              GL.currentProgram $= Just ssaoShader
              GL.uniform uLoc_E_projection         $= projection
              GL.activeTexture $= GL.TextureUnit 0
              GL.textureBinding GL.Texture2D $= Just gPosition
              GL.activeTexture $= GL.TextureUnit 1
              GL.textureBinding GL.Texture2D $= Just gNormal
              GL.activeTexture $= GL.TextureUnit 2
              GL.textureBinding GL.Texture2D $= Just noiseTexture
              GL.bindVertexArrayObject $= Just quadVAO
              GL.drawArrays GL.TriangleStrip 0 4
              GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

              -- SSAO Blur
              GL.bindFramebuffer GL.Framebuffer $= ssaoBlurFBO
              GL.clear [GL.ColorBuffer]
              GL.currentProgram $= Just ssaoBlurShader
              GL.activeTexture $= GL.TextureUnit 0
              GL.textureBinding GL.Texture2D $= Just ssaoColorBuffer
              GL.bindVertexArrayObject $= Just quadVAO
              GL.drawArrays GL.TriangleStrip 0 4
              GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

              -- lighting
              GL.viewport $= (GL.Position 0 0, GL.Size scrWidth scrHeight)
              GL.currentProgram $= Just lightingShader
              GL.uniform uLoc_A_light_position     $= (v3ToVector3 $ (view_ !* point currentLightPos) ^._xyz)
              GL.uniform uLoc_A_light_ambient      $= (Vector3 0.2 0.2 0.2 :: Vector3 GLfloat)
              GL.uniform uLoc_A_light_diffuse      $= (Vector3 0.5 0.5 0.5 :: Vector3 GLfloat)
              GL.uniform uLoc_A_light_specular     $= (Vector3 1 1 1 :: Vector3 GLfloat)


              GL.activeTexture $= GL.TextureUnit 0
              GL.textureBinding GL.Texture2D $= Just gPosition
              GL.activeTexture $= GL.TextureUnit 1
              GL.textureBinding GL.Texture2D $= Just gNormal
              GL.activeTexture $= GL.TextureUnit 2
              GL.textureBinding GL.Texture2D $= Just gAlbedo
              GL.activeTexture $= GL.TextureUnit 3
              GL.textureBinding GL.Texture2D $= Just ssaoColorBufferBlur
              GL.bindVertexArrayObject $= Just quadVAO
              GL.drawArrays GL.TriangleStrip 0 4
              
            Nothing -> return ()

          GLFW.swapBuffers window
          GLFW.pollEvents
          
          rec

      freeMyStateVars myState

      deleteObjectName cubeVAO
      deleteObjectName cubeVBO
      deleteObjectName planeVAO
      deleteObjectName planeVBO
      deleteObjectName quadVAO
      deleteObjectName quadVBO
      deleteObjectName skyboxVAO
      deleteObjectName skyboxVBO
      deleteObjectName lightingShader
      deleteObjectName skyboxShader

      GLFW.terminate




data ObjType   = Obj_Plane | Obj_Cube deriving Enum

makeListOfVAOs :: VertexArrayObject -> VertexArrayObject -> [VertexArrayObject]
makeListOfVAOs plane cube = do
  i <- [0..1]
  if i == fromEnum Obj_Plane then
    return plane
  else if i == fromEnum Obj_Cube then
    return cube
  else
    []

data MaterialType  = Default | MyMaterial1 (V3 GLfloat) (V3 GLfloat) GLfloat | MyMaterial2 (Maybe TextureObject) (Maybe TextureObject) GLfloat
type UniformLocs = (UniformLocation, Maybe UniformLocation, Maybe UniformLocation, Maybe UniformLocation)
data ModelData = ModelData [VertexArrayObject] [((ObjType, MaterialType), M44 GLfloat)]

emptyData :: ModelData
emptyData = (ModelData [] [])

addObject :: ModelData -> (ObjType, MaterialType, M44 GLfloat) -> ModelData
addObject (ModelData objInfo objData) (a, b, c) = (ModelData objInfo (((a,b),c):objData))

renderScene :: ModelData -> UniformLocs -> IO ()
renderScene (ModelData objInfo objData) (modelLoc, difLoc', specLoc', shLoc') = do
  let
    uniforms' = do
      (x,i) <- zip [difLoc', specLoc', shLoc'] [0..2]
      case x of
        Just x' -> return (x',i)
        Nothing -> []
    ignoreMaterial = null uniforms'
  foldr (>>) (return ()) $ do
    i <- enumFromTo 0 (length objData - 1)
    let
      objType      = fst.fst $ objData !! i
      materialType = snd.fst $ objData !! i
      modelMat     = snd     $ objData !! i
      
    return $ do  
      mat4ToGLmatrix modelMat >>= \model ->
        GL.uniform modelLoc $= model
  
      case objType of
        Obj_Plane -> do
          GL.bindVertexArrayObject $= Just (objInfo !! fromEnum Obj_Plane)
          if ignoreMaterial then
            return ()
          else
            case materialType of
              MyMaterial2 diffuseTex specularTex shininess -> do
                if snd (uniforms' !! 0) == 2 then do
                  GL.uniform (fst $ uniforms' !! 0) $= shininess
                  GL.activeTexture $= GL.TextureUnit 0
                  GL.textureBinding GL.Texture2D      $= diffuseTex
                  GL.activeTexture $= GL.TextureUnit 1
                  GL.textureBinding GL.Texture2D      $= specularTex
                else
                  return ()
                  
              _ -> return ()
          GL.drawArrays GL.Triangles 0 6
          GL.bindVertexArrayObject $= Nothing
  
        Obj_Cube -> do
          GL.bindVertexArrayObject $= Just (objInfo !! fromEnum Obj_Cube)
          if ignoreMaterial then
            return ()
          else
            case materialType of
              MyMaterial2 diffuseTex specularTex shininess -> do
                if snd (uniforms' !! 0) == 2 then do
                  GL.uniform (fst $ uniforms' !! 0) $= shininess
                  GL.activeTexture $= GL.TextureUnit 0
                  GL.textureBinding GL.Texture2D      $= diffuseTex
                  GL.activeTexture $= GL.TextureUnit 1
                  GL.textureBinding GL.Texture2D      $= specularTex
                else
                  return ()
              _ -> return ()
          GL.drawArrays GL.Triangles 0 36
          GL.bindVertexArrayObject $= Nothing




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

mat4_ortho :: (Floating a) => a -> a -> a -> a -> M44 a
mat4_ortho npWidth npHeight near far =
  (V4 (V4 (1/npWith_2) 0 0 0) (V4 0 (1/npHeight_2) 0 0)
    (V4 0 0 (-2/d) (-1*(far+near)/d)) (V4 0 0 0 1))
  where
    d = far - near
    npWith_2   = npWidth/2
    npHeight_2 = npHeight/2

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
  [(-1), 1, (-1)
  ,(-1), (-1), (-1)
  ,1, (-1), (-1)
  ,1, (-1), (-1)
  ,1, 1, (-1)
  ,(-1), 1, (-1)

  ,(-1), (-1), 1
  ,(-1), (-1), (-1)
  ,(-1), 1, (-1)
  ,(-1), 1, (-1)
  ,(-1), 1, 1
  ,(-1), (-1), 1

  ,1, (-1), (-1)
  ,1, (-1), 1
  ,1, 1, 1
  ,1, 1, 1
  ,1, 1, (-1)
  ,1, (-1), (-1)

  ,(-1), (-1), 1
  ,(-1), 1, 1
  ,1, 1, 1
  ,1, 1, 1
  ,1, (-1), 1
  ,(-1), (-1), 1

  ,(-1), 1, (-1)
  ,1, 1, (-1)
  ,1, 1, 1
  ,1, 1, 1
  ,(-1), 1, 1
  ,(-1), 1, (-1)

  ,(-1), (-1), (-1)
  ,(-1), (-1), 1
  ,1, (-1), (-1)
  ,1, (-1), (-1)
  ,(-1), (-1), 1
  ,1, (-1), 1]

model_cube :: [GLfloat]
model_cube =
  [(-0.5), (-0.5), (-0.5), 0, 0, (-1), 0, 0
  ,(-0.5), 0.5, (-0.5), 0, 0, (-1), 0, 1
  ,0.5, 0.5, (-0.5), 0, 0, (-1), 1, 1
  ,0.5, 0.5, (-0.5), 0, 0, (-1), 1, 1
  ,0.5, (-0.5), (-0.5), 0, 0, (-1), 1, 0
  ,(-0.5), (-0.5), (-0.5), 0, 0, (-1), 0, 0

  ,(-0.5), (-0.5), 0.5, 0, 0, 1, 0, 0
  ,0.5, (-0.5), 0.5, 0, 0, 1, 1, 0
  ,0.5, 0.5, 0.5, 0, 0, 1, 1, 1
  ,0.5, 0.5, 0.5, 0, 0, 1, 1, 1
  ,(-0.5), 0.5, 0.5, 0, 0, 1, 0, 1
  ,(-0.5), (-0.5), 0.5, 0, 0, 1, 0, 0

  ,(-0.5), 0.5, 0.5, (-1), 0, 0, 1, 0
  ,(-0.5), 0.5, (-0.5), (-1), 0, 0, 1, 1
  ,(-0.5), (-0.5), (-0.5), (-1), 0, 0, 0, 1
  ,(-0.5), (-0.5), (-0.5), (-1), 0, 0, 0, 1
  ,(-0.5), (-0.5), 0.5, (-1), 0, 0, 0, 0
  ,(-0.5), 0.5, 0.5, (-1), 0, 0, 1, 0

  ,0.5, 0.5, 0.5, 1, 0, 0, 1, 0
  ,0.5, (-0.5), 0.5, 1, 0, 0, 0, 0
  ,0.5, (-0.5), (-0.5), 1, 0, 0, 0, 1
  ,0.5, (-0.5), (-0.5), 1, 0, 0, 0, 1
  ,0.5, 0.5, (-0.5), 1, 0, 0, 1, 1
  ,0.5, 0.5, 0.5, 1, 0, 0, 1, 0

  ,(-0.5), (-0.5), (-0.5), 0, (-1), 0, 0, 1
  ,0.5, (-0.5), (-0.5), 0, (-1), 0, 1, 1
  ,0.5, (-0.5), 0.5, 0, (-1), 0, 1, 0
  ,0.5, (-0.5), 0.5, 0, (-1), 0, 1, 0
  ,(-0.5), (-0.5), 0.5, 0, (-1), 0, 0, 0
  ,(-0.5), (-0.5), (-0.5), 0, (-1), 0, 0, 1

  ,(-0.5), 0.5, (-0.5), 0, 1, 0, 0, 1
  ,(-0.5), 0.5, 0.5, 0, 1, 0, 0, 0
  ,0.5, 0.5, 0.5, 0, 1, 0, 1, 0
  ,0.5, 0.5, 0.5, 0, 1, 0, 1, 0
  ,0.5, 0.5, (-0.5), 0, 1, 0, 1, 1
  ,(-0.5), 0.5, (-0.5), 0, 1, 0, 0, 1]

planeVertices :: [GLfloat]
planeVertices =
  [(-210), (-0.5), (-210), 0, 1, 0,  0, 210
  ,(-210), (-0.5), 210, 0, 1, 0,  0, 0
  ,210, (-0.5), 210, 0, 1, 0, 210, 0

  ,210, (-0.5), (-210), 0, 1, 0, 210, 210
  ,(-210), (-0.5), (-210), 0, 1, 0,  0, 210  
  ,210, (-0.5), 210, 0, 1, 0, 210, 0]


quadVertices :: [GLfloat]
quadVertices = 
  [(-1), 1, 0, 0, 1
  ,(-1), (-1), 0, 0, 0
  ,1, 1, 0, 1, 1
  ,1, (-1), 0, 1, 0]


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

lerp' :: (Num a) => a -> a -> a -> a
lerp' a b t = a + t*(b-a)
