// Copyright (C) 2006-2010 NeoAxis Group Ltd.
using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing.Design;
using System.ComponentModel;
using System.IO;
using System.Diagnostics;
using System.Reflection;
using Engine;
using Engine.Renderer;
using Engine.MathEx;
using Engine.MapSystem;
using Engine.FileSystem;
using Engine.Utils;

namespace GAPLA_COMMON
{
    /// <summary>
    /// SodanKerjuu's amazing template material system.
    /// </summary>
    [Description("SodanKerjuu's amazing template material system.")]
    public class TemplateMaterial : HighLevelMaterial
    {
        //General
        MaterialBlendingTypes blending;
        bool lighting = true;
        bool doubleSided = false;
        bool receiveShadows = true;
        CompareFunction alphaRejectFunction = CompareFunction.AlwaysPass;
        byte alphaRejectValue = 127;
        bool alphaToCoverage;
        bool allowFog = true;
        bool depthWrite = true;
        bool depthTest = true;
        float depthOffset = 0;
        bool skipDirectionalLights = false;
        bool disableInstancing = false;
        bool useEnvironmentZones = false;

        //Shaders
        string defaultShader = "COMMON\\Shaders\\TextureBlaster.shader";
        string shadowCasterShader = "COMMON\\Shaders\\RejectShadowCaster.shader";
        List<SchemeShader> materialSchemeShaders = new List<SchemeShader>();

        //Arguments
        List<Definition> definitions = new List<Definition>();

        //Texturemaps
        MapItem textureMap0;
        MapItem textureMap1;
        MapItem textureMap2;
        MapItem textureMap3;
        MapItem textureMap4;
        MapItem textureMap5;
        MapItem textureMap6;
        MapItem textureMap7;
        MapItem textureMap8;
        MapItem textureMap9;

        //for cubemap reflections and environment variables
        List<Pair<Pass, TextureUnitState>> environmentEventUnitStates;

        //for maps animations
        List<MapItem> mapsWithAnimations;

        List<Pass> subscribedPassesForRenderObjectPass;

        string defaultTechniqueErrorString;
        bool fixedPipelineInitialized;

        ///////////////////////////////////////////

        //gpu parameters constants
        public enum GpuParameters
        {
            textureMap0TransformMul, textureMap0TransformAdd,
            textureMap1TransformMul, textureMap1TransformAdd,
            textureMap2TransformMul, textureMap2TransformAdd,
            textureMap3TransformMul, textureMap3TransformAdd,
            textureMap4TransformMul, textureMap4TransformAdd,
            textureMap5TransformMul, textureMap5TransformAdd,
            textureMap6TransformMul, textureMap6TransformAdd,
            textureMap7TransformMul, textureMap7TransformAdd,
            textureMap8TransformMul, textureMap8TransformAdd,
            textureMap9TransformMul, textureMap9TransformAdd,
            dynamicColor1, dynamicColor2, dynamicColor3, 
            dynamicColor4, dynamicColor5, dynamicColor6,
            dynamicScalars1, dynamicScalars2,
            environmentZoneValues,
        }

        ///////////////////////////////////////////

        public enum MaterialBlendingTypes
        {
            Opaque,
            AlphaAdd,
            AlphaBlend,
        }

        ///////////////////////////////////////////

        public enum UVIndexes
        {
            UVChannel0,
            UVChannel1,
            UVChannel2,
            UVChannel3,
        }

        ///////////////////////////////////////////

        public class SchemeShader
        {
            string materialSchemeName;

            public string MaterialSchemeName
            {
                get {return materialSchemeName;}
                set {materialSchemeName = value;}
            }

            string shaderFile;

            [Editor(typeof(EditorShaderUITypeEditor), typeof(UITypeEditor))]
            [RefreshProperties(RefreshProperties.Repaint)]
            public string ShaderFile
            {
                get {return shaderFile;}
                set {shaderFile = value;}
            }

            public override string ToString()
            {
                string text = "";
                if (string.IsNullOrEmpty(materialSchemeName) || string.IsNullOrEmpty(shaderFile))
                    text = "NOT ASSIGNED";
                else
                    text = materialSchemeName + " = " + shaderFile;

                return text;
            }

            public void Load(TextBlock block)
            {
                if (block.IsAttributeExist("materialSchemeName"))
                    materialSchemeName = block.GetAttribute("materialSchemeName");
                if (block.IsAttributeExist("shaderFile"))
                    shaderFile = block.GetAttribute("shaderFile");
            }

            public void Save(TextBlock block)
            {
                if (!string.IsNullOrEmpty(materialSchemeName))
                    block.SetAttribute("materialSchemeName", materialSchemeName.ToString());
                if (!string.IsNullOrEmpty(shaderFile))
                    block.SetAttribute("shaderFile", shaderFile.ToString());
            }

            public bool IsDataExists()
            {
                return !string.IsNullOrEmpty(materialSchemeName) || !string.IsNullOrEmpty(shaderFile);
            }

            internal void OnClone(SchemeShader source)
            {
                materialSchemeName = source.materialSchemeName;
                shaderFile = source.shaderFile;
            }
        }

        public class Definition
        {
            public enum DefinitionTypes
            {
                Label,
                Half1,
                Half2,
                Half3,
                Half4,
                Float1,
                Float2,
                Float3,
                Float4,
            }

            string definitionName;
            public string DefinitionName
            {
                get {return definitionName;}
                set {definitionName = value;}
            }

            DefinitionTypes definitionType = DefinitionTypes.Label;
            [DefaultValue(DefinitionTypes.Label)]
            public DefinitionTypes DefinitionType
            {
                get {return definitionType;}
                set {definitionType = value;}
            }

            Vec4 values = Vec4.Zero;
            [DefaultValue(typeof(Vec4), "0 0 0 0")]
            public Vec4 Values
            {
                get {return values;}
                set {values = value;}
            }

            public override string ToString()
            {
                string text = "";
                if (string.IsNullOrEmpty(definitionName))
                    text = "NOT ASSIGNED";
                else
                {
                    text = "(" + definitionType.ToString() + ") " + definitionName;

                    if (definitionType != DefinitionTypes.Label)
                        text += " = ";

                    switch (definitionType)
                    {
                        case Definition.DefinitionTypes.Half1:
                        case Definition.DefinitionTypes.Float1:
                            text += values.X.ToString();
                            break;

                        case Definition.DefinitionTypes.Half2:
                        case Definition.DefinitionTypes.Float2:
                            text += values.X.ToString();
                            text += " " + values.Y.ToString();
                            break;
                        
                        case Definition.DefinitionTypes.Half3:
                        case Definition.DefinitionTypes.Float3:
                            text += values.X.ToString();
                            text += " " + values.Y.ToString();
                            text += " " + values.Z.ToString();
                            break;

                        case Definition.DefinitionTypes.Half4:
                        case Definition.DefinitionTypes.Float4:
                            text += values.ToString();
                            break;
                    }
                }

                return text;
            }

            public void Load(TextBlock block)
            {
                if (block.IsAttributeExist("definitionName"))
                    definitionName = block.GetAttribute("definitionName");

                if (block.IsAttributeExist("definitionType"))
                    definitionType = (DefinitionTypes)Enum.Parse(typeof(DefinitionTypes), block.GetAttribute("definitionType"));

                if (block.IsAttributeExist("values"))
                    values = Vec4.Parse(block.GetAttribute("values"));
            }

            public void Save(TextBlock block)
            {
                if (!string.IsNullOrEmpty(definitionName))
                    block.SetAttribute("definitionName", definitionName.ToString());

                if (definitionType != DefinitionTypes.Label)
                    block.SetAttribute("definitionType", definitionType.ToString());

                if (values != Vec4.Zero)
                    block.SetAttribute("values", values.ToString());
            }

            public bool IsDataExists()
            {
                return !string.IsNullOrEmpty(definitionName) || definitionType != DefinitionTypes.Label || values != Vec4.Zero;
            }

            internal void OnClone(Definition source)
            {
                definitionName = source.definitionName;
                definitionType = source.definitionType;
                values = source.values;
            }
        }

        ///////////////////////////////////////////

        //for expand properties and allow change texture name from the group textbox in the propertyGrid
        public class MapItemTypeConverter : ExpandableObjectConverter
        {
            public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
            {
                return sourceType == typeof(string);
            }

            public override object ConvertFrom(ITypeDescriptorContext context,
                System.Globalization.CultureInfo culture, object value)
            {
                if (value.GetType() == typeof(string))
                {
                    PropertyInfo property = typeof(TemplateMaterial).GetProperty(
                        context.PropertyDescriptor.Name);
                    MapItem map = (MapItem)property.GetValue(context.Instance, null);
                    map.TextureFile = (string)value;
                    return map;
                }
                return base.ConvertFrom(context, culture, value);
            }
        }

        ///////////////////////////////////////////

        //SodanKerjuu: special ShaderTypeEditor for shader files
        public class EditorShaderUITypeEditor : UITypeEditor
        {
            public override object EditValue(ITypeDescriptorContext context, IServiceProvider provider, object value)
            {
                string path = "";

                if (ResourceUtils.DoUITypeEditorEditValueDelegate("Shader", ref path, null))
                {
                    return path;
                }

                return value;
            }

            public override UITypeEditorEditStyle GetEditStyle(ITypeDescriptorContext context)
            {
                return UITypeEditorEditStyle.Modal;
            }
        }

        ///////////////////////////////////////////

        //special EditorTextureUITypeEditor for MapItem classes
        public class MapItemEditorTextureUITypeEditor : UITypeEditor
        {
            public override object EditValue(ITypeDescriptorContext context,
                IServiceProvider provider, object value)
            {
                MapItem map = (MapItem)value;

                string path = map.TextureFile;
                if (ResourceUtils.DoUITypeEditorEditValueDelegate("Texture", ref path, null))
                {
                    if (path == null)
                        path = "";

                    //create new MapItem and copy properties.
                    //it is need for true property grid updating.
                    Type type = map.GetType();
                    ConstructorInfo constructor = type.GetConstructor(
                        BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public,
                        null, new Type[] { typeof(TemplateMaterial) }, null);
                    MapItem newMap = (MapItem)constructor.Invoke(new object[] { map.owner });
                    newMap.OnClone(map);

                    newMap.TextureFile = path;

                    return newMap;
                }

                return value;
            }

            public override UITypeEditorEditStyle GetEditStyle(ITypeDescriptorContext context)
            {
                return UITypeEditorEditStyle.Modal;
            }
        }

        ///////////////////////////////////////////

        [TypeConverter(typeof(MapItemTypeConverter))]
        [Editor(typeof(MapItemEditorTextureUITypeEditor), typeof(UITypeEditor))]
        public class MapItem
        {
            internal TemplateMaterial owner;
            string textureFile = "";
            UVIndexes uvChannel = UVIndexes.UVChannel0;
            TextureAddressingMode addressingMode = TextureAddressingMode.Wrap;
            Texture.Type textureType = Texture.Type.Type2D;
            TransformItem transform;

            internal List<TextureUnitState> textureUnitStatesForFixedPipeline;

            //

            //internal MapItem(TemplateMaterial owner)
            public MapItem(TemplateMaterial owner)
            {
                this.owner = owner;
                transform = new TransformItem(this);
            }

            [Editor(typeof(EditorTextureUITypeEditor), typeof(UITypeEditor))]
            [RefreshProperties(RefreshProperties.Repaint)]
            public string TextureFile
            {
                get { return textureFile; }
                set { textureFile = value; }
            }

            [DefaultValue(UVIndexes.UVChannel0)]
            [RefreshProperties(RefreshProperties.Repaint)]
            public UVIndexes UVChannel
            {
                get { return uvChannel; }
                set { uvChannel = value; }
            }

            [DefaultValue(TextureAddressingMode.Wrap)]
            [RefreshProperties(RefreshProperties.Repaint)]
            public TextureAddressingMode AddressingMode
            {
                get { return addressingMode; }
                set { addressingMode = value; }
            }

            [DefaultValue(Texture.Type.Type2D)]
            [RefreshProperties(RefreshProperties.Repaint)]
            public Texture.Type TextureType
            {
                get { return textureType; }
                set 
                { 
                    textureType = value;

                    //need reload
                    if (!string.IsNullOrEmpty(textureFile))
                    {
                        TextureManager.Instance.Unload(textureFile);
                        TextureManager.Instance.Load(textureFile, textureType);
                    }
                }
            }

            public TransformItem Transform
            {
                get { return transform; }
                set { transform = value; }
            }

            public override string ToString()
            {
                if (string.IsNullOrEmpty(textureFile))
                    return "";
                return textureFile;
            }

            public virtual void Load(TextBlock block)
            {
                if (block.IsAttributeExist("textureFile"))
                    textureFile = block.GetAttribute("textureFile");

                if (block.IsAttributeExist("uvChannel"))
                    uvChannel = (UVIndexes)Enum.Parse(typeof(UVIndexes),
                        block.GetAttribute("uvChannel"));

                if (block.IsAttributeExist("addressingMode"))
                    addressingMode = (TextureAddressingMode)Enum.Parse(typeof(TextureAddressingMode),
                        block.GetAttribute("addressingMode"));

                if (block.IsAttributeExist("textureType"))
                    textureType = (Texture.Type)Enum.Parse(typeof(Texture.Type),
                        block.GetAttribute("textureType"));

                TextBlock transformBlock = block.FindChild("transform");
                if (transformBlock != null)
                    transform.Load(transformBlock);
            }

            public virtual void Save(TextBlock block)
            {
                if (!string.IsNullOrEmpty(textureFile))
                    block.SetAttribute("textureFile", textureFile);

                if (uvChannel != UVIndexes.UVChannel0)
                    block.SetAttribute("uvChannel", uvChannel.ToString());

                if (addressingMode != TextureAddressingMode.Wrap)
                    block.SetAttribute("addressingMode", addressingMode.ToString());

                if (textureType != Texture.Type.Type2D)
                    block.SetAttribute("textureType", textureType.ToString());

                if (transform.IsDataExists())
                {
                    TextBlock transformBlock = block.AddChild("transform");
                    transform.Save(transformBlock);
                }
            }

            public virtual bool IsDataExists()
            {
                return !string.IsNullOrEmpty(textureFile) || uvChannel != UVIndexes.UVChannel0 || transform.IsDataExists() ||
                    addressingMode != TextureAddressingMode.Wrap || textureType != Texture.Type.Type2D;
            }

            internal virtual void OnClone(MapItem source)
            {
                textureFile = source.textureFile;
                uvChannel = source.uvChannel;
                addressingMode = source.addressingMode;
                textureType = source.textureType;
                transform.OnClone(source.transform);
            }
        }

        ///////////////////////////////////////////

        [TypeConverter(typeof(ExpandableObjectConverter))]
        public class TransformItem
        {
            internal MapItem owner;
            Vec2 scroll;
            Vec2 scale = new Vec2(1, 1);
            float rotate;
            bool dynamicParameters;
            AnimationItem animation;

            //

            internal TransformItem(MapItem owner)
            {
                this.owner = owner;
                animation = new AnimationItem(this);
            }

            [DefaultValue(typeof(Vec2), "0 0")]
            [Editor(typeof(Vec2ValueEditor), typeof(UITypeEditor))]
            [EditorLimitsRange(-1, 1)]
            [RefreshProperties(RefreshProperties.Repaint)]
            public Vec2 Scroll
            {
                get { return scroll; }
                set
                {
                    if (scroll == value)
                        return;

                    scroll = value;

                    MapItem map = owner;
                    map.owner.UpdateMapTransformGpuParameters(map);

                    if (map.owner.fixedPipelineInitialized)
                        map.owner.UpdateMapTransformForFixedPipeline(map);
                }
            }

            [DefaultValue(typeof(Vec2), "1 1")]
            [Editor(typeof(Vec2ValueEditor), typeof(UITypeEditor))]
            [EditorLimitsRange(.1f, 30)]
            [RefreshProperties(RefreshProperties.Repaint)]
            public Vec2 Scale
            {
                get { return scale; }
                set
                {
                    if (scale == value)
                        return;

                    scale = value;

                    MapItem map = owner;
                    map.owner.UpdateMapTransformGpuParameters(map);

                    if (map.owner.fixedPipelineInitialized)
                        map.owner.UpdateMapTransformForFixedPipeline(map);
                }
            }

            [DefaultValue(0.0f)]
            [Editor(typeof(SingleValueEditor), typeof(UITypeEditor))]
            [EditorLimitsRange(-1, 1)]
            [RefreshProperties(RefreshProperties.Repaint)]
            public float Rotate
            {
                get { return rotate; }
                set
                {
                    if (rotate == value)
                        return;

                    rotate = value;

                    MapItem map = owner;
                    map.owner.UpdateMapTransformGpuParameters(map);

                    if (map.owner.fixedPipelineInitialized)
                        map.owner.UpdateMapTransformForFixedPipeline(map);
                }
            }

            [DefaultValue(false)]
            public bool DynamicParameters
            {
                get { return dynamicParameters; }
                set { dynamicParameters = value; }
            }

            public AnimationItem Animation
            {
                get { return animation; }
                set { animation = value; }
            }

            public override string ToString()
            {
                string text = "";
                if (scroll != Vec2.Zero)
                    text += string.Format("Scroll: {0}", scroll);
                if (scale != new Vec2(1, 1))
                {
                    if (text != "")
                        text += ", ";
                    text += string.Format("Scale: {0}", scale);
                }
                if (rotate != 0)
                {
                    if (text != "")
                        text += ", ";
                    text += string.Format("Rotate: {0}", rotate);
                }
                if (dynamicParameters)
                {
                    if (text != "")
                        text += ", ";
                    text += string.Format("Dynamic Parameters: {0}", dynamicParameters.ToString());
                }
                if (animation.IsDataExists())
                {
                    if (text != "")
                        text += ", ";
                    text += string.Format("Animation: {0}", animation.ToString());
                }
                return text;
            }

            public void Load(TextBlock block)
            {
                if (block.IsAttributeExist("scroll"))
                    scroll = Vec2.Parse(block.GetAttribute("scroll"));
                if (block.IsAttributeExist("scale"))
                    scale = Vec2.Parse(block.GetAttribute("scale"));
                if (block.IsAttributeExist("rotate"))
                    rotate = float.Parse(block.GetAttribute("rotate"));
                if (block.IsAttributeExist("dynamicParameters"))
                    dynamicParameters = bool.Parse(block.GetAttribute("dynamicParameters"));

                TextBlock animationBlock = block.FindChild("animation");
                if (animationBlock != null)
                    animation.Load(animationBlock);
            }

            public void Save(TextBlock block)
            {
                if (scroll != Vec2.Zero)
                    block.SetAttribute("scroll", scroll.ToString());
                if (scale != new Vec2(1, 1))
                    block.SetAttribute("scale", scale.ToString());
                if (rotate != 0)
                    block.SetAttribute("rotate", rotate.ToString());
                if (dynamicParameters)
                    block.SetAttribute("dynamicParameters", dynamicParameters.ToString());

                if (animation.IsDataExists())
                {
                    TextBlock animationBlock = block.AddChild("animation");
                    animation.Save(animationBlock);
                }
            }

            public bool IsDataExists()
            {
                return scroll != Vec2.Zero || scale != new Vec2(1, 1) ||
                    rotate != 0 || dynamicParameters || animation.IsDataExists();
            }

            internal void OnClone(TransformItem source)
            {
                scroll = source.scroll;
                scale = source.scale;
                rotate = source.rotate;
                dynamicParameters = source.dynamicParameters;
                animation.OnClone(source.animation);
            }
        }

        ///////////////////////////////////////////

        [TypeConverter(typeof(ExpandableObjectConverter))]
        public class AnimationItem
        {
            internal TransformItem owner;
            Vec2 scrollSpeed;
            Vec2 scrollRound;
            float rotateSpeed;

            internal AnimationItem(TransformItem owner)
            {
                this.owner = owner;
            }

            [DefaultValue(typeof(Vec2), "0 0")]
            [Editor(typeof(Vec2ValueEditor), typeof(UITypeEditor))]
            [EditorLimitsRange(-3, 3)]
            [RefreshProperties(RefreshProperties.Repaint)]
            public Vec2 ScrollSpeed
            {
                get { return scrollSpeed; }
                set
                {
                    if (scrollSpeed == value)
                        return;

                    scrollSpeed = value;

                    MapItem map = owner.owner;
                    map.owner.InitializeAndUpdateMapTransformGpuParameters(map);

                    if (map.owner.fixedPipelineInitialized)
                        map.owner.UpdateMapTransformForFixedPipeline(map);
                }
            }

            [DefaultValue(typeof(Vec2), "0 0")]
            [Editor(typeof(Vec2ValueEditor), typeof(UITypeEditor))]
            [EditorLimitsRange(0, 1)]
            [RefreshProperties(RefreshProperties.Repaint)]
            public Vec2 ScrollRound
            {
                get { return scrollRound; }
                set { scrollRound = value; }
            }

            [DefaultValue(0.0f)]
            [Editor(typeof(SingleValueEditor), typeof(UITypeEditor))]
            [EditorLimitsRange(-3, 3)]
            [RefreshProperties(RefreshProperties.Repaint)]
            public float RotateSpeed
            {
                get { return rotateSpeed; }
                set
                {
                    if (rotateSpeed == value)
                        return;

                    rotateSpeed = value;

                    MapItem map = owner.owner;
                    map.owner.InitializeAndUpdateMapTransformGpuParameters(map);

                    if (map.owner.fixedPipelineInitialized)
                        map.owner.UpdateMapTransformForFixedPipeline(map);
                }
            }

            public override string ToString()
            {
                string text = "";
                if (scrollSpeed != Vec2.Zero)
                    text += string.Format("Scroll: {0}", scrollSpeed);
                if (rotateSpeed != 0)
                {
                    if (text != "")
                        text += ", ";
                    text += string.Format("Rotate: {0}", rotateSpeed);
                }
                return text;
            }

            public void Load(TextBlock block)
            {
                if (block.IsAttributeExist("scrollSpeed"))
                    scrollSpeed = Vec2.Parse(block.GetAttribute("scrollSpeed"));
                if (block.IsAttributeExist("scrollRound"))
                    scrollRound = Vec2.Parse(block.GetAttribute("scrollRound"));
                if (block.IsAttributeExist("rotateSpeed"))
                    rotateSpeed = float.Parse(block.GetAttribute("rotateSpeed"));
            }

            public void Save(TextBlock block)
            {
                if (scrollSpeed != Vec2.Zero)
                    block.SetAttribute("scrollSpeed", scrollSpeed.ToString());
                if (scrollRound != Vec2.Zero)
                    block.SetAttribute("scrollRound", scrollRound.ToString());
                if (rotateSpeed != 0)
                    block.SetAttribute("rotateSpeed", rotateSpeed.ToString());
            }

            public bool IsDataExists()
            {
                return scrollSpeed != Vec2.Zero || scrollRound != Vec2.Zero || rotateSpeed != 0;
            }

            internal void OnClone(AnimationItem source)
            {
                scrollSpeed = source.scrollSpeed;
                scrollRound = source.scrollRound;
                rotateSpeed = source.rotateSpeed;
            }
        }

        ///////////////////////////////////////////

        [Category("_ShaderBase")]
        [DefaultValue(MaterialBlendingTypes.Opaque)]
        public MaterialBlendingTypes Blending
        {
            get { return blending; }
            set { blending = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(true)]
        public bool Lighting
        {
            get { return lighting; }
            set { lighting = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(false)]
        public bool DoubleSided
        {
            get { return doubleSided; }
            set { doubleSided = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(true)]
        public bool ReceiveShadows
        {
            get { return receiveShadows; }
            set { receiveShadows = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(CompareFunction.AlwaysPass)]
        public CompareFunction AlphaRejectFunction
        {
            get { return alphaRejectFunction; }
            set { alphaRejectFunction = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue((byte)127)]
        public byte AlphaRejectValue
        {
            get { return alphaRejectValue; }
            set { alphaRejectValue = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(false)]
        public bool AlphaToCoverage
        {
            get { return alphaToCoverage; }
            set { alphaToCoverage = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(true)]
        public bool AllowFog
        {
            get { return allowFog; }
            set { allowFog = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(false)]
        public bool SkipDirectionalLights
        {
            get { return skipDirectionalLights; }
            set { skipDirectionalLights = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(false)]
        public bool DisableInstancing
        {
            get { return disableInstancing; }
            set { disableInstancing = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(true)]
        [Description("Depth write flag will be automatically disabled if \"Blending\" not equal to \"Opaque\".")]
        public bool DepthWrite
        {
            get { return depthWrite; }
            set { depthWrite = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(true)]
        public bool DepthTest
        {
            get { return depthTest; }
            set { depthTest = value; }
        }

        [Category("_ShaderBase")]
        [DefaultValue(0.0f)]
        public float DepthOffset
        {
            get { return depthOffset; }
            set { depthOffset = value; }
        } 

        [Category("_ShaderBase")]
        [DefaultValue(false)]
        public bool UseEnvironmentZones
        {
            get { return useEnvironmentZones; }
            set { useEnvironmentZones = value; }
        }

        [Editor(typeof(EditorShaderUITypeEditor), typeof(UITypeEditor))]
        [RefreshProperties(RefreshProperties.Repaint)]
        [Category("Shaders")]
        [DefaultValue("COMMON\\Shaders\\TextureBlaster.shader")]
        public string DefaultShader
        {
            get { return defaultShader; }
            set { defaultShader = value; }
        }

        [Editor(typeof(EditorShaderUITypeEditor), typeof(UITypeEditor))]
        [RefreshProperties(RefreshProperties.Repaint)]
        [Category("Shaders")]
        [DefaultValue("COMMON\\Shaders\\RejectShadowCaster.shader")]
        public string ShadowCasterShader
        {
            get { return shadowCasterShader; }
            set { shadowCasterShader = value; }
        }

        [Category("Shaders")]
        public List<SchemeShader> MaterialSchemeShaders
        {
            get { return materialSchemeShaders; }
        }

        [Category("Arguments")]
        public List<Definition> Definitions
        {
            get { return definitions; }
        }

        [Category("Texturemaps")]
        public MapItem TextureMap0
        {
            get { return textureMap0; }
            set { textureMap0 = value; }
        }

        [Category("Texturemaps")]
        public MapItem TextureMap1
        {
            get { return textureMap1; }
            set { textureMap1 = value; }
        }

        [Category("Texturemaps")]
        public MapItem TextureMap2
        {
            get { return textureMap2; }
            set { textureMap2 = value; }
        }

        [Category("Texturemaps")]
        public MapItem TextureMap3
        {
            get { return textureMap3; }
            set { textureMap3 = value; }
        }

        [Category("Texturemaps")]
        public MapItem TextureMap4
        {
            get { return textureMap4; }
            set { textureMap4 = value; }
        }

        [Category("Texturemaps")]
        public MapItem TextureMap5
        {
            get { return textureMap5; }
            set { textureMap5 = value; }
        }

        [Category("Texturemaps")]
        public MapItem TextureMap6
        {
            get { return textureMap6; }
            set { textureMap6 = value; }
        }

        [Category("Texturemaps")]
        public MapItem TextureMap7
        {
            get { return textureMap7; }
            set { textureMap7 = value; }
        }

        [Category("Texturemaps")]
        public MapItem TextureMap8
        {
            get { return textureMap8; }
            set { textureMap8 = value; }
        }

        [Category("Texturemaps")]
        public MapItem TextureMap9
        {
            get { return textureMap9; }
            set { textureMap9 = value; }
        }

        public TemplateMaterial()
        {
            textureMap0 = new MapItem(this);
            textureMap1 = new MapItem(this);
            textureMap2 = new MapItem(this);
            textureMap3 = new MapItem(this);
            textureMap4 = new MapItem(this);
            textureMap5 = new MapItem(this);
            textureMap6 = new MapItem(this);
            textureMap7 = new MapItem(this);
            textureMap8 = new MapItem(this);
            textureMap9 = new MapItem(this);

            SceneManager.Instance.FogAndShadowSettingsChanged += SceneManager_FogAndShadowSettingsChanged;
        }

        public override void Dispose()
        {
            SceneManager.Instance.FogAndShadowSettingsChanged -= SceneManager_FogAndShadowSettingsChanged;
            base.Dispose();
        }

        protected override void OnClone(HighLevelMaterial sourceMaterial)
        {
            base.OnClone(sourceMaterial);

            TemplateMaterial source = (TemplateMaterial)sourceMaterial;

            //General
            blending = source.blending;
            lighting = source.lighting;
            doubleSided = source.doubleSided;
            receiveShadows = source.receiveShadows;
            alphaRejectFunction = source.alphaRejectFunction;
            alphaRejectValue = source.alphaRejectValue;
            alphaToCoverage = source.alphaToCoverage;
            allowFog = source.allowFog;
            depthWrite = source.depthWrite;
            depthTest = source.depthTest;
            skipDirectionalLights = source.skipDirectionalLights;
            disableInstancing = source.disableInstancing;
            depthOffset = source.depthOffset;
            useEnvironmentZones = source.useEnvironmentZones;

            //Texturemaps
            textureMap0.OnClone(source.textureMap0);
            textureMap1.OnClone(source.textureMap1);
            textureMap2.OnClone(source.textureMap2);
            textureMap3.OnClone(source.textureMap3);
            textureMap4.OnClone(source.textureMap4);
            textureMap5.OnClone(source.textureMap5);
            textureMap6.OnClone(source.textureMap6);
            textureMap7.OnClone(source.textureMap7);
            textureMap8.OnClone(source.textureMap8);
            textureMap9.OnClone(source.textureMap9);

            //Shaders
            defaultShader = source.defaultShader;
            shadowCasterShader = source.shadowCasterShader;
            materialSchemeShaders = source.materialSchemeShaders;
        }

        protected override bool OnLoad(TextBlock block)
        {
            if (!base.OnLoad(block))
                return false;

            //General
            {
                if (block.IsAttributeExist("blending"))
                    blending = (MaterialBlendingTypes)Enum.Parse(
                        typeof(MaterialBlendingTypes), block.GetAttribute("blending"));

                if (block.IsAttributeExist("lighting"))
                    lighting = bool.Parse(block.GetAttribute("lighting"));

                if (block.IsAttributeExist("doubleSided"))
                    doubleSided = bool.Parse(block.GetAttribute("doubleSided"));

                if (block.IsAttributeExist("receiveShadows"))
                    receiveShadows = bool.Parse(block.GetAttribute("receiveShadows"));

                if (block.IsAttributeExist("alphaRejectFunction"))
                    alphaRejectFunction = (CompareFunction)Enum.Parse(typeof(CompareFunction),
                        block.GetAttribute("alphaRejectFunction"));

                if (block.IsAttributeExist("alphaRejectValue"))
                    alphaRejectValue = byte.Parse(block.GetAttribute("alphaRejectValue"));

                if (block.IsAttributeExist("alphaToCoverage"))
                    alphaToCoverage = bool.Parse(block.GetAttribute("alphaToCoverage"));

                if (block.IsAttributeExist("allowFog"))
                    allowFog = bool.Parse(block.GetAttribute("allowFog"));

                if (block.IsAttributeExist("skipDirectionalLights"))
                    skipDirectionalLights = bool.Parse(block.GetAttribute("skipDirectionalLights"));

                if (block.IsAttributeExist("disableInstancing"))
                    disableInstancing = bool.Parse(block.GetAttribute("disableInstancing"));

                if (block.IsAttributeExist("depthWrite"))
                    depthWrite = bool.Parse(block.GetAttribute("depthWrite"));

                if (block.IsAttributeExist("depthTest"))
                    depthTest = bool.Parse(block.GetAttribute("depthTest"));

                if (block.IsAttributeExist("depthTest"))
                    depthTest = bool.Parse(block.GetAttribute("depthTest"));

                if (block.IsAttributeExist("depthOffset"))
                    depthOffset = float.Parse(block.GetAttribute("depthOffset"));

                if (block.IsAttributeExist("useEnvironmentZones"))
                    useEnvironmentZones = bool.Parse(block.GetAttribute("useEnvironmentZones"));
            }

            //Texturemaps
            {
                TextBlock textureMap0Block = block.FindChild("textureMap0");
                if (textureMap0Block != null)
                    textureMap0.Load(textureMap0Block);
                TextBlock textureMap1Block = block.FindChild("textureMap1");
                if (textureMap1Block != null)
                    textureMap1.Load(textureMap1Block);
                TextBlock textureMap2Block = block.FindChild("textureMap2");
                if (textureMap2Block != null)
                    textureMap2.Load(textureMap2Block);
                TextBlock textureMap3Block = block.FindChild("textureMap3");
                if (textureMap3Block != null)
                    textureMap3.Load(textureMap3Block);
                TextBlock textureMap4Block = block.FindChild("textureMap4");
                if (textureMap4Block != null)
                    textureMap4.Load(textureMap4Block);
                TextBlock textureMap5Block = block.FindChild("textureMap5");
                if (textureMap5Block != null)
                    textureMap5.Load(textureMap5Block);
                TextBlock textureMap6Block = block.FindChild("textureMap6");
                if (textureMap6Block != null)
                    textureMap6.Load(textureMap6Block);
                TextBlock textureMap7Block = block.FindChild("textureMap7");
                if (textureMap7Block != null)
                    textureMap7.Load(textureMap7Block);
                TextBlock textureMap8Block = block.FindChild("textureMap8");
                if (textureMap8Block != null)
                    textureMap8.Load(textureMap8Block);
                TextBlock textureMap9Block = block.FindChild("textureMap9");
                if (textureMap9Block != null)
                    textureMap9.Load(textureMap9Block);
            }

            //Shaderfiles
            {
                if (block.IsAttributeExist("defaultShader"))
                    defaultShader = block.GetAttribute("defaultShader");

                if (block.IsAttributeExist("shadowCasterShader"))
                    shadowCasterShader = block.GetAttribute("shadowCasterShader");

                int schemeNumber = 1;
                again:
                {
                    TextBlock schemeShaderBlock = block.FindChild("schemeShader" + schemeNumber.ToString());
                    if (schemeShaderBlock != null)
                    {
                        SchemeShader schemeShader = new SchemeShader();
                        schemeShader.Load(schemeShaderBlock);
                        materialSchemeShaders.Add(schemeShader);
                        schemeNumber++;
                        goto again;
                    }
                }
            }
            
            //Definitions
            {
                int definitionNumber = 1;
                again:
                {
                    TextBlock definitionBlock = block.FindChild("definition" + definitionNumber.ToString());
                    if (definitionBlock != null)
                    {
                        Definition definition = new Definition();
                        definition.Load(definitionBlock);
                        definitions.Add(definition);
                        definitionNumber++;
                        goto again;
                    }
                }
            }

            return true;
        }

        protected override void OnSave(TextBlock block)
        {
            base.OnSave(block);

            //General
            {
                if (blending != MaterialBlendingTypes.Opaque)
                    block.SetAttribute("blending", blending.ToString());

                if (!lighting)
                    block.SetAttribute("lighting", lighting.ToString());

                if (doubleSided)
                    block.SetAttribute("doubleSided", doubleSided.ToString());

                if (!receiveShadows)
                    block.SetAttribute("receiveShadows", receiveShadows.ToString());

                if (alphaRejectFunction != CompareFunction.AlwaysPass)
                    block.SetAttribute("alphaRejectFunction", alphaRejectFunction.ToString());

                if (alphaRejectValue != 127)
                    block.SetAttribute("alphaRejectValue", alphaRejectValue.ToString());

                if (alphaToCoverage)
                    block.SetAttribute("alphaToCoverage", alphaToCoverage.ToString());

                if (!allowFog)
                    block.SetAttribute("allowFog", allowFog.ToString());

                if (skipDirectionalLights)
                    block.SetAttribute("skipDirectionalLights", skipDirectionalLights.ToString());

                if (disableInstancing)
                    block.SetAttribute("disableInstancing", disableInstancing.ToString());

                if (!depthWrite)
                    block.SetAttribute("depthWrite", depthWrite.ToString());

                if (!depthTest)
                    block.SetAttribute("depthTest", depthTest.ToString());

                if (depthOffset != 0.0f)
                    block.SetAttribute("depthOffset", depthOffset.ToString());

                if (useEnvironmentZones)
                    block.SetAttribute("useEnvironmentZones", useEnvironmentZones.ToString());
            }

            //Texturemaps
            {
                if (textureMap0.IsDataExists())
                {
                    TextBlock textureMap0Block = block.AddChild("textureMap0");
                    textureMap0.Save(textureMap0Block);
                }
                if (textureMap1.IsDataExists())
                {
                    TextBlock textureMap1Block = block.AddChild("textureMap1");
                    textureMap1.Save(textureMap1Block);
                }
                if (textureMap2.IsDataExists())
                {
                    TextBlock textureMap2Block = block.AddChild("textureMap2");
                    textureMap2.Save(textureMap2Block);
                }
                if (textureMap3.IsDataExists())
                {
                    TextBlock textureMap3Block = block.AddChild("textureMap3");
                    textureMap3.Save(textureMap3Block);
                }
                if (textureMap4.IsDataExists())
                {
                    TextBlock textureMap4Block = block.AddChild("textureMap4");
                    textureMap4.Save(textureMap4Block);
                }
                if (textureMap5.IsDataExists())
                {
                    TextBlock textureMap5Block = block.AddChild("textureMap5");
                    textureMap5.Save(textureMap5Block);
                }
                if (textureMap6.IsDataExists())
                {
                    TextBlock textureMap6Block = block.AddChild("textureMap6");
                    textureMap6.Save(textureMap6Block);
                }
                if (textureMap7.IsDataExists())
                {
                    TextBlock textureMap7Block = block.AddChild("textureMap7");
                    textureMap7.Save(textureMap7Block);
                }
                if (textureMap8.IsDataExists())
                {
                    TextBlock textureMap8Block = block.AddChild("textureMap8");
                    textureMap8.Save(textureMap8Block);
                }
                if (textureMap9.IsDataExists())
                {
                    TextBlock textureMap9Block = block.AddChild("textureMap9");
                    textureMap9.Save(textureMap9Block);
                }
            }

            //Shaderfiles
            {
                if (defaultShader != "COMMON\\Shaders\\TextureBlaster.shader")
                    block.SetAttribute("defaultShader", defaultShader);

                if (shadowCasterShader != "COMMON\\Shaders\\RejectShadowCaster.shader")
                    block.SetAttribute("shadowCasterShader", shadowCasterShader);

                int schemeNumber = 1;
                foreach (SchemeShader schemeShader in materialSchemeShaders)
                {
                    if (schemeShader.IsDataExists())
                    {
                        TextBlock schemeShaderBlock = block.AddChild("schemeShader" + schemeNumber.ToString());
                        schemeShader.Save(schemeShaderBlock);
                        schemeNumber++;
                    }
                }
            }

            //Definitions
            {
                int definitionNumber = 1;
                foreach (Definition definition in definitions)
                {
                    if (definition.IsDataExists())
                    {
                        TextBlock definitionBlock = block.AddChild("definition" + definitionNumber.ToString());
                        definition.Save(definitionBlock);
                        definitionNumber++;
                    }
                }
            }
        }

        protected virtual void OnSetProgramAutoConstants(
            GpuProgramParameters parameters, int lightCount)
        {
            parameters.SetNamedAutoConstant("worldMatrix",
                GpuProgramParameters.AutoConstantType.WorldMatrix);
            parameters.SetNamedAutoConstant("worldViewMatrix",
                GpuProgramParameters.AutoConstantType.WorldViewMatrix);
            parameters.SetNamedAutoConstant("worldViewProjMatrix",
                GpuProgramParameters.AutoConstantType.WorldViewProjMatrix);
            parameters.SetNamedAutoConstant("viewProjMatrix",
                GpuProgramParameters.AutoConstantType.ViewProjMatrix);
            parameters.SetNamedAutoConstant("cameraPositionObjectSpace",
                GpuProgramParameters.AutoConstantType.CameraPositionObjectSpace);
            parameters.SetNamedAutoConstant("cameraPosition",
                GpuProgramParameters.AutoConstantType.CameraPosition);
            parameters.SetNamedAutoConstant("farClipDistance",
                GpuProgramParameters.AutoConstantType.FarClipDistance);

            parameters.SetNamedAutoConstant("texelOffsets",
                GpuProgramParameters.AutoConstantType.TexelOffsets);
            parameters.SetNamedAutoConstant("alphaRejectValue",
                GpuProgramParameters.AutoConstantType.AlphaRejectValue);

            parameters.SetNamedAutoConstant("drawShadowDebugging",
                GpuProgramParameters.AutoConstantType.DrawShadowDebugging);
            parameters.SetNamedAutoConstant("shadowDirectionalLightBias",
                GpuProgramParameters.AutoConstantType.ShadowDirectionalLightBias);
            parameters.SetNamedAutoConstant("shadowSpotLightBias",
                GpuProgramParameters.AutoConstantType.ShadowSpotLightBias);
            parameters.SetNamedAutoConstant("shadowPointLightBias",
                GpuProgramParameters.AutoConstantType.ShadowPointLightBias);


            //Light
            parameters.SetNamedAutoConstant("ambientLightColor",
                GpuProgramParameters.AutoConstantType.AmbientLightColor);

            if (lightCount != 0)
            {
                parameters.SetNamedAutoConstant("textureViewProjMatrix0",
                    GpuProgramParameters.AutoConstantType.TextureViewProjMatrix, 0);
                parameters.SetNamedAutoConstant("textureViewProjMatrix1",
                    GpuProgramParameters.AutoConstantType.TextureViewProjMatrix, 1);
                parameters.SetNamedAutoConstant("textureViewProjMatrix2",
                    GpuProgramParameters.AutoConstantType.TextureViewProjMatrix, 2);
                parameters.SetNamedAutoConstant("lightShadowFarClipDistance",
                    GpuProgramParameters.AutoConstantType.LightShadowFarClipDistance, 0);
                parameters.SetNamedAutoConstant("shadowFarDistance",
                    GpuProgramParameters.AutoConstantType.ShadowFarDistance);
                parameters.SetNamedAutoConstant("shadowColorIntensity",
                    GpuProgramParameters.AutoConstantType.ShadowColorIntensity);
                parameters.SetNamedAutoConstant("shadowTextureSizes",
                    GpuProgramParameters.AutoConstantType.ShadowTextureSizes);
                parameters.SetNamedAutoConstant("shadowDirectionalLightSplitDistances",
                    GpuProgramParameters.AutoConstantType.ShadowDirectionalLightSplitDistances);
                parameters.SetNamedAutoConstant("lightPositionArray",
                    GpuProgramParameters.AutoConstantType.LightPositionArray, lightCount);
                parameters.SetNamedAutoConstant("lightPositionObjectSpaceArray",
                    GpuProgramParameters.AutoConstantType.LightPositionObjectSpaceArray, lightCount);
                parameters.SetNamedAutoConstant("lightDirectionArray",
                    GpuProgramParameters.AutoConstantType.LightDirectionArray, lightCount);
                parameters.SetNamedAutoConstant("lightDirectionObjectSpaceArray",
                    GpuProgramParameters.AutoConstantType.LightDirectionObjectSpaceArray, lightCount);
                parameters.SetNamedAutoConstant("lightAttenuationArray",
                    GpuProgramParameters.AutoConstantType.LightAttenuationArray, lightCount);
                parameters.SetNamedAutoConstant("lightDiffuseColorPowerScaledArray",
                    GpuProgramParameters.AutoConstantType.LightDiffuseColorPowerScaledArray, lightCount);
                parameters.SetNamedAutoConstant("lightSpecularColorPowerScaledArray",
                    GpuProgramParameters.AutoConstantType.LightSpecularColorPowerScaledArray, lightCount);
                parameters.SetNamedAutoConstant("spotLightParamsArray",
                    GpuProgramParameters.AutoConstantType.SpotLightParamsArray, lightCount);
                parameters.SetNamedAutoConstant("lightCastShadowsArray",
                    GpuProgramParameters.AutoConstantType.LightCastShadowsArray, lightCount);
                parameters.SetNamedAutoConstant("lightCustomShaderParameterArray",
                    GpuProgramParameters.AutoConstantType.LightCustomShaderParameterArray, lightCount);
            }

            //Fog
            parameters.SetNamedAutoConstant("fogParams",
                GpuProgramParameters.AutoConstantType.FogParams);
            parameters.SetNamedAutoConstant("fogColor",
                GpuProgramParameters.AutoConstantType.FogColor);

            //Time
            //1 hour interval. for better precision
            parameters.SetNamedAutoConstantFloat("time",
                GpuProgramParameters.AutoConstantType.Time0X, 3600.0f);

            //lightmap
            parameters.SetNamedAutoConstant("lightmapUVTransform",
                GpuProgramParameters.AutoConstantType.LightmapUVTransform);

            //clip planes
            for (int n = 0; n < 6; n++)
            {
                parameters.SetNamedAutoConstant("clipPlane" + n.ToString(),
                    GpuProgramParameters.AutoConstantType.ClipPlane, n);
            }

            parameters.SetNamedAutoConstant("instancing",
                GpuProgramParameters.AutoConstantType.Instancing);
        }

        /*
        protected virtual string OnGetExtensionFileName()
        {
            return null;
        }
        */

        void GenerateUVChannelString(StringBuilder builder, int uvChannel, TransformItem transformItem,
            string transformGpuParameterNamePrefix)
        {
            if (transformItem.IsDataExists())
            {
                builder.AppendFormat(
                    "mul(float2x2({0}Mul.x,{0}Mul.y,{0}Mul.z,{0}Mul.w),uvChannel{1})+{0}Add",
                    transformGpuParameterNamePrefix, uvChannel);
            }
            else
            {
                builder.AppendFormat("uvChannel{0}", uvChannel);
            }
        }

        MapItem GetMapIndex(int index)
        {
            MapItem map = null;

            switch (index)
            {
                case 0: map = textureMap0; break;
                case 1: map = textureMap1; break;
                case 2: map = textureMap2; break;
                case 3: map = textureMap3; break;
                case 4: map = textureMap4; break;
                case 5: map = textureMap5; break;
                case 6: map = textureMap6; break;
                case 7: map = textureMap6; break;
                case 8: map = textureMap6; break;
                case 9: map = textureMap6; break;
            }

            if (map == null)
                Log.Fatal("TemplateMaterial: GetMapIndex: map == null");

            return map;
        }

        protected virtual void OnAddCompileArguments(StringBuilder arguments) { }

        bool CreateDefaultTechnique(out bool shadersIsNotSupported)
        {
            shadersIsNotSupported = false;

            string vertexSyntax;
            string fragmentSyntax;
            int maxSamplerCount = 16;
            int maxTexCoordCount = 8;

            if (RenderSystem.Instance.IsDirect3D())
            {
                vertexSyntax = "vs_2_0";
                fragmentSyntax = "ps_2_0";
            }
            else
            {
                vertexSyntax = "arbvp1";
                fragmentSyntax = "arbfp1";
            }

            //technique is supported?
            {
                if (!GpuProgramManager.Instance.IsSyntaxSupported(fragmentSyntax))
                {
                    defaultTechniqueErrorString = string.Format(
                        "The fragment shaders ({0}) are not supported.", fragmentSyntax);
                    shadersIsNotSupported = true;
                    return false;
                }

                if (!GpuProgramManager.Instance.IsSyntaxSupported(vertexSyntax))
                {
                    defaultTechniqueErrorString = string.Format(
                        "The vertex shaders ({0}) are not supported.", vertexSyntax);
                    shadersIsNotSupported = true;
                    return false;
                }
            }

            //switch to shader model 3. it need for high shadowmaps.
            if (RenderSystem.Instance.HasShaderModel3())
            {
                if (RenderSystem.Instance.IsDirect3D())
                {
                    vertexSyntax = "vs_3_0";
                    fragmentSyntax = "ps_3_0";
                }
                else
                {
                    vertexSyntax = "arbvp1";
                    fragmentSyntax = "arbfp1";
                }
                maxTexCoordCount = 10;
            }

            bool supportAtiHardwareShadows = false;
            bool supportNvidiaHardwareShadows = false;
            {
                if (RenderSystem.Instance.HasShaderModel3() &&
                    TextureManager.Instance.IsFormatSupported(Texture.Type.Type2D,
                    PixelFormat.Depth24, Texture.Usage.RenderTarget))
                {
                    if (RenderSystem.Instance.Capabilities.Vendor == GPUVendors.ATI)
                        supportAtiHardwareShadows = true;
                    if (RenderSystem.Instance.Capabilities.Vendor == GPUVendors.NVidia)
                        supportNvidiaHardwareShadows = true;
                }
            }

            BaseMaterial.ReceiveShadows = receiveShadows;

            //create techniques
            foreach (MaterialSchemes materialScheme in Enum.GetValues(typeof(MaterialSchemes)))
            {
                Technique technique = BaseMaterial.CreateTechnique();
                technique.SchemeName = materialScheme.ToString();

                //mergeAmbientAndDirectionalLightPasses == false
                //pass 0: ambient pass
                //pass 1: directional light
                //pass 2: point light
                //pass 3: spot light

                //mergeAmbientAndDirectionalLightPasses == true
                //pass 0: ambient pass
                //pass 1: ambient pass + first directional light
                //pass 2: directional light (ignore first directional light)
                //pass 3: point light
                //pass 4: spot light
                
                bool mergeAmbientAndDirectionalLightPasses = true;
                if (SceneManager.Instance.IsShadowTechniqueStencilBased())
                    mergeAmbientAndDirectionalLightPasses = false;
                if (skipDirectionalLights)
                    mergeAmbientAndDirectionalLightPasses = false;
                

                int passCount;
                if (lighting)
                    passCount = mergeAmbientAndDirectionalLightPasses ? 5 : 4;
                else
                    passCount = 1;

                for (int nPass = 0; nPass < passCount; nPass++)
                {
                    
                    //skip directional light pass
                    if (skipDirectionalLights)
                        if (nPass == 1) 
                            continue;

                    //create pass
                    Pass pass = technique.CreatePass();
                    Pass shadowCasterPass = null;

                    pass.DepthWrite = depthWrite;
                    pass.DepthCheck = depthTest;

                    bool ambientPass;
                    bool lightPass;

                    RenderLightType lightType = RenderLightType.Directional;

                    if (lighting)
                    {
                        if (mergeAmbientAndDirectionalLightPasses)
                        {
                            ambientPass = nPass <= 1;
                            lightPass = nPass >= 1;

                            switch (nPass)
                            {
                                case 0:
                                    //ambient only. this pass skipped when exists directional light.
                                    pass.SpecialRendering = true;
                                    pass.SpecialRenderingAllowOnlyNotExistsSpecificLights = true;
                                    pass.SpecialRenderingLightType = RenderLightType.Directional;
                                    break;
                                case 1:
                                    //ambient + first directional light
                                    lightType = RenderLightType.Directional;
                                    pass.SpecialRendering = true;
                                    pass.SpecialRenderingAllowOnlyExistsSpecificLights = true;
                                    pass.SpecialRenderingMaxLightCount = 1;
                                    pass.SpecialRenderingLightType = lightType;
                                    break;
                                case 2:
                                    //directional light (ignore first directional light)
                                    lightType = RenderLightType.Directional;
                                    pass.SpecialRendering = true;
                                    pass.SpecialRenderingIteratePerLight = true;
                                    pass.SpecialRenderingSkipLightCount = 1;
                                    pass.SpecialRenderingLightType = lightType;
                                    break;
                                case 3:
                                    //point light
                                    lightType = RenderLightType.Point;
                                    pass.SpecialRendering = true;
                                    pass.SpecialRenderingIteratePerLight = true;
                                    pass.SpecialRenderingLightType = lightType;
                                    break;
                                case 4:
                                    //spot light
                                    lightType = RenderLightType.Spot;
                                    pass.SpecialRendering = true;
                                    pass.SpecialRenderingIteratePerLight = true;
                                    pass.SpecialRenderingLightType = lightType;
                                    break;
                            }
                        }
                        else
                        {
                            ambientPass = nPass == 0;
                            lightPass = nPass != 0;

                            switch (nPass)
                            {
                                case 1: lightType = RenderLightType.Directional; break;
                                case 2: lightType = RenderLightType.Point; break;
                                case 3: lightType = RenderLightType.Spot; break;
                            }

                            if (lightPass)
                            {
                                pass.SpecialRendering = true;
                                pass.SpecialRenderingIteratePerLight = true;
                                pass.SpecialRenderingLightType = lightType;
                            }

                            if (SceneManager.Instance.IsShadowTechniqueStencilBased())
                            {
                                if (ambientPass)
                                    pass.StencilShadowsIlluminationStage = IlluminationStage.Ambient;
                                if (lightPass)
                                    pass.StencilShadowsIlluminationStage = IlluminationStage.PerLight;
                            }
                        }
                    }
                    else
                    {
                        ambientPass = true;
                        lightPass = false;
                    }

                    int lightCount = lightPass ? 1 : 0;

                    //bool needLightmap = lightPass && lightType == RenderLightType.Directional && LightmapTexCoordIndex != -1;
                    bool needLightmap = ambientPass && LightmapTexCoordIndex != -1;

                    //create shadow caster material
                    if (lightPass && SceneManager.Instance.IsShadowTechniqueShadowmapBased())
                    {
                        Material shadowCasterMaterial = MaterialManager.Instance.Create(
                            MaterialManager.Instance.GetUniqueName(BaseMaterial.Name + "_ShadowCaster"));

                        BaseMaterial.SetShadowTextureCasterMaterial(lightType, shadowCasterMaterial);

                        Technique shadowCasterTechnique = shadowCasterMaterial.CreateTechnique();
                        shadowCasterPass = shadowCasterTechnique.CreatePass();
                        shadowCasterPass.SetFogOverride(FogMode.None, new ColorValue(0, 0, 0), 0, 0, 0);
                    }

                    /////////////////////////////////////
                    //configure general pass settings
                    {
                        //disable Direct3D standard fog features
                        pass.SetFogOverride(FogMode.None, new ColorValue(0, 0, 0), 0, 0, 0);

                        //Light pass
                        if (!ambientPass)
                        {
                            pass.DepthWrite = false;
                            pass.SourceBlendFactor = SceneBlendFactor.One;
                            pass.DestBlendFactor = SceneBlendFactor.One;
                        }

                        //Transparent
                        if (blending != MaterialBlendingTypes.Opaque)
                        {
                            pass.DepthWrite = false;

                            switch (blending)
                            {
                                case MaterialBlendingTypes.AlphaAdd:
                                    pass.SourceBlendFactor = SceneBlendFactor.SourceAlpha;
                                    pass.DestBlendFactor = SceneBlendFactor.One;
                                    break;
                                case MaterialBlendingTypes.AlphaBlend:
                                    pass.SourceBlendFactor = SceneBlendFactor.SourceAlpha;
                                    pass.DestBlendFactor = SceneBlendFactor.OneMinusSourceAlpha;
                                    break;
                            }
                        }

                        //AlphaReject
                        pass.AlphaRejectFunction = alphaRejectFunction;
                        pass.AlphaRejectValue = alphaRejectValue;
                        pass.AlphaToCoverage = alphaToCoverage;
                        if (shadowCasterPass != null)
                        {
                            shadowCasterPass.AlphaRejectFunction = alphaRejectFunction;
                            shadowCasterPass.AlphaRejectValue = alphaRejectValue;
                        }

                        //Culling
                        if (doubleSided)
                        {
                            pass.CullingMode = CullingMode.None;
                            //shadow caster material
                            if (shadowCasterPass != null)
                                shadowCasterPass.CullingMode = CullingMode.None;
                        }
                    }

                    /////////////////////////////////////
                    //generate general compile arguments and create texture unit states
                    StringBuilder generalArguments = new StringBuilder(256);
                    int generalSamplerCount = 0;
                    int generalTexCoordCount = 4;
                    {
                        if (RenderSystem.Instance.IsDirect3D())
                            generalArguments.Append(" -DDIRECT3D");
                        if (RenderSystem.Instance.IsOpenGL())
                            generalArguments.Append(" -DOPENGL");

                        if (RenderSystem.Instance.HasShaderModel3())
                            generalArguments.Append(" -DSHADER_MODEL_3");

                        if (lightType == RenderLightType.Directional || lightType == RenderLightType.Spot)
                        {
                            if (supportAtiHardwareShadows)
                                generalArguments.Append(" -DATI_HARDWARE_SHADOWS");
                            if (supportNvidiaHardwareShadows)
                                generalArguments.Append(" -DNVIDIA_HARDWARE_SHADOWS");
                        }

                        if (ambientPass)
                            generalArguments.Append(" -DAMBIENT_PASS");
                        if (lightPass)
                            generalArguments.AppendFormat(" -DLIGHTTYPE_{0}", lightType.ToString().ToUpper());
                        if (lighting)
                            generalArguments.Append(" -DLIGHTING");
                        if (doubleSided)
                            generalArguments.Append(" -DDOUBLESIDED");

                        if (depthOffset != 0)
                            generalArguments.AppendFormat(" -DDEPTH_OFFSET={0}", depthOffset);

                        generalArguments.AppendFormat(" -DLIGHT_COUNT={0}", lightCount);
                        generalArguments.AppendFormat(" -DBLENDING_{0}", blending.ToString().ToUpper());

                        //hardware instancing
                        if (RenderSystem.Instance.HasShaderModel3() && 
                            RenderSystem.Instance.Capabilities.HardwareInstancing &&
                            !disableInstancing)
                        {
                            if (blending == MaterialBlendingTypes.Opaque)
                            {
                                pass.SupportHardwareInstancing = true;

                                generalArguments.Append(" -DINSTANCING");

                                if (shadowCasterPass != null)
                                    shadowCasterPass.SupportHardwareInstancing = true;
                            }
                        }

                        //definitions
                        foreach (Definition definition in definitions)
                        {
                            string argument = " -D" + definition.DefinitionName;

                            Vec4 v = definition.Values;

                            switch (definition.DefinitionType)
                            {
                                case Definition.DefinitionTypes.Label:
                                    generalArguments.Append(argument);
                                    break;

                                case Definition.DefinitionTypes.Half1:
                                    generalArguments.AppendFormat(argument + "=half({0})", v.X);
                                    break;
                                case Definition.DefinitionTypes.Float1:
                                    generalArguments.AppendFormat(argument + "=float({0})", v.X);
                                    break;
                                
                                case Definition.DefinitionTypes.Half2:
                                    generalArguments.AppendFormat(argument + "=half2({0},{1})", v.X, v.Y);
                                    break;
                                case Definition.DefinitionTypes.Float2:
                                    generalArguments.AppendFormat(argument + "=float2({0},{1})", v.X, v.Y);
                                    break;
                                
                                case Definition.DefinitionTypes.Half3:
                                    generalArguments.AppendFormat(argument + "=half3({0},{1},{2})", v.X, v.Y, v.Z);
                                    break;
                                case Definition.DefinitionTypes.Float3:
                                    generalArguments.AppendFormat(argument + "=float3({0},{1},{2})", v.X, v.Y, v.Z);
                                    break;
                                
                                case Definition.DefinitionTypes.Half4:
                                    generalArguments.AppendFormat(argument + "=half4({0},{1},{2},{3})", v.X, v.Y, v.Z, v.W);
                                    break;
                                case Definition.DefinitionTypes.Float4:
                                    generalArguments.AppendFormat(argument + "=float4({0},{1},{2},{3})", v.X, v.Y, v.Z, v.W);
                                    break;
                            }
                        }

                        //Fog
                        FogMode fogMode = SceneManager.Instance.GetFogMode();
                        if (allowFog && fogMode != FogMode.None)
                        {
                            generalArguments.Append(" -DFOG_ENABLED");
                            generalArguments.Append(" -DFOG_" + fogMode.ToString().ToUpper());
                        }

                        //UVChannels
                        {
                            bool uvChannel0 = false;
                            bool uvChannel1 = false;
                            bool uvChannel2 = false;
                            bool uvChannel3 = false;
                            
                            for (int mapIndex = 0; mapIndex <= 7; mapIndex++)
                            {
                                MapItem map = GetMapIndex(mapIndex);

                                if (!string.IsNullOrEmpty(map.TextureFile))
                                {
                                    if ((int)map.UVChannel == 0)
                                        uvChannel0 = true;
                                    else if ((int)map.UVChannel == 1)
                                        uvChannel1 = true;
                                    else if ((int)map.UVChannel == 2)
                                        uvChannel2 = true;
                                    else if ((int)map.UVChannel == 3)
                                        uvChannel3 = true;
                                }
                            }

                            //static lighting
                            if (needLightmap)
                            {
                                if (LightmapTexCoordIndex == 0)
                                    uvChannel0 = true;
                                else if (LightmapTexCoordIndex == 1)
                                    uvChannel1 = true;
                                else if (LightmapTexCoordIndex == 2)
                                    uvChannel2 = true;
                                else if (LightmapTexCoordIndex == 3)
                                    uvChannel3 = true;
                            }

                            if (uvChannel0 || uvChannel1)
                            {
                                generalArguments.Append(" -DUVCHANNEL_01");
                                generalArguments.AppendFormat(" -DUVCHANNEL01_TEXCOORD=TEXCOORD{0}", generalTexCoordCount);
                                generalTexCoordCount++;
                            }

                            if (uvChannel2 || uvChannel3)
                            {
                                generalArguments.Append(" -DUVCHANNEL_23");
                                generalArguments.AppendFormat(" -DUVCHANNEL23_TEXCOORD=TEXCOORD{0}", generalTexCoordCount);
                                generalTexCoordCount++;
                            }

                            if (uvChannel0)
                                generalArguments.Append(" -DUVCHANNEL_0");

                            if (uvChannel1)
                                generalArguments.Append(" -DUVCHANNEL_1");

                            if (uvChannel2)
                                generalArguments.Append(" -DUVCHANNEL_2");
                            
                            if (uvChannel3)
                                generalArguments.Append(" -DUVCHANNEL_3");
                        }

                        //TextureMaps
                        {
                            for (int mapIndex = 0; mapIndex <= 9; mapIndex++)
                            {
                                MapItem map = GetMapIndex(mapIndex);

                                if (!string.IsNullOrEmpty(map.TextureFile))
                                {
                                    generalArguments.AppendFormat(" -DTEXTUREMAP{0}", mapIndex);
                                    generalArguments.AppendFormat(" -DTEXTUREMAP{0}_REGISTER=s{1}", mapIndex, generalSamplerCount);
                                    generalSamplerCount++;

                                    generalArguments.AppendFormat(" -DTEXTUREMAP{0}_TEXCOORD=", mapIndex);
                                    GenerateUVChannelString(generalArguments, (int)map.UVChannel, map.Transform, string.Format("textureMap{0}Transform", mapIndex));

                                    TextureUnitState state = pass.CreateTextureUnitState();
                                    state.SetTextureName(map.TextureFile, map.TextureType);
                                    state.SetTextureAddressingMode(map.AddressingMode);

                                    //shadow caster material
                                    if (shadowCasterPass != null)
                                    {
                                        TextureUnitState casterState = shadowCasterPass.CreateTextureUnitState();
                                        casterState.SetTextureName(map.TextureFile, map.TextureType);
                                        casterState.SetTextureAddressingMode(map.AddressingMode);
                                    }
                                }
                            }
                        }

                        //EnvironmentZones
                        if (useEnvironmentZones)
                        {
                            generalArguments.Append( " -DENVIRONMENT_ZONES");
                            generalArguments.AppendFormat( " -DENVIRONMENT_CUBEMAP_REGISTER=s{0}", generalSamplerCount );
							generalSamplerCount++;

                            TextureUnitState textureState = pass.CreateTextureUnitState();
							textureState.SetTextureAddressingMode( TextureAddressingMode.Clamp );

                            SubscribePassToRenderObjectPassEvent( pass );

                            if( environmentEventUnitStates == null )
							    environmentEventUnitStates = new List<Pair<Pass, TextureUnitState>>();
							
                            environmentEventUnitStates.Add(new Pair<Pass, TextureUnitState>( pass, textureState ) );
                        }

                        //ShadowMapping
                        {
                            if (lightPass)
                            {
                                if (SceneManager.Instance.IsShadowTechniqueShadowmapBased() &&
                                    ReceiveShadows)
                                {
                                    bool pssm = SceneManager.Instance.IsShadowTechniquePSSM() &&
                                        lightType == RenderLightType.Directional;

                                    generalArguments.Append(" -DSHADOW_MAP");

                                    if (RenderSystem.Instance.HasShaderModel3() &&
                                        (SceneManager.Instance.ShadowTechnique == ShadowTechniques.ShadowmapHigh ||
                                        SceneManager.Instance.ShadowTechnique == ShadowTechniques.ShadowmapHighPSSM))
                                    {
                                        generalArguments.Append(" -DSHADOW_MAP_HIGH");
                                    }
                                    else if (RenderSystem.Instance.HasShaderModel3() &&
                                        (SceneManager.Instance.ShadowTechnique == ShadowTechniques.ShadowmapMedium ||
                                        SceneManager.Instance.ShadowTechnique == ShadowTechniques.ShadowmapMediumPSSM))
                                    {
                                        generalArguments.Append(" -DSHADOW_MAP_MEDIUM");
                                    }
                                    else
                                    {
                                        generalArguments.Append(" -DSHADOW_MAP_LOW");
                                    }

                                    if (pssm)
                                        generalArguments.Append(" -DSHADOW_PSSM");

                                    int shadowMapCount = pssm ? 3 : 1;
                                    for (int n = 0; n < shadowMapCount; n++)
                                    {
                                        generalArguments.AppendFormat(" -DSHADOW_MAP{0}_REGISTER=s{1}",
                                            n, generalSamplerCount);
                                        generalSamplerCount++;

                                        generalArguments.AppendFormat(" -DSHADOW_UV{0}_TEXCOORD=TEXCOORD{1}",
                                            n, generalTexCoordCount);
                                        generalTexCoordCount++;

                                        TextureUnitState state = pass.CreateTextureUnitState("");
                                        state.ContentType = TextureUnitState.ContentTypes.Shadow;
                                        state.SetTextureAddressingMode(TextureAddressingMode.Clamp);
                                        state.SetTextureFiltering(FilterOptions.Point,
                                            FilterOptions.Point, FilterOptions.None);

                                        if (lightType == RenderLightType.Directional ||
                                            lightType == RenderLightType.Spot)
                                        {
                                            if (supportAtiHardwareShadows)
                                                state.Fetch4 = true;

                                            if (supportNvidiaHardwareShadows)
                                            {
                                                state.SetTextureFiltering(FilterOptions.Linear,
                                                    FilterOptions.Linear, FilterOptions.None);
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        //Lightmap
                        if (needLightmap)
                        {
                            generalArguments.Append(" -DLIGHTMAP");

                            generalArguments.AppendFormat(" -DLIGHTMAP_REGISTER=s{0}", generalSamplerCount);
                            generalSamplerCount++;

                            if (LightmapTexCoordIndex > 3)
                            {
                                defaultTechniqueErrorString = "LightmapUVChannel > 3 is not supported.";
                                return false;
                            }

                            generalArguments.AppendFormat(" -DLIGHTMAP_TEXCOORD=uvChannel{0}", LightmapTexCoordIndex);

                            TextureUnitState state = pass.CreateTextureUnitState("");
                            state.ContentType = TextureUnitState.ContentTypes.Lightmap;
                        }
                    }

                    //check maximum sampler count
                    if (generalSamplerCount > maxSamplerCount)
                    {
                        defaultTechniqueErrorString = string.Format(
                            "The limit of amount of textures is exceeded. Need: {0}, Maximum: {1}. ({2})",
                            generalSamplerCount, maxSamplerCount, FileName);
                        return false;
                    }

                    //check maximum texture coordinates count
                    if (generalTexCoordCount > maxTexCoordCount)
                    {
                        defaultTechniqueErrorString = string.Format(
                            "The limit of amount of texture coordinates is exceeded. Need: {0}, " +
                            "Maximum: {1}. ({2})", generalTexCoordCount, maxTexCoordCount, FileName);
                        return false;
                    }

                    
                    /////////////////////////////////////
                    //generate replace strings for program compiling
                    List<KeyValuePair<string, string>> replaceStrings = new List<KeyValuePair<string, string>>();
                    /*
                    {
                        //extension file includes
                        string extensionFileName = OnGetExtensionFileName();
                        if (extensionFileName != null)
                        {
                            string directory = "Materials/Common/";
                            string replaceText = string.Format("#include \"{0}{1}\"",
                                directory, extensionFileName);
                            replaceStrings.Add(new KeyValuePair<string, string>(
                                "_INCLUDE_EXTENSION_FILE", replaceText));
                        }
                        else
                        {
                            replaceStrings.Add(new KeyValuePair<string, string>(
                                "_INCLUDE_EXTENSION_FILE", ""));
                        }
                    }
                    */
 
                    OnAddCompileArguments(generalArguments);

                    /////////////////////////////////////
                    //generate programs

                    //get shader file
                    string sourceFile = defaultShader;

                    foreach (SchemeShader schemeShader in materialSchemeShaders)
                        if (!string.IsNullOrEmpty(schemeShader.MaterialSchemeName))
                            if (materialScheme.ToString() == schemeShader.MaterialSchemeName)
                                sourceFile = schemeShader.ShaderFile;

                    //generate program for only ambient pass
                    if (ambientPass && !lightPass)
                    {
                        //vertex program
                        GpuProgram vertexProgram = GpuProgramCacheManager.Instance.AddProgram(
                            "TemplateMaterial_AmbientOnly_Vertex_", GpuProgramType.VertexProgram, sourceFile,
                            "main_vp", vertexSyntax, generalArguments.ToString(), replaceStrings,
                            out defaultTechniqueErrorString);
                        if (vertexProgram == null)
                            return false;

                        OnSetProgramAutoConstants(vertexProgram.DefaultParameters, 0);
                        pass.VertexProgramName = vertexProgram.Name;

                        //fragment program
                        GpuProgram fragmentProgram = GpuProgramCacheManager.Instance.AddProgram(
                            "TemplateMaterial_AmbientOnly_Fragment_", GpuProgramType.FragmentProgram, sourceFile,
                            "main_fp", fragmentSyntax, generalArguments.ToString(), replaceStrings,
                            out defaultTechniqueErrorString);
                        if (fragmentProgram == null)
                            return false;

                        OnSetProgramAutoConstants(fragmentProgram.DefaultParameters, 0);
                        pass.FragmentProgramName = fragmentProgram.Name;
                    }

                    //generate program for light passes
                    if (lightPass)
                    {
                        StringBuilder arguments = new StringBuilder(generalArguments.Length + 100);
                        arguments.Append(generalArguments.ToString());

                        //vertex program
                        GpuProgram vertexProgram = GpuProgramCacheManager.Instance.AddProgram(
                            "TemplateMaterial_Lighted_Vertex_", GpuProgramType.VertexProgram, sourceFile,
                            "main_vp", vertexSyntax, arguments.ToString(), replaceStrings,
                            out defaultTechniqueErrorString);
                        if (vertexProgram == null)
                            return false;

                        OnSetProgramAutoConstants(vertexProgram.DefaultParameters, lightCount);
                        pass.VertexProgramName = vertexProgram.Name;

                        //fragment program
                        GpuProgram fragmentProgram = GpuProgramCacheManager.Instance.AddProgram(
                            "TemplateMaterial_Lighted_Fragment_", GpuProgramType.FragmentProgram, sourceFile,
                            "main_fp", fragmentSyntax, arguments.ToString(), replaceStrings,
                            out defaultTechniqueErrorString);
                        if (fragmentProgram == null)
                            return false;

                        OnSetProgramAutoConstants(fragmentProgram.DefaultParameters, lightCount);
                        pass.FragmentProgramName = fragmentProgram.Name;
                    }

                    //shadow caster material
                    if (shadowCasterPass != null)
                    {
                        StringBuilder arguments = new StringBuilder(generalArguments.Length + 40);
                        arguments.Append(generalArguments.ToString());

                        if (alphaRejectFunction != CompareFunction.AlwaysPass)
                        {
                            arguments.Append(" -DALPHA_REJECT");
                            arguments.AppendFormat(" -DALPHA_REJECT_FUNCTION_{0}", alphaRejectFunction.ToString().ToUpper());
                        }

                        //vertex program
                        GpuProgram vertexProgram = GpuProgramCacheManager.Instance.AddProgram(
                            "TemplateMaterial_Shadowcaster_Vertex_", GpuProgramType.VertexProgram, shadowCasterShader,
                            "shadowCaster_vp", vertexSyntax, arguments.ToString(),
                            replaceStrings, out defaultTechniqueErrorString);
                        if (vertexProgram == null)
                            return false;

                        OnSetProgramAutoConstants(vertexProgram.DefaultParameters, 0);
                        shadowCasterPass.VertexProgramName = vertexProgram.Name;

                        //fragment program
                        GpuProgram fragmentProgram = GpuProgramCacheManager.Instance.AddProgram(
                            "TemplateMaterial_Shadowcaster_Fragment_", GpuProgramType.FragmentProgram, shadowCasterShader,
                            "shadowCaster_fp", fragmentSyntax, arguments.ToString(),
                            replaceStrings, out defaultTechniqueErrorString);
                        if (fragmentProgram == null)
                            return false;

                        OnSetProgramAutoConstants(fragmentProgram.DefaultParameters, 0);
                        shadowCasterPass.FragmentProgramName = fragmentProgram.Name;
                    }
                }
            }

            InitializeAndUpdateDynamicGpuParameters();

            return true;
        }

        void SubscribePassToRenderObjectPassEvent(Pass pass)
        {
            if (subscribedPassesForRenderObjectPass == null)
                subscribedPassesForRenderObjectPass = new List<Pass>();
            if (!subscribedPassesForRenderObjectPass.Contains(pass))
            {
                pass.RenderObjectPass += Pass_RenderObjectPass;
                subscribedPassesForRenderObjectPass.Add(pass);
            }
        }

        void UpdateMapTransformForFixedPipeline(MapItem map)
        {
            List<TextureUnitState> states = map.textureUnitStatesForFixedPipeline;
            if (states == null)
                return;

            foreach (TextureUnitState state in states)
            {
                TransformItem transform = map.Transform;
                AnimationItem animation = transform.Animation;

                state.TextureScroll = transform.Scroll;
                state.TextureRotate = transform.Rotate * (MathFunctions.PI * 2);

                if (transform.Scale != new Vec2(1, 1))
                {
                    Vec2 s = Vec2.Zero;
                    if (transform.Scale.X != 0)
                        s.X = 1.0f / transform.Scale.X;
                    if (transform.Scale.Y != 0)
                        s.Y = 1.0f / transform.Scale.Y;
                    state.TextureScale = s;
                    state.TextureScroll -= (new Vec2(1, 1) - transform.Scale) / 2;
                }

                //property RotateRound is not supported

                state.SetScrollAnimation(-animation.ScrollSpeed);
                state.SetRotateAnimation(-animation.RotateSpeed);
            }
        }

        void FixedPipelineAddTextureMapToPass(Pass pass)
        {
            if (!string.IsNullOrEmpty(textureMap0.TextureFile))
            {
                TextureUnitState state = pass.CreateTextureUnitState(textureMap0.TextureFile, (int)textureMap0.UVChannel);

                state.SetTextureAddressingMode(textureMap0.AddressingMode);

                if (textureMap0.textureUnitStatesForFixedPipeline == null)
                    textureMap0.textureUnitStatesForFixedPipeline = new List<TextureUnitState>();
                textureMap0.textureUnitStatesForFixedPipeline.Add(state);
                UpdateMapTransformForFixedPipeline(textureMap0);

            }
        }

        void CreateFixedPipelineTechnique()
        {
            //ReceiveShadows
            {
                BaseMaterial.ReceiveShadows = receiveShadows;

                //disable receiving shadows when alpha function is enabled
                if (AlphaRejectFunction != CompareFunction.AlwaysPass)
                {
                    if (SceneManager.Instance.IsShadowTechniqueShadowmapBased())
                        BaseMaterial.ReceiveShadows = false;
                }
            }

            Technique tecnhique = BaseMaterial.CreateTechnique();

            if (SceneManager.Instance.IsShadowTechniqueStencilBased())
            {
                //stencil shadows are enabled

                //ambient pass
                if (blending == MaterialBlendingTypes.Opaque)
                {
                    Pass pass = tecnhique.CreatePass();
                    pass.NormalizeNormals = true;

                    pass.Ambient = new ColorValue(1, 1, 1);
                    pass.Diffuse = new ColorValue(0, 0, 0);
                    pass.Specular = new ColorValue(0, 0, 0);

                    pass.AlphaRejectFunction = alphaRejectFunction;
                    pass.AlphaRejectValue = alphaRejectValue;
                    pass.Lighting = lighting;
                    if (doubleSided)
                        pass.CullingMode = CullingMode.None;

                    pass.DepthWrite = depthWrite;
                    pass.DepthCheck = depthTest;

                    if (!allowFog || blending == MaterialBlendingTypes.AlphaAdd)
                        pass.SetFogOverride(FogMode.None, new ColorValue(0, 0, 0), 0, 0, 0);

                    FixedPipelineAddTextureMapToPass(pass);

                    pass.StencilShadowsIlluminationStage = IlluminationStage.Ambient;
                }

                {
                    Pass pass = tecnhique.CreatePass();
                    pass.NormalizeNormals = true;

                    pass.Ambient = new ColorValue(0, 0, 0);
                    pass.Diffuse = new ColorValue(1, 1, 1);
                    pass.AlphaRejectFunction = alphaRejectFunction;
                    pass.AlphaRejectValue = alphaRejectValue;
                    pass.Lighting = lighting;
                    if (doubleSided)
                        pass.CullingMode = CullingMode.None;

                    pass.DepthWrite = false;
                    pass.DepthCheck = depthTest;

                    if (!allowFog || blending == MaterialBlendingTypes.AlphaAdd)
                        pass.SetFogOverride(FogMode.None, new ColorValue(0, 0, 0), 0, 0, 0);

                    pass.SourceBlendFactor = SceneBlendFactor.SourceAlpha;
                    pass.DestBlendFactor = SceneBlendFactor.One;

                    if (blending != MaterialBlendingTypes.Opaque)
                    {
                        switch (blending)
                        {
                            case MaterialBlendingTypes.AlphaAdd:
                                pass.SourceBlendFactor = SceneBlendFactor.SourceAlpha;
                                pass.DestBlendFactor = SceneBlendFactor.One;
                                break;
                            case MaterialBlendingTypes.AlphaBlend:
                                pass.SourceBlendFactor = SceneBlendFactor.SourceAlpha;
                                pass.DestBlendFactor = SceneBlendFactor.OneMinusSourceAlpha;
                                break;
                        }
                    }

                    FixedPipelineAddTextureMapToPass(pass);

                    pass.StencilShadowsIlluminationStage = IlluminationStage.PerLight;
                }

            }
            else
            {
                //stencil shadows are disabled

                Pass pass = tecnhique.CreatePass();
                pass.NormalizeNormals = true;

                pass.Ambient = new ColorValue(1, 1, 1);
                pass.Diffuse = new ColorValue(1, 1, 1);

                pass.AlphaRejectFunction = alphaRejectFunction;
                pass.AlphaRejectValue = alphaRejectValue;
                pass.Lighting = lighting;
                if (doubleSided)
                    pass.CullingMode = CullingMode.None;

                pass.DepthWrite = depthWrite;
                pass.DepthCheck = depthTest;

                if (!allowFog || blending == MaterialBlendingTypes.AlphaAdd)
                    pass.SetFogOverride(FogMode.None, new ColorValue(0, 0, 0), 0, 0, 0);

                if (blending != MaterialBlendingTypes.Opaque)
                {
                    pass.DepthWrite = false;

                    switch (blending)
                    {
                        case MaterialBlendingTypes.AlphaAdd:
                            pass.SourceBlendFactor = SceneBlendFactor.SourceAlpha;
                            pass.DestBlendFactor = SceneBlendFactor.One;
                            break;
                        case MaterialBlendingTypes.AlphaBlend:
                            pass.SourceBlendFactor = SceneBlendFactor.SourceAlpha;
                            pass.DestBlendFactor = SceneBlendFactor.OneMinusSourceAlpha;
                            break;
                    }
                }

                FixedPipelineAddTextureMapToPass(pass);
            }

            fixedPipelineInitialized = true;
        }

        void ClearBaseMaterial()
        {
            if (fixedPipelineInitialized)
            {
                textureMap1.textureUnitStatesForFixedPipeline = null;
                textureMap2.textureUnitStatesForFixedPipeline = null;
                textureMap3.textureUnitStatesForFixedPipeline = null;
                textureMap4.textureUnitStatesForFixedPipeline = null;
                textureMap5.textureUnitStatesForFixedPipeline = null;
                textureMap6.textureUnitStatesForFixedPipeline = null;
            }

            //destroy shadow caster material
            for (int n = 0; n < 3; n++)
            {
                RenderLightType lightType = (RenderLightType)n;

                Material shadowCasterMaterial = BaseMaterial.GetShadowTextureCasterMaterial(lightType);
                if (shadowCasterMaterial != null)
                {
                    BaseMaterial.SetShadowTextureCasterMaterial(lightType, null);
                    shadowCasterMaterial.Dispose();
                }
            }

            if (subscribedPassesForRenderObjectPass != null)
            {
                foreach (Pass pass in subscribedPassesForRenderObjectPass)
                    pass.RenderObjectPass -= Pass_RenderObjectPass;
                subscribedPassesForRenderObjectPass.Clear();
            }

            if (mapsWithAnimations != null)
                mapsWithAnimations.Clear();

            environmentEventUnitStates = null;

            //clear material
            BaseMaterial.RemoveAllTechniques();

            fixedPipelineInitialized = false;
        }

        protected override bool OnInitBaseMaterial()
        {
            if (!base.OnInitBaseMaterial())
                return false;

            defaultTechniqueErrorString = null;

            bool shadersIsNotSupported;
            bool success = CreateDefaultTechnique(out shadersIsNotSupported);

            if (!success)
            {
                //no fatal error if is the Resource Editor
                if (!shadersIsNotSupported &&
                    EngineApp.Instance.ApplicationType != EngineApp.ApplicationTypes.ResourceEditor)
                {
                    if (!string.IsNullOrEmpty(defaultTechniqueErrorString))
                    {
                        Log.Fatal("Cannot create material \"{0}\". {1}", Name,
                            defaultTechniqueErrorString);
                    }
                    return false;
                }

                ClearBaseMaterial();
                CreateFixedPipelineTechnique();
            }

            return true;
        }

        protected override void OnClearBaseMaterial()
        {
            ClearBaseMaterial();
            base.OnClearBaseMaterial();
        }

        protected virtual void InitializeAndUpdateDynamicGpuParameters()
        {
            //initialize and update gpu parameters
            InitializeAndUpdateMapTransformGpuParameters(textureMap1);
            InitializeAndUpdateMapTransformGpuParameters(textureMap2);
            InitializeAndUpdateMapTransformGpuParameters(textureMap3);
            InitializeAndUpdateMapTransformGpuParameters(textureMap4);
            InitializeAndUpdateMapTransformGpuParameters(textureMap5);
            InitializeAndUpdateMapTransformGpuParameters(textureMap6);

            //touch the parameters
            SetCustomGpuParameter(GpuParameters.dynamicColor1, Vec4.Zero);
            SetCustomGpuParameter(GpuParameters.dynamicColor2, Vec4.Zero);
            SetCustomGpuParameter(GpuParameters.dynamicColor3, Vec4.Zero);
            SetCustomGpuParameter(GpuParameters.dynamicColor4, Vec4.Zero);
            SetCustomGpuParameter(GpuParameters.dynamicColor5, Vec4.Zero);
            SetCustomGpuParameter(GpuParameters.dynamicColor6, Vec4.Zero);
            
            //basically there's no difference between color and scalar, but hey easier to keep track of different names
            SetCustomGpuParameter(GpuParameters.dynamicScalars1, Vec4.Zero);
            SetCustomGpuParameter(GpuParameters.dynamicScalars2, Vec4.Zero);
        }

        void SetCustomGpuParameter(GpuParameters parameter, Vec4 value)
        {
            for (int nMaterial = 0; nMaterial < 4; nMaterial++)
            {
                Material material = null;

                switch (nMaterial)
                {
                    case 0:
                        material = BaseMaterial;
                        break;
                    case 1:
                        material = BaseMaterial.GetShadowTextureCasterMaterial(RenderLightType.Point);
                        break;
                    case 2:
                        material = BaseMaterial.GetShadowTextureCasterMaterial(RenderLightType.Directional);
                        break;
                    case 3:
                        material = BaseMaterial.GetShadowTextureCasterMaterial(RenderLightType.Spot);
                        break;
                }

                if (material == null)
                    continue;

                foreach (Technique technique in material.Techniques)
                {
                    foreach (Pass pass in technique.Passes)
                    {
                        if (pass.VertexProgramParameters != null ||
                            pass.FragmentProgramParameters != null)
                        {
                            if (pass.VertexProgramParameters != null)
                            {
                                if (!pass.IsCustomGpuParameterInitialized((int)parameter))
                                {
                                    pass.VertexProgramParameters.SetNamedAutoConstant(parameter.ToString(),
                                        GpuProgramParameters.AutoConstantType.Custom, (int)parameter);
                                }
                            }

                            if (pass.FragmentProgramParameters != null)
                            {
                                if (!pass.IsCustomGpuParameterInitialized((int)parameter))
                                {
                                    pass.FragmentProgramParameters.SetNamedAutoConstant(parameter.ToString(),
                                        GpuProgramParameters.AutoConstantType.Custom, (int)parameter);
                                }
                            }

                            pass.SetCustomGpuParameter((int)parameter, value);
                        }
                    }
                }
            }
        }

        void InitializeAndUpdateMapTransformGpuParameters(MapItem map)
        {
            //subscribe parameters for animation updating via RenderObjectPass event
            if (map.Transform.Animation.IsDataExists())
            {
                //add map to mapsWithAnimations
                if (mapsWithAnimations == null)
                    mapsWithAnimations = new List<MapItem>();
                if (!mapsWithAnimations.Contains(map))
                    mapsWithAnimations.Add(map);

                foreach (Technique technique in BaseMaterial.Techniques)
                    foreach (Pass pass in technique.Passes)
                        SubscribePassToRenderObjectPassEvent(pass);
            }

            //update parameters
            UpdateMapTransformGpuParameters(map);
        }

        void UpdateMapTransformGpuParameters(MapItem map)
        {
            TransformItem transform = map.Transform;

            if (!transform.IsDataExists())
                return;

            //calculate scroll and rotate
            Vec2 scroll = transform.Scroll;
            Vec2 scale = transform.Scale;
            float rotate = transform.Rotate;

            AnimationItem animation = transform.Animation;
            if (animation.IsDataExists())
            {
                float time = RendererWorld.Instance.FrameRenderTime;

                Vec2 animationScroll = animation.ScrollSpeed * time;

                Vec2 round = animation.ScrollRound;
                if (round.X != 0)
                {
                    animationScroll.X =
                        MathFunctions.Round(animationScroll.X * (1.0f / round.X)) * round.X;
                }
                if (round.Y != 0)
                {
                    animationScroll.Y =
                        MathFunctions.Round(animationScroll.Y * (1.0f / round.Y)) * round.Y;
                }

                scroll += animationScroll;
                rotate += animation.RotateSpeed * time;
            }

            scroll.X = scroll.X % 1.0f;
            scroll.Y = scroll.Y % 1.0f;
            rotate = rotate % 1.0f;

            //calculate matrix
            Mat3 matrix;
            {
                if (scale != new Vec2(1, 1))
                    matrix = Mat3.FromScale(new Vec3(scale.X, scale.Y, 1));
                else
                    matrix = Mat3.Identity;

                if (rotate != 0)
                {
                    Mat3 m;
                    m = new Mat3(1, 0, -.5f, 0, 1, -.5f, 0, 0, 1);
                    m *= Mat3.FromRotateByZ(rotate * (MathFunctions.PI * 2));
                    m *= new Mat3(1, 0, .5f, 0, 1, .5f, 0, 0, 1);
                    matrix *= m;
                }

                if (scroll != Vec2.Zero)
                    matrix *= new Mat3(1, 0, scroll.X, 0, 1, scroll.Y, 0, 0, 1);
            }

            //find gpu parameters
            GpuParameters mulGpuParameter;
            GpuParameters addGpuParameter;
            {
                if (map == textureMap1)
                {
                    mulGpuParameter = GpuParameters.textureMap1TransformMul;
                    addGpuParameter = GpuParameters.textureMap1TransformAdd;
                }
                else if (map == textureMap2)
                {
                    mulGpuParameter = GpuParameters.textureMap2TransformMul;
                    addGpuParameter = GpuParameters.textureMap2TransformAdd;
                }
                else if (map == textureMap3)
                {
                    mulGpuParameter = GpuParameters.textureMap3TransformMul;
                    addGpuParameter = GpuParameters.textureMap3TransformAdd;
                }
                else if (map == textureMap4)
                {
                    mulGpuParameter = GpuParameters.textureMap4TransformMul;
                    addGpuParameter = GpuParameters.textureMap4TransformAdd;
                }
                else if (map == textureMap5)
                {
                    mulGpuParameter = GpuParameters.textureMap5TransformMul;
                    addGpuParameter = GpuParameters.textureMap5TransformAdd;
                }
                else if (map == textureMap6)
                {
                    mulGpuParameter = GpuParameters.textureMap6TransformMul;
                    addGpuParameter = GpuParameters.textureMap6TransformAdd;
                }
                else
                {
                    Log.Fatal("ShaderBaseMaterial: Internal error (UpdateMapTransformGpuParameters).");
                    return;
                }
            }

            //set parameters
            SetCustomGpuParameter(mulGpuParameter, new Vec4(matrix.Item0.X, matrix.Item0.Y, matrix.Item1.X, matrix.Item1.Y));
            SetCustomGpuParameter(addGpuParameter, new Vec4(matrix.Item0.Z, matrix.Item1.Z, 0, 0));
        }

        void Pass_RenderObjectPass(Pass pass, Vec3 objectWorldPosition)
        {
            //update cubemap reflection textures
            if (environmentEventUnitStates != null)
            {
                foreach (Pair<Pass, TextureUnitState> item in environmentEventUnitStates)
                    if (item.First == pass)
                        UpdateEnvironmentCubemap(item.Second, objectWorldPosition);
            }

            //update maps transform with animations
            if (mapsWithAnimations != null)
            {
                foreach (MapItem map in mapsWithAnimations)
                    UpdateMapTransformGpuParameters(map);
            }
        }

        void UpdateEnvironmentCubemap(TextureUnitState textureUnitState, Vec3 objectWorldPosition)
        {
            string textureName = "";

            //get cubemap from CubemapZone
            if (Map.Instance != null)
            {
                Map.Instance.GetObjects(new Bounds(objectWorldPosition), delegate(MapObject obj)
                {
                    CubemapZone zone = obj as CubemapZone;
                    if (zone != null && !zone.GlobalZone)
                        textureName = zone.GetTextureName();
                });

                if (string.IsNullOrEmpty(textureName))
                {
                    CubemapZone zone = CubemapZone.GetGlobalZone() as CubemapZone;
                    if (zone != null)
                        textureName = zone.GetTextureName();
                }
            }

            //get cubemap from SkyBox
            if (string.IsNullOrEmpty(textureName))
                textureName = SceneManager.Instance.GetSkyBoxTextureName();

            //update state
            textureUnitState.SetCubicTextureName(textureName, true);
        }

        void SceneManager_FogAndShadowSettingsChanged(bool fogModeChanged, bool shadowTechniqueChanged)
        {
            if (IsBaseMaterialInitialized())
            {
                if ((allowFog && fogModeChanged) || shadowTechniqueChanged)
                    UpdateBaseMaterial();
            }
        }


        public bool IsDefaultTechniqueCreated()
        {
            return string.IsNullOrEmpty(defaultTechniqueErrorString);
        }

        protected override void OnGetEditorShowInformation(out string[] lines, out ColorValue color)
        {
            base.OnGetEditorShowInformation(out lines, out color);

            if (!IsDefaultTechniqueCreated())
            {
                List<string> list = new List<string>();

                list.Add(string.Format("The fixed pipeline is used."));
                list.Add("");
                list.Add("Reason:");

                bool tooManyInstructions = false;

                string[] errorStrings = defaultTechniqueErrorString.Split(new char[] { '\n' },
                    StringSplitOptions.RemoveEmptyEntries);
                foreach (string errorString in errorStrings)
                {
                    //ignore warnings
                    if (errorString.Contains(": warning X"))
                        continue;

                    if (errorString.Contains("Compiled shader code uses too many arithmetic " +
                        "instruction slots"))
                    {
                        tooManyInstructions = true;
                    }

                    string[] strings = StringUtils.TextWordWrap(errorString, 70);
                    foreach (string s in strings)
                        list.Add(s);
                }

                if (EngineApp.Instance.ApplicationType == EngineApp.ApplicationTypes.ResourceEditor &&
                    tooManyInstructions)
                {
                    bool limitToShaderModel2 = false;
                    {
                        TextBlock block = TextBlockUtils.LoadFromVirtualFile(
                            "Definitions/ResourceEditor.config");
                        if (block != null)
                        {
                            if (block.IsAttributeExist("limitToShaderModel2"))
                                limitToShaderModel2 = bool.Parse(block.GetAttribute("limitToShaderModel2"));
                        }
                    }

                    if (limitToShaderModel2)
                    {
                        list.Add("");
                        list.Add("Note: For debugging purposes shader version is limited to 2.");
                        list.Add("You can disable this limitation in the " +
                            "\"Definitions\\ResourceEditor.config\".");
                    }
                }

                lines = list.ToArray();
                color = new ColorValue(1, 0, 0);
            }
        }

        void PreloadTexture(string textureName, Texture.Type textureType)
        {
            Texture texture = TextureManager.Instance.Load(textureName, textureType);
            if (texture != null)
                texture.Touch();
        }

        protected override void OnPreloadResources()
        {
            base.OnPreloadResources();

            if (!string.IsNullOrEmpty(textureMap0.TextureFile))
                PreloadTexture(textureMap0.TextureFile, textureMap0.TextureType);
            if (!string.IsNullOrEmpty(textureMap1.TextureFile))
                PreloadTexture(textureMap1.TextureFile, textureMap1.TextureType);
            if (!string.IsNullOrEmpty(textureMap2.TextureFile))
                PreloadTexture(textureMap2.TextureFile, textureMap2.TextureType);
            if (!string.IsNullOrEmpty(textureMap3.TextureFile))
                PreloadTexture(textureMap3.TextureFile, textureMap3.TextureType);
            if (!string.IsNullOrEmpty(textureMap4.TextureFile))
                PreloadTexture(textureMap4.TextureFile, textureMap4.TextureType);
            if (!string.IsNullOrEmpty(textureMap5.TextureFile))
                PreloadTexture(textureMap5.TextureFile, textureMap5.TextureType);
            if (!string.IsNullOrEmpty(textureMap6.TextureFile))
                PreloadTexture(textureMap6.TextureFile, textureMap6.TextureType);
            if (!string.IsNullOrEmpty(textureMap7.TextureFile))
                PreloadTexture(textureMap7.TextureFile, textureMap7.TextureType);
            if (!string.IsNullOrEmpty(textureMap8.TextureFile))
                PreloadTexture(textureMap8.TextureFile, textureMap8.TextureType);
            if (!string.IsNullOrEmpty(textureMap9.TextureFile))
                PreloadTexture(textureMap9.TextureFile, textureMap9.TextureType);
        }
    }
}
