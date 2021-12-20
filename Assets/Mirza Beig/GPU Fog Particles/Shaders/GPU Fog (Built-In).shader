// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Mirza Beig/GPU Fog (Built-In)"
{
	Properties
	{
		[HDR]_Albedo("Albedo", Color) = (1,1,1,1)
		_SimpleNoiseScale("Simple Noise Scale", Float) = 20
		_SimplexNoiseScale("Simplex Noise Scale", Float) = 4
		_VoronoiScale("Voronoi Scale", Float) = 5
		_SimpleNoiseAnimation("Simple Noise Animation", Vector) = (0,0,0,0)
		_SimplexNoiseAnimation("Simplex Noise Animation", Vector) = (0,0,0.02,0)
		_VoronoiNoiseAnimation("Voronoi Noise Animation", Vector) = (0,0,0,0)
		_SimpleNoiseAmount("Simple Noise Amount", Range( 0 , 1)) = 0.25
		_SimplexNoiseAmount("Simplex Noise Amount", Range( 0 , 1)) = 0.25
		_VoronoiNoiseAmount("Voronoi Noise Amount", Range( 0 , 1)) = 0.5
		_SimpleNoiseRemap("Simple Noise Remap", Range( 0 , 1)) = 0
		_SimplexNoiseRemap("Simplex Noise Remap", Range( 0 , 1)) = 0
		_VoronoiNoiseRemap("Voronoi Noise Remap", Range( 0 , 1)) = 0
		_CombinedNoiseRemap("Combined Noise Remap", Range( 0 , 1)) = 0
		_SurfaceDepthFade("Surface Depth Fade", Float) = 0
		_CameraDepthFadeRange("Camera Depth Fade Range", Float) = 0
		_CameraDepthFadeOffset("Camera Depth Fade Offset", Float) = 0
		[HideInInspector] _texcoord( "", 2D ) = "white" {}
		[HideInInspector] __dirty( "", Int ) = 1
	}

	SubShader
	{
		Tags{ "RenderType" = "Transparent"  "Queue" = "Transparent+0" "IgnoreProjector" = "True" "IsEmissive" = "true"  }
		Cull Back
		CGINCLUDE
		#include "UnityShaderVariables.cginc"
		#include "UnityCG.cginc"
		#include "UnityPBSLighting.cginc"
		#include "Lighting.cginc"
		#pragma target 3.5
		#undef TRANSFORM_TEX
		#define TRANSFORM_TEX(tex,name) float4(tex.xy * name##_ST.xy + name##_ST.zw, tex.z, tex.w)
		struct Input
		{
			float4 vertexColor : COLOR;
			float4 uv_texcoord;
			float4 screenPos;
			float eyeDepth;
		};

		uniform float4 _Albedo;
		uniform float2 _SimpleNoiseAnimation;
		uniform float _SimpleNoiseScale;
		uniform float _SimpleNoiseRemap;
		uniform float _SimpleNoiseAmount;
		uniform float3 _SimplexNoiseAnimation;
		uniform float _SimplexNoiseScale;
		uniform float _SimplexNoiseRemap;
		uniform float _SimplexNoiseAmount;
		uniform float _VoronoiScale;
		uniform float3 _VoronoiNoiseAnimation;
		uniform float _VoronoiNoiseRemap;
		uniform float _VoronoiNoiseAmount;
		uniform float _CombinedNoiseRemap;
		UNITY_DECLARE_DEPTH_TEXTURE( _CameraDepthTexture );
		uniform float4 _CameraDepthTexture_TexelSize;
		uniform float _SurfaceDepthFade;
		uniform float _CameraDepthFadeRange;
		uniform float _CameraDepthFadeOffset;


		inline float noise_randomValue (float2 uv) { return frac(sin(dot(uv, float2(12.9898, 78.233)))*43758.5453); }

		inline float noise_interpolate (float a, float b, float t) { return (1.0-t)*a + (t*b); }

		inline float valueNoise (float2 uv)
		{
			float2 i = floor(uv);
			float2 f = frac( uv );
			f = f* f * (3.0 - 2.0 * f);
			uv = abs( frac(uv) - 0.5);
			float2 c0 = i + float2( 0.0, 0.0 );
			float2 c1 = i + float2( 1.0, 0.0 );
			float2 c2 = i + float2( 0.0, 1.0 );
			float2 c3 = i + float2( 1.0, 1.0 );
			float r0 = noise_randomValue( c0 );
			float r1 = noise_randomValue( c1 );
			float r2 = noise_randomValue( c2 );
			float r3 = noise_randomValue( c3 );
			float bottomOfGrid = noise_interpolate( r0, r1, f.x );
			float topOfGrid = noise_interpolate( r2, r3, f.x );
			float t = noise_interpolate( bottomOfGrid, topOfGrid, f.y );
			return t;
		}


		float SimpleNoise(float2 UV)
		{
			float t = 0.0;
			float freq = pow( 2.0, float( 0 ) );
			float amp = pow( 0.5, float( 3 - 0 ) );
			t += valueNoise( UV/freq )*amp;
			freq = pow(2.0, float(1));
			amp = pow(0.5, float(3-1));
			t += valueNoise( UV/freq )*amp;
			freq = pow(2.0, float(2));
			amp = pow(0.5, float(3-2));
			t += valueNoise( UV/freq )*amp;
			return t;
		}


		float3 mod3D289( float3 x ) { return x - floor( x / 289.0 ) * 289.0; }

		float4 mod3D289( float4 x ) { return x - floor( x / 289.0 ) * 289.0; }

		float4 permute( float4 x ) { return mod3D289( ( x * 34.0 + 1.0 ) * x ); }

		float4 taylorInvSqrt( float4 r ) { return 1.79284291400159 - r * 0.85373472095314; }

		float snoise( float3 v )
		{
			const float2 C = float2( 1.0 / 6.0, 1.0 / 3.0 );
			float3 i = floor( v + dot( v, C.yyy ) );
			float3 x0 = v - i + dot( i, C.xxx );
			float3 g = step( x0.yzx, x0.xyz );
			float3 l = 1.0 - g;
			float3 i1 = min( g.xyz, l.zxy );
			float3 i2 = max( g.xyz, l.zxy );
			float3 x1 = x0 - i1 + C.xxx;
			float3 x2 = x0 - i2 + C.yyy;
			float3 x3 = x0 - 0.5;
			i = mod3D289( i);
			float4 p = permute( permute( permute( i.z + float4( 0.0, i1.z, i2.z, 1.0 ) ) + i.y + float4( 0.0, i1.y, i2.y, 1.0 ) ) + i.x + float4( 0.0, i1.x, i2.x, 1.0 ) );
			float4 j = p - 49.0 * floor( p / 49.0 );  // mod(p,7*7)
			float4 x_ = floor( j / 7.0 );
			float4 y_ = floor( j - 7.0 * x_ );  // mod(j,N)
			float4 x = ( x_ * 2.0 + 0.5 ) / 7.0 - 1.0;
			float4 y = ( y_ * 2.0 + 0.5 ) / 7.0 - 1.0;
			float4 h = 1.0 - abs( x ) - abs( y );
			float4 b0 = float4( x.xy, y.xy );
			float4 b1 = float4( x.zw, y.zw );
			float4 s0 = floor( b0 ) * 2.0 + 1.0;
			float4 s1 = floor( b1 ) * 2.0 + 1.0;
			float4 sh = -step( h, 0.0 );
			float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy;
			float4 a1 = b1.xzyw + s1.xzyw * sh.zzww;
			float3 g0 = float3( a0.xy, h.x );
			float3 g1 = float3( a0.zw, h.y );
			float3 g2 = float3( a1.xy, h.z );
			float3 g3 = float3( a1.zw, h.w );
			float4 norm = taylorInvSqrt( float4( dot( g0, g0 ), dot( g1, g1 ), dot( g2, g2 ), dot( g3, g3 ) ) );
			g0 *= norm.x;
			g1 *= norm.y;
			g2 *= norm.z;
			g3 *= norm.w;
			float4 m = max( 0.6 - float4( dot( x0, x0 ), dot( x1, x1 ), dot( x2, x2 ), dot( x3, x3 ) ), 0.0 );
			m = m* m;
			m = m* m;
			float4 px = float4( dot( x0, g0 ), dot( x1, g1 ), dot( x2, g2 ), dot( x3, g3 ) );
			return 42.0 * dot( m, px);
		}


		float2 voronoihash2( float2 p )
		{
			
			p = float2( dot( p, float2( 127.1, 311.7 ) ), dot( p, float2( 269.5, 183.3 ) ) );
			return frac( sin( p ) *43758.5453);
		}


		float voronoi2( float2 v, float time, inout float2 id, inout float2 mr, float smoothness, inout float2 smoothId )
		{
			float2 n = floor( v );
			float2 f = frac( v );
			float F1 = 8.0;
			float F2 = 8.0; float2 mg = 0;
			for ( int j = -1; j <= 1; j++ )
			{
				for ( int i = -1; i <= 1; i++ )
			 	{
			 		float2 g = float2( i, j );
			 		float2 o = voronoihash2( n + g );
					o = ( sin( time + o * 6.2831 ) * 0.5 + 0.5 ); float2 r = f - g - o;
					float d = 0.5 * dot( r, r );
			 		if( d<F1 ) {
			 			F2 = F1;
			 			F1 = d; mg = g; mr = r; id = o;
			 		} else if( d<F2 ) {
			 			F2 = d;
			
			 		}
			 	}
			}
			return (F2 + F1) * 0.5;
		}


		void vertexDataFunc( inout appdata_full v, out Input o )
		{
			UNITY_INITIALIZE_OUTPUT( Input, o );
			o.eyeDepth = -UnityObjectToViewPos( v.vertex.xyz ).z;
		}

		inline half4 LightingUnlit( SurfaceOutput s, half3 lightDir, half atten )
		{
			return half4 ( 0, 0, 0, s.Alpha );
		}

		void surf( Input i , inout SurfaceOutput o )
		{
			float4 Albedo81 = ( _Albedo * i.vertexColor );
			o.Emission = Albedo81.rgb;
			float ParticleStableRandom43 = i.uv_texcoord.z;
			float2 uvs_TexCoord3 = i.uv_texcoord;
			uvs_TexCoord3.xy = i.uv_texcoord.xy + ( ( _SimpleNoiseAnimation * _Time.y ) + ( ParticleStableRandom43 * 10.0 ) );
			float simpleNoise1 = SimpleNoise( uvs_TexCoord3.xy*_SimpleNoiseScale );
			float SimpleNoise18 = saturate( (0.0 + (simpleNoise1 - _SimpleNoiseRemap) * (1.0 - 0.0) / (1.0 - _SimpleNoiseRemap)) );
			float lerpResult34 = lerp( 1.0 , SimpleNoise18 , _SimpleNoiseAmount);
			float simplePerlin3D19 = snoise( ( float3( i.uv_texcoord.xy ,  0.0 ) + ( _SimplexNoiseAnimation * _Time.y ) + ( ParticleStableRandom43 * 20.0 ) )*_SimplexNoiseScale );
			simplePerlin3D19 = simplePerlin3D19*0.5 + 0.5;
			float SimplexNoise25 = saturate( (0.0 + (simplePerlin3D19 - _SimplexNoiseRemap) * (1.0 - 0.0) / (1.0 - _SimplexNoiseRemap)) );
			float lerpResult33 = lerp( 1.0 , SimplexNoise25 , _SimplexNoiseAmount);
			float mulTime6 = _Time.y * _VoronoiNoiseAnimation.z;
			float time2 = mulTime6;
			float2 voronoiSmoothId2 = 0;
			float2 uvs_TexCoord7 = i.uv_texcoord;
			uvs_TexCoord7.xy = i.uv_texcoord.xy + ( (_VoronoiNoiseAnimation).xy * _Time.y );
			float2 coords2 = uvs_TexCoord7.xy * _VoronoiScale;
			float2 id2 = 0;
			float2 uv2 = 0;
			float voroi2 = voronoi2( coords2, time2, id2, uv2, 0, voronoiSmoothId2 );
			float VoronoiNoise12 = saturate( (0.0 + (voroi2 - _VoronoiNoiseRemap) * (1.0 - 0.0) / (1.0 - _VoronoiNoiseRemap)) );
			float lerpResult5 = lerp( 1.0 , VoronoiNoise12 , _VoronoiNoiseAmount);
			float Noise36 = ( lerpResult34 * lerpResult33 * lerpResult5 );
			float RemappedNoise60 = saturate( (0.0 + (Noise36 - _CombinedNoiseRemap) * (1.0 - 0.0) / (1.0 - _CombinedNoiseRemap)) );
			float4 ase_screenPos = float4( i.screenPos.xyz , i.screenPos.w + 0.00000000001 );
			float4 ase_screenPosNorm = ase_screenPos / ase_screenPos.w;
			ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
			float screenDepth50 = LinearEyeDepth(SAMPLE_DEPTH_TEXTURE( _CameraDepthTexture, ase_screenPosNorm.xy ));
			float distanceDepth50 = abs( ( screenDepth50 - LinearEyeDepth( ase_screenPosNorm.z ) ) / ( _SurfaceDepthFade ) );
			float SurfaceDepthFade55 = saturate( distanceDepth50 );
			float cameraDepthFade87 = (( i.eyeDepth -_ProjectionParams.y - _CameraDepthFadeOffset ) / _CameraDepthFadeRange);
			float CameraDepthFade88 = saturate( cameraDepthFade87 );
			float2 uvs_TexCoord76 = i.uv_texcoord;
			uvs_TexCoord76.xy = i.uv_texcoord.xy * float2( 2,2 ) + float2( -1,-1 );
			float RadialMask79 = saturate( ( 1.0 - length( uvs_TexCoord76.xy ) ) );
			o.Alpha = ( RemappedNoise60 * SurfaceDepthFade55 * CameraDepthFade88 * RadialMask79 * Albedo81.a );
		}

		ENDCG
		CGPROGRAM
		#pragma surface surf Unlit alpha:fade keepalpha fullforwardshadows vertex:vertexDataFunc 

		ENDCG
		Pass
		{
			Name "ShadowCaster"
			Tags{ "LightMode" = "ShadowCaster" }
			ZWrite On
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma target 3.5
			#pragma multi_compile_shadowcaster
			#pragma multi_compile UNITY_PASS_SHADOWCASTER
			#pragma skip_variants FOG_LINEAR FOG_EXP FOG_EXP2
			#include "HLSLSupport.cginc"
			#if ( SHADER_API_D3D11 || SHADER_API_GLCORE || SHADER_API_GLES || SHADER_API_GLES3 || SHADER_API_METAL || SHADER_API_VULKAN )
				#define CAN_SKIP_VPOS
			#endif
			#include "UnityCG.cginc"
			#include "Lighting.cginc"
			#include "UnityPBSLighting.cginc"
			sampler3D _DitherMaskLOD;
			struct v2f
			{
				V2F_SHADOW_CASTER;
				float4 customPack1 : TEXCOORD1;
				float1 customPack2 : TEXCOORD2;
				float3 worldPos : TEXCOORD3;
				float4 screenPos : TEXCOORD4;
				half4 color : COLOR0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};
			v2f vert( appdata_full v )
			{
				v2f o;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_INITIALIZE_OUTPUT( v2f, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				Input customInputData;
				vertexDataFunc( v, customInputData );
				float3 worldPos = mul( unity_ObjectToWorld, v.vertex ).xyz;
				half3 worldNormal = UnityObjectToWorldNormal( v.normal );
				o.customPack1.xyzw = customInputData.uv_texcoord;
				o.customPack1.xyzw = v.texcoord;
				o.customPack2.x = customInputData.eyeDepth;
				o.worldPos = worldPos;
				TRANSFER_SHADOW_CASTER_NORMALOFFSET( o )
				o.screenPos = ComputeScreenPos( o.pos );
				o.color = v.color;
				return o;
			}
			half4 frag( v2f IN
			#if !defined( CAN_SKIP_VPOS )
			, UNITY_VPOS_TYPE vpos : VPOS
			#endif
			) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				Input surfIN;
				UNITY_INITIALIZE_OUTPUT( Input, surfIN );
				surfIN.uv_texcoord = IN.customPack1.xyzw;
				surfIN.eyeDepth = IN.customPack2.x;
				float3 worldPos = IN.worldPos;
				half3 worldViewDir = normalize( UnityWorldSpaceViewDir( worldPos ) );
				surfIN.screenPos = IN.screenPos;
				surfIN.vertexColor = IN.color;
				SurfaceOutput o;
				UNITY_INITIALIZE_OUTPUT( SurfaceOutput, o )
				surf( surfIN, o );
				#if defined( CAN_SKIP_VPOS )
				float2 vpos = IN.pos;
				#endif
				half alphaRef = tex3D( _DitherMaskLOD, float3( vpos.xy * 0.25, o.Alpha * 0.9375 ) ).a;
				clip( alphaRef - 0.01 );
				SHADOW_CASTER_FRAGMENT( IN )
			}
			ENDCG
		}
	}
	Fallback "Diffuse"
	CustomEditor "ASEMaterialInspector"
}
/*ASEBEGIN
Version=18933
0;617.3334;1278;741.3333;2316.604;752.2167;1.767779;True;False
Node;AmplifyShaderEditor.TexCoordVertexDataNode;42;-4773.393,-1670.432;Inherit;False;0;4;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;43;-4506.393,-1595.432;Inherit;False;ParticleStableRandom;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;16;-5721.805,-850.3937;Inherit;False;Property;_SimpleNoiseAnimation;Simple Noise Animation;4;0;Create;True;0;0;0;False;0;False;0,0;-0.01,0.05;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.Vector3Node;63;-5589.325,312.534;Inherit;False;Property;_VoronoiNoiseAnimation;Voronoi Noise Animation;6;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;45;-5647.741,-619.663;Inherit;False;43;ParticleStableRandom;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;15;-5684.653,-711.9423;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;46;-5344.741,-624.663;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;10;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;49;-5663.008,45.80794;Inherit;False;43;ParticleStableRandom;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;14;-5432.046,-807.7477;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Vector3Node;27;-5511.908,-268.972;Inherit;False;Property;_SimplexNoiseAnimation;Simplex Noise Animation;5;0;Create;True;0;0;0;False;0;False;0,0,0.02;0,0.05,0.01;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleTimeNode;10;-5457.396,515.4869;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;64;-5317.099,301.8354;Inherit;False;True;True;False;False;1;0;FLOAT3;0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleTimeNode;22;-5446.602,-83.80537;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;21;-5193.994,-179.6109;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;44;-5147.497,-703.2904;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;9;-5050.049,360.6223;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;20;-5434.677,-463.7364;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;48;-5237.009,-10.32689;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;20;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;3;-4940.935,-765.1147;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleTimeNode;6;-4847.874,430.8412;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;7;-4880.786,302.4518;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;24;-4851.136,-75.68236;Inherit;False;Property;_SimplexNoiseScale;Simplex Noise Scale;2;0;Create;True;0;0;0;False;0;False;4;4;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;11;-4867.758,541.6586;Inherit;False;Property;_VoronoiScale;Voronoi Scale;3;0;Create;True;0;0;0;False;0;False;5;5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;17;-4890.735,-615.4178;Inherit;False;Property;_SimpleNoiseScale;Simple Noise Scale;1;0;Create;True;0;0;0;False;0;False;20;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;26;-4934.829,-192.3494;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;66;-4591.549,-507.3452;Inherit;False;Property;_SimpleNoiseRemap;Simple Noise Remap;10;0;Create;True;0;0;0;False;0;False;0;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;19;-4557.703,-176.7493;Inherit;True;Simplex3D;True;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;15;False;1;FLOAT;0
Node;AmplifyShaderEditor.VoronoiNode;2;-4559.122,342.4288;Inherit;True;0;0;1;3;1;False;1;False;False;False;4;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;5;False;3;FLOAT;0;False;3;FLOAT;0;FLOAT2;1;FLOAT2;2
Node;AmplifyShaderEditor.RangedFloatNode;71;-4643.665,612.6635;Inherit;False;Property;_VoronoiNoiseRemap;Voronoi Noise Remap;12;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;68;-4622.581,87.78754;Inherit;False;Property;_SimplexNoiseRemap;Simplex Noise Remap;11;0;Create;True;0;0;0;False;0;False;0;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;1;-4618.936,-738.1154;Inherit;True;Simple;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;15;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;69;-4202.97,-136.1302;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;65;-4171.941,-731.2629;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;72;-4224.056,344.5222;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;73;-3988.906,349.7338;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;70;-3965.474,-143.9353;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;67;-3936.791,-727.352;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;12;-3782.273,337.2327;Inherit;False;VoronoiNoise;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;25;-3768.302,-147.9679;Inherit;False;SimplexNoise;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;18;-3725.708,-730.9498;Inherit;False;SimpleNoise;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;30;-3151.249,-288.5534;Inherit;False;12;VoronoiNoise;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;35;-3170.335,-611.5272;Inherit;False;Property;_SimpleNoiseAmount;Simple Noise Amount;7;0;Create;True;0;0;0;False;0;False;0.25;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;32;-3175.604,-414.1709;Inherit;False;Property;_SimplexNoiseAmount;Simplex Noise Amount;8;0;Create;True;0;0;0;False;0;False;0.25;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;31;-3162.881,-206.4902;Inherit;False;Property;_VoronoiNoiseAmount;Voronoi Noise Amount;9;0;Create;True;0;0;0;False;0;False;0.5;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;28;-3172.02,-700.3565;Inherit;False;18;SimpleNoise;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;29;-3169.177,-506.701;Inherit;False;25;SimplexNoise;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;33;-2773.697,-493.3024;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;1;False;2;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;34;-2802.572,-704.7106;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;1;False;2;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;5;-2774.64,-288.1064;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;1;False;2;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;4;-2458.382,-590.9659;Inherit;True;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;36;-2188.472,-590.161;Inherit;False;Noise;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;76;-4969.166,910.8445;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;2,2;False;1;FLOAT2;-1,-1;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.VertexColorNode;74;-1656.055,-936.4586;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;59;-3098.238,336.5399;Inherit;False;Property;_CombinedNoiseRemap;Combined Noise Remap;13;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;90;-3117.019,1198.411;Inherit;False;Property;_CameraDepthFadeOffset;Camera Depth Fade Offset;16;0;Create;True;0;0;0;False;0;False;0;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;89;-3116.02,1111.904;Inherit;False;Property;_CameraDepthFadeRange;Camera Depth Fade Range;15;0;Create;True;0;0;0;False;0;False;0;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;52;-3186.774,877.5768;Inherit;False;Property;_SurfaceDepthFade;Surface Depth Fade;14;0;Create;True;0;0;0;False;0;False;0;0.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LengthOpNode;77;-4693.919,917.0438;Inherit;True;1;0;FLOAT2;0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;57;-3037.143,137.7677;Inherit;True;36;Noise;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;39;-1674.146,-1120.435;Inherit;False;Property;_Albedo;Albedo;0;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,1;4.541205,1.902076,0,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CameraDepthFade;87;-2771.673,1093.332;Inherit;False;3;2;FLOAT3;0,0,0;False;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;78;-4486.864,918.2834;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;58;-2746.238,145.5399;Inherit;True;5;0;FLOAT;0;False;1;FLOAT;0.37;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;75;-1372.566,-1016.508;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.DepthFade;50;-2942.532,865.1908;Inherit;False;True;False;True;2;1;FLOAT3;0,0,0;False;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;86;-4288.17,938.4823;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;54;-2639.95,860.5768;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;92;-2497.987,1083.377;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;81;-1201.542,-1000.529;Inherit;False;Albedo;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SaturateNode;61;-2459.379,158.8667;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;83;-1426.194,94.55268;Inherit;False;81;Albedo;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;60;-2305.409,156.9948;Inherit;False;RemappedNoise;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;55;-2451.932,840.6652;Inherit;False;SurfaceDepthFade;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;88;-2321.523,1077.808;Inherit;False;CameraDepthFade;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;79;-4099.953,922.2216;Inherit;False;RadialMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;91;-1305.227,-113.2066;Inherit;False;88;CameraDepthFade;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;80;-1290.623,-6.926636;Inherit;False;79;RadialMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;38;-1286.237,-283.6189;Inherit;False;60;RemappedNoise;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;84;-1186.729,87.57947;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.GetLocalVarNode;56;-1292.892,-204.7867;Inherit;False;55;SurfaceDepthFade;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;82;-1275.562,-519.4245;Inherit;False;81;Albedo;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;53;-933.4528,-282.0087;Inherit;False;5;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StandardSurfaceOutputNode;37;-648.6334,-526.9959;Float;False;True;-1;3;ASEMaterialInspector;0;0;Unlit;Mirza Beig/GPU Fog (Built-In);False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;False;False;False;False;False;False;Back;0;False;-1;0;False;-1;False;0;False;-1;0;False;-1;False;0;Transparent;0.5;True;True;0;False;Transparent;;Transparent;All;18;all;True;True;True;True;0;False;-1;False;0;False;-1;255;False;-1;255;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;False;2;15;10;25;False;0.5;True;2;5;False;-1;10;False;-1;2;5;False;-1;10;False;-1;0;False;-1;0;False;-1;0;False;0;0,0,0,0;VertexOffset;True;False;Cylindrical;False;True;Relative;0;;-1;-1;-1;-1;0;False;0;0;False;-1;-1;0;False;-1;0;0;0;False;0.1;False;-1;0;False;-1;False;15;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT;0;False;4;FLOAT;0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT;0;False;9;FLOAT;0;False;10;FLOAT;0;False;13;FLOAT3;0,0,0;False;11;FLOAT3;0,0,0;False;12;FLOAT3;0,0,0;False;14;FLOAT4;0,0,0,0;False;15;FLOAT3;0,0,0;False;0
WireConnection;43;0;42;3
WireConnection;46;0;45;0
WireConnection;14;0;16;0
WireConnection;14;1;15;0
WireConnection;64;0;63;0
WireConnection;21;0;27;0
WireConnection;21;1;22;0
WireConnection;44;0;14;0
WireConnection;44;1;46;0
WireConnection;9;0;64;0
WireConnection;9;1;10;0
WireConnection;48;0;49;0
WireConnection;3;1;44;0
WireConnection;6;0;63;3
WireConnection;7;1;9;0
WireConnection;26;0;20;0
WireConnection;26;1;21;0
WireConnection;26;2;48;0
WireConnection;19;0;26;0
WireConnection;19;1;24;0
WireConnection;2;0;7;0
WireConnection;2;1;6;0
WireConnection;2;2;11;0
WireConnection;1;0;3;0
WireConnection;1;1;17;0
WireConnection;69;0;19;0
WireConnection;69;1;68;0
WireConnection;65;0;1;0
WireConnection;65;1;66;0
WireConnection;72;0;2;0
WireConnection;72;1;71;0
WireConnection;73;0;72;0
WireConnection;70;0;69;0
WireConnection;67;0;65;0
WireConnection;12;0;73;0
WireConnection;25;0;70;0
WireConnection;18;0;67;0
WireConnection;33;1;29;0
WireConnection;33;2;32;0
WireConnection;34;1;28;0
WireConnection;34;2;35;0
WireConnection;5;1;30;0
WireConnection;5;2;31;0
WireConnection;4;0;34;0
WireConnection;4;1;33;0
WireConnection;4;2;5;0
WireConnection;36;0;4;0
WireConnection;77;0;76;0
WireConnection;87;0;89;0
WireConnection;87;1;90;0
WireConnection;78;0;77;0
WireConnection;58;0;57;0
WireConnection;58;1;59;0
WireConnection;75;0;39;0
WireConnection;75;1;74;0
WireConnection;50;0;52;0
WireConnection;86;0;78;0
WireConnection;54;0;50;0
WireConnection;92;0;87;0
WireConnection;81;0;75;0
WireConnection;61;0;58;0
WireConnection;60;0;61;0
WireConnection;55;0;54;0
WireConnection;88;0;92;0
WireConnection;79;0;86;0
WireConnection;84;0;83;0
WireConnection;53;0;38;0
WireConnection;53;1;56;0
WireConnection;53;2;91;0
WireConnection;53;3;80;0
WireConnection;53;4;84;3
WireConnection;37;2;82;0
WireConnection;37;9;53;0
ASEEND*/
//CHKSM=7E9D6294ECB1A3C9A97A57F39C9D4384B49B013B