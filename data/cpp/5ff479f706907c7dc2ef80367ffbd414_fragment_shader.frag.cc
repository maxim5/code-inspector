// Data

// back and front faces of the cube
uniform sampler2D back, front;

// 2D transfer function
uniform sampler2D transfer_function_2D, transfer_function_from_image;

// volume data and 3D transfer functions
uniform sampler3D volume_texture, transfer_texture, cluster_texture, importance_texture, transfer_texture2, gradient_texture;

// stochastic jittering
uniform sampler2D noise_texture;
uniform int stochastic_jittering; 

// fusion factor between 0 and 1
uniform float fusion_factor;

// for raycasting
uniform float stepsize, luminance, clip;

// for choosing a transfer function
uniform int transfer_function_option;

// enable or disable lighting
uniform int lighting_option;

varying vec4 position; // vertex position, pos = gl_Position;

// for lighting
uniform vec4 fvAmbient;
uniform vec4 fvSpecular;
uniform vec4 fvDiffuse;
uniform float fSpecularPower;

uniform vec3 fvLightPosition;
uniform vec3 fvEyePosition;

// for threshold peeling
uniform int peeling_option;
uniform float threshold_low, threshold_high;

// size of the volume data
uniform vec3 sizes; 

// for cluster peeling
uniform float cluster_interval;
uniform int peeling_layer;

// for feature peeling
uniform float slope_threshold;

// for linear interpolation of alpha in the transfer function
uniform float alpha_opacity;

// for histogram equalization
uniform float scalar_min_normalized, scalar_max_normalized;

// histogram equalization
float scalar_max_min = scalar_max_normalized - scalar_min_normalized;

float truncate(float n)
{
	if(n >= 0)
		return floor(n);
	else
		return ceil(n);
}

// Converts an HSV color to an RGB color.
vec3 convert_hsv_to_rgb(vec3 hsv)
{
	float h = hsv.x, s = hsv.y, v = hsv.z;
	if (h > 360)
	{
		h -= 360;
	}
	float r = 0, g = 0, b = 0;

	if (s == 0)
	{
		r = v;
		g = v;
		b = v;
	}
	else
	{
		int i;
		float f, p, q, t;

		if (h == 360)
			h = 0;
		else
			h = h / 60;

		i = int(truncate(h));
		f = h - i;

		p = v * (1.0 - s);
		q = v * (1.0 - (s * f));
		t = v * (1.0 - (s * (1.0 - f)));

		switch (i)
		{
		case 0:
			r = v;
			g = t;
			b = p;
			break;

		case 1:
			r = q;
			g = v;
			b = p;
			break;

		case 2:
			r = p;
			g = v;
			b = t;
			break;

		case 3:
			r = p;
			g = q;
			b = v;
			break;

		case 4:
			r = t;
			g = p;
			b = v;
			break;

		default:
			r = v;
			g = p;
			b = q;
			break;
		}
	}

	return vec3(r, g, b);
}

vec3 equalize(vec3 v)
{
	return (v - scalar_min_normalized) / scalar_max_min;
}

bool is_in_the_same_cluster(float p1, float p2)
{
	return abs(p1 - p2) / cluster_interval <= 1.0;
}

float sum3(vec3 c)
{
	return c.x + c.y + c.z;
}

float sum3(vec4 c)
{
	return c.x + c.y + c.z;
}

float average(vec4 c)
{
	return (c.x + c.y + c.z) * 0.33333333333333333333333333333333;
}

bool isReacheThreshold(vec4 col_acc, vec4 color_sample)
{
	return average(col_acc) > threshold_high && average(color_sample) > threshold_low;
}

vec4 inc = vec4(vec3(-1, 1, 0)/sizes, 0);

//vec4 mean_low_pass_filter(vec3 p)
//{
//	// neighbors along x, y, z axes
//	return 
//	(texture3D(volume, p)
//	+ texture3D(volume, p + inc.yww) + texture3D(volume, p + inc.xww)
//	+ texture3D(volume, p + inc.wyw) + texture3D(volume, p + inc.wxw)
//	+ texture3D(volume, p + inc.wwy) + texture3D(volume, p + inc.wwx))
//	/ 7;
//}

vec4 median_filter_9(vec4 v1, vec4 v2, vec4 v3, vec4 v4, vec4 v5, vec4 v6, vec4 v7, vec4 v8, vec4 v9)
{
	const int N = 9, N2 = N / 2;
	vec4 data[N], temp;
	data[0] = v1;
	data[1] = v2;
	data[2] = v3;
	data[3] = v4;
	data[4] = v5;
	data[5] = v6;
	data[6] = v7;
	data[7] = v8;
	data[8] = v9;

	// selection sort
	for (int i=0; i<=N2; i++)
	{
		// Select the minimum
		int min = i;
		for (int j=i+1; j<N; j++)
		{
			if (sum3(data[j]) < sum3(data[min]))
			{
				min = j;
			}
		}
		if (min != i)
		{
			temp = data[min];
			data[min] = data[i];
			data[i] = temp;
		}
	}

	return data[N2];
}

// median filter and converter
vec4 median_filter_to_position(vec3 p)
{
	const int N = 7, N2 = N / 2;
	vec4 data[N], temp;
	data[0] = texture3D(volume_texture, p);
	data[1] = texture3D(volume_texture, p + inc.yww);
	data[2] = texture3D(volume_texture, p + inc.xww);
	data[3] = texture3D(volume_texture, p + inc.wyw);
	data[4] = texture3D(volume_texture, p + inc.wxw);
	data[5] = texture3D(volume_texture, p + inc.wwy);
	data[6] = texture3D(volume_texture, p + inc.wwx);

	// selection sort
	for (int i=0; i<=N2; i++)
	{
		// Select the minimum
		int min = i;
		for (int j=i+1; j<N; j++)
		{
			if (data[j].x < data[min].x)
			{
				min = j;
			}
		}
		if (min != i)
		{
			temp = data[min];
			data[min] = data[i];
			data[i] = temp;
		}
	}

	return data[N2];
}

vec3 filter_and_equalize(vec3 p)
{
	return equalize(median_filter_to_position(p).rgb);
}

float get_slope(vec3 v1, vec3 v2)
{
	return sum3(v2) - sum3(v1);
}

//float get_importance_value(vec3 p)
//{
//	return 0;
//}

vec3 diameter = 1.732050 / sizes;
bool detect_boundary_multisample_9(vec3 v1, vec3 p)
{
	const float epsilon = 1e-5;
	vec3 v2 = vec3(0,0,0);
	const int size = 3;
	int count = 0, index1 = -1, index2 = -1;

	// how many non-zero components there are
	for (int i=0; i<size; i++)
	{
		if (abs(v1[i]) > epsilon)
		{
			count++;
			if (index1 == -1)
			{
				index1 = i;
			}else
			{
				index2 = i;
			}
		}
	}

	// get a vector v2 that is vertical to v1
	switch(count)
	{
	case 0: return false;
	case 1:
		index2 = index1 + 1;
		index2 = (index2 >= size) ? (index2 - size) : index2;
		v2[index2] = v1[index1];
		break;
	case 2:
		v2[index2] = -v1[index1];
		v2[index1] = v1[index2];
		break;
	default:
		v2.x = v2.y = v1.z;
		v2.z = - v1.x - v1.y;
	}

	// get a vector v3 that is vertical to both v1 and v2, and then normalize v2 and v3
	// get four positions that are adjacent to the original position
	v2 = normalize(v2);
	vec3 v3 = normalize(cross(v1, v2));
	vec3 delta_v1 = v1 * diameter;
	vec3 delta_v2 = v2 * diameter;
	vec3 delta_v3 = v3 * diameter;

	// horizontal and vertical neighbors
	vec3 p1 = p + delta_v2;
	vec3 p2 = p - delta_v2;
	vec3 p3 = p + delta_v3;
	vec3 p4 = p - delta_v3;

	// diagonal neighbors
	vec3 p5 = p + delta_v2 + delta_v3;
	vec3 p6 = p - delta_v2 - delta_v3;
	vec3 p7 = p + delta_v2 - delta_v3;
	vec3 p8 = p - delta_v2 + delta_v3;

	// how many pairs belong to different clusters
	const float one = 1.0 - epsilon;
	count
		= (is_in_the_same_cluster(texture3D(cluster_texture, p + delta_v1).x, texture3D(cluster_texture, p - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p1 + delta_v1).x, texture3D(cluster_texture, p1 - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p2 + delta_v1).x, texture3D(cluster_texture, p2 - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p3 + delta_v1).x, texture3D(cluster_texture, p3 - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p4 + delta_v1).x, texture3D(cluster_texture, p4 - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p5 + delta_v1).x, texture3D(cluster_texture, p5 - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p6 + delta_v1).x, texture3D(cluster_texture, p6 - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p7 + delta_v1).x, texture3D(cluster_texture, p7 - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p8 + delta_v1).x, texture3D(cluster_texture, p8 - delta_v1).x) ? 1 : 0);

	// It is a boundary if more than a half pairs belong to different clusters
	return count >= 5;
}

bool detect_boundary_multisample_5(vec3 v1, vec3 p)
{
	const float epsilon = 1e-5;
	vec3 v2 = vec3(0,0,0);
	const int size = 3;
	int count = 0, index1 = -1, index2 = -1;

	// how many non-zero components there are
	for (int i=0; i<size; i++)
	{
		if (abs(v1[i]) > epsilon)
		{
			count++;
			if (index1 == -1)
			{
				index1 = i;
			}else
			{
				index2 = i;
			}
		}
	}

	// get a vector v2 that is vertical to v1
	switch(count)
	{
	case 0: return false;
	case 1:
		index2 = index1 + 1;
		index2 = (index2 >= size) ? (index2 - size) : index2;
		v2[index2] = v1[index1];
		break;
	case 2:
		v2[index2] = -v1[index1];
		v2[index1] = v1[index2];
		break;
	default:
		v2.x = v2.y = v1.z;
		v2.z = - v1.x - v1.y;
	}

	// get a vector v3 that is vertical to both v1 and v2, and then normalize v2 and v3
	// get four positions that are adjacent to the original position
	v2 = normalize(v2);
	vec3 v3 = normalize(cross(v1, v2));
	vec3 delta_v1 = v1 * diameter;
	vec3 delta_v2 = v2 * diameter;
	vec3 delta_v3 = v3 * diameter;

	// horizontal and vertical neighbors
	vec3 p1 = p + delta_v2;
	vec3 p2 = p - delta_v2;
	vec3 p3 = p + delta_v3;
	vec3 p4 = p - delta_v3;

	// how many pairs belong to different clusters
	const float one = 1.0 - epsilon;
	count
		= (is_in_the_same_cluster(texture3D(cluster_texture, p + delta_v1).x, texture3D(cluster_texture, p - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p1 + delta_v1).x, texture3D(cluster_texture, p1 - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p2 + delta_v1).x, texture3D(cluster_texture, p2 - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p3 + delta_v1).x, texture3D(cluster_texture, p3 - delta_v1).x) ? 1 : 0)
		+ (is_in_the_same_cluster(texture3D(cluster_texture, p4 + delta_v1).x, texture3D(cluster_texture, p4 - delta_v1).x) ? 1 : 0);

	// It is a boundary if more than a half pairs belong to different clusters
	return count >= 3;
}

vec4 multisample_9(vec3 v1, vec3 p)
{
	const float epsilon = 1e-5;
	vec3 v2 = vec3(0,0,0);
	const int size = 3;
	int count = 0, index1 = -1, index2 = -1;

	// how many non-zero components there are
	for (int i=0; i<size; i++)
	{
		if (abs(v1[i]) > epsilon)
		{
			count++;
			if (index1 == -1)
			{
				index1 = i;
			}else
			{
				index2 = i;
			}
		}
	}

	// get a vector v2 that is vertical to v1
	switch(count)
	{
	case 0: return vec4(0, 0, 0, 0);
	case 1:
		index2 = index1 + 1;
		index2 = (index2 >= size) ? (index2 - size) : index2;
		v2[index2] = v1[index1];
		break;
	case 2:
		v2[index2] = -v1[index1];
		v2[index1] = v1[index2];
		break;
	default:
		v2.x = v2.y = v1.z;
		v2.z = - v1.x - v1.y;
	}

	// get a vector v3 that is vertical to both v1 and v2, and then normalize v2 and v3
	// get four positions that are adjacent to the original position
	v2 = normalize(v2);
	vec3 v3 = normalize(cross(v1, v2));
	//vec3 delta_v1 = v1 * diameter;
	vec3 delta_v2 = v2 * diameter;
	vec3 delta_v3 = v3 * diameter;

	// horizontal and vertical neighbors
	vec3 p1 = p + delta_v2;
	vec3 p2 = p - delta_v2;
	vec3 p3 = p + delta_v3;
	vec3 p4 = p - delta_v3;

	// diagonal neighbors
	vec3 p5 = p + delta_v2 + delta_v3;
	vec3 p6 = p - delta_v2 - delta_v3;
	vec3 p7 = p + delta_v2 - delta_v3;
	vec3 p8 = p - delta_v2 + delta_v3;

	return median_filter_9(
		texture3D(cluster_texture, p),
		texture3D(cluster_texture, p1),
		texture3D(cluster_texture, p2),
		texture3D(cluster_texture, p3),
		texture3D(cluster_texture, p4),
		texture3D(cluster_texture, p5),
		texture3D(cluster_texture, p6),
		texture3D(cluster_texture, p7),
		texture3D(cluster_texture, p8)
		);
}

// pseudo-random number generator
// http://stackoverflow.com/questions/4200224/random-noise-functions-for-glsl
float rand(vec2 co){
	return fract(sin(dot(co.xy, vec2(12.9898, 78.233))) * 43758.5453);
}

// the direct rendering process
vec4 directRendering(vec3 frontPos, vec3 backPos)
{
	float length_acc = 0.;
	vec3 dir = backPos - frontPos;
	float len = length(dir);  // the length from front to back is calculated and used to terminate the ray
	vec3 norm_dir = normalize(dir);
	vec3 delta_dir = norm_dir * stepsize;
	float delta_dir_len = length(delta_dir);
	vec3 ray = frontPos;
	if (stochastic_jittering != 0)
	{
		//ray += delta_dir * texture2D(noise_texture, position.xy).x;
		ray += delta_dir * rand(position.xy);
	}
	//return vec4(vec3(texture2D(noise_texture, position.xy).x),1);
	
	// color sample
	vec4 color_sample;

	// alpha sample
	float alpha_sample;

	// accumulated alpha value
	float alpha_acc = 0.;

	// black or white background
	vec4 col_acc = vec4(0, 0, 0, 0);
	//vec4 col_acc = vec4(1,1,1,1);

	int sample_number = int(len / stepsize);

	// calculate difference to sharpen the image
	const vec4 mask = vec4(1, 0, 0, 0);
	vec4 d = vec4(vec3(1, 1, 1)/sizes, 0), d2 = d * 2.0;
	vec4 e = vec4(vec3(-1, 1, 0)/sizes, 0);
	vec4 c, c2, second_derivative;
	float second_derivative_magnitude;

	// for culstering peeling
	int peeling_counter = 0;

	// for gradient peeling
	vec3 gradient_acc = vec3(0, 0, 0);

	// for feature peeling
	int state = 0;
	float slope;//, peeling_threshold, importance;	
	vec3 local_min, local_max, current_value, next_value;

	// for Sobel operator
	vec4 g_x, g_y, g_z;

	for(int i = 0; i < sample_number; i++)
	{
		// increase the length
		length_acc += delta_dir_len;

		if(length_acc > clip)
		{
			// transfer functions
			switch(transfer_function_option)
			{
			case 0:
				// Raw scalar values without a transfer function
				color_sample = texture3D(volume_texture, ray);
				break;

			case 1:
				// Simple 2D transfer function
				c = texture3D(volume_texture, ray);
				c.rgb = equalize(c.rgb);
				color_sample
					= mask.xxxw * texture2D(transfer_function_2D, c.xw)
					+ mask.wwwx * sum3(c.rgb);
				break;

			case 2:
				// Ben transfer function
				color_sample = texture3D(transfer_texture, ray);
				break;

			case 3:
				// Directional derivatives as RGB
				g_x = abs(texture3D(volume_texture, ray+d.xww)-texture3D(volume_texture, ray-d.xww));
				g_y = abs(texture3D(volume_texture, ray+d.wyw)-texture3D(volume_texture, ray-d.wyw));
				g_z = abs(texture3D(volume_texture, ray+d.wwz)-texture3D(volume_texture, ray-d.wwz));

				color_sample
					= mask.xwww * g_x
					+ mask.wxww * g_y
					+ mask.wwxw * g_z
					+ mask.wwwx * (g_x.x + g_y.y + g_z.z);
				break;

			case 4:
				// second derivatives
				c = texture3D(volume_texture, ray);
				c2 = c * 2.0;

				second_derivative
					= mask.xwww * abs(texture3D(volume_texture, ray+d2.xww) - c2 + texture3D(volume_texture, ray-d2.xww))
					+ mask.wxww * abs(texture3D(volume_texture, ray+d2.wyw) - c2 + texture3D(volume_texture, ray-d2.wyw))
					+ mask.wwxw * abs(texture3D(volume_texture, ray+d2.wwz) - c2 + texture3D(volume_texture, ray-d2.wwz));
				second_derivative_magnitude = max(length(second_derivative), 1e-6);

				// Directional derivatives as RGB
				g_x = abs(texture3D(volume_texture, ray+d.xww)-texture3D(volume_texture, ray-d.xww));
				g_y = abs(texture3D(volume_texture, ray+d.wyw)-texture3D(volume_texture, ray-d.wyw));
				g_z = abs(texture3D(volume_texture, ray+d.wwz)-texture3D(volume_texture, ray-d.wwz));

				color_sample
					= mask.xwww * g_x
					+ mask.wxww * g_y
					+ mask.wwxw * g_z
					+ mask.wwwx * sum3(equalize(c.rgb)) / second_derivative_magnitude;
				break;

			case 5:
				// Sobel operator
				g_x = abs(
					2.0 * (texture3D(volume_texture, ray+e.yww)-texture3D(volume_texture, ray+e.xww))
					+ (texture3D(volume_texture, ray+e.yxw)-texture3D(volume_texture, ray+e.xxw))
					+ (texture3D(volume_texture, ray+e.yyw)-texture3D(volume_texture, ray+e.xyw)) 
					+ (texture3D(volume_texture, ray+e.ywx)-texture3D(volume_texture, ray+e.xwx))
					+ (texture3D(volume_texture, ray+e.ywy)-texture3D(volume_texture, ray+e.xwy))
					);

				g_y = abs(
					2.0 * (texture3D(volume_texture, ray+e.wyw)-texture3D(volume_texture, ray+e.wxw))
					+ (texture3D(volume_texture, ray+e.xyw)-texture3D(volume_texture, ray+e.xxw)) 
					+ (texture3D(volume_texture, ray+e.yyw)-texture3D(volume_texture, ray+e.yxw))
					+ (texture3D(volume_texture, ray+e.wyx)-texture3D(volume_texture, ray+e.wxx))
					+ (texture3D(volume_texture, ray+e.wyy)-texture3D(volume_texture, ray+e.wxy))
					);

				g_z = abs(
					2.0 * (texture3D(volume_texture, ray+e.wwy)-texture3D(volume_texture, ray+e.wwx))
					+ (texture3D(volume_texture, ray+e.xwy)-texture3D(volume_texture, ray+e.xwx)) 
					+ (texture3D(volume_texture, ray+e.ywy)-texture3D(volume_texture, ray+e.ywx))
					+ (texture3D(volume_texture, ray+e.wxy)-texture3D(volume_texture, ray+e.wxx))
					+ (texture3D(volume_texture, ray+e.wyy)-texture3D(volume_texture, ray+e.wyx))
					);

				color_sample
					= mask.xwww * g_x
					+ mask.wxww * g_y
					+ mask.wwxw * g_z
					+ mask.wwwx * (alpha_opacity > 0.0 ? mix((g_x.x + g_y.y + g_z.z), sum3(equalize(texture3D(volume_texture, ray).rgb)), alpha_opacity) : (g_x.x + g_y.y + g_z.z));
				break;

			case 6:
				// Sobel 3D operator
				g_x = abs(
					4.0 * (texture3D(volume_texture, ray+e.yww)-texture3D(volume_texture, ray+e.xww))
					+ 2.0 * (texture3D(volume_texture, ray+e.yxw)-texture3D(volume_texture, ray+e.xxw))
					+ 2.0 * (texture3D(volume_texture, ray+e.yyw)-texture3D(volume_texture, ray+e.xyw)) 
					+ 2.0 * (texture3D(volume_texture, ray+e.ywx)-texture3D(volume_texture, ray+e.xwx))
					+ 2.0 * (texture3D(volume_texture, ray+e.ywy)-texture3D(volume_texture, ray+e.xwy))
					+ (texture3D(volume_texture, ray+e.yxx)-texture3D(volume_texture, ray+e.xxx))
					+ (texture3D(volume_texture, ray+e.yyy)-texture3D(volume_texture, ray+e.xyy)) 
					+ (texture3D(volume_texture, ray+e.yyx)-texture3D(volume_texture, ray+e.xyx))
					+ (texture3D(volume_texture, ray+e.yxy)-texture3D(volume_texture, ray+e.xxy))
					);

				g_y = abs(
					4.0 * (texture3D(volume_texture, ray+e.wyw)-texture3D(volume_texture, ray+e.wxw))
					+ 2.0 * (texture3D(volume_texture, ray+e.xyw)-texture3D(volume_texture, ray+e.xxw)) 
					+ 2.0 * (texture3D(volume_texture, ray+e.yyw)-texture3D(volume_texture, ray+e.yxw))
					+ 2.0 * (texture3D(volume_texture, ray+e.wyx)-texture3D(volume_texture, ray+e.wxx))
					+ 2.0 * (texture3D(volume_texture, ray+e.wyy)-texture3D(volume_texture, ray+e.wxy))
					+ (texture3D(volume_texture, ray+e.xyx)-texture3D(volume_texture, ray+e.xxx)) 
					+ (texture3D(volume_texture, ray+e.yyy)-texture3D(volume_texture, ray+e.yxy))
					+ (texture3D(volume_texture, ray+e.yyx)-texture3D(volume_texture, ray+e.yxx))
					+ (texture3D(volume_texture, ray+e.xyy)-texture3D(volume_texture, ray+e.xxy))
					);

				g_z = abs(
					4.0 * (texture3D(volume_texture, ray+e.wwy)-texture3D(volume_texture, ray+e.wwx))
					+ 2.0 * (texture3D(volume_texture, ray+e.xwy)-texture3D(volume_texture, ray+e.xwx)) 
					+ 2.0 * (texture3D(volume_texture, ray+e.ywy)-texture3D(volume_texture, ray+e.ywx))
					+ 2.0 * (texture3D(volume_texture, ray+e.wxy)-texture3D(volume_texture, ray+e.wxx))
					+ 2.0 * (texture3D(volume_texture, ray+e.wyy)-texture3D(volume_texture, ray+e.wyx))
					+ (texture3D(volume_texture, ray+e.xxy)-texture3D(volume_texture, ray+e.xxx)) 
					+ (texture3D(volume_texture, ray+e.yyy)-texture3D(volume_texture, ray+e.yyx))
					+ (texture3D(volume_texture, ray+e.yxy)-texture3D(volume_texture, ray+e.yxx))
					+ (texture3D(volume_texture, ray+e.xyy)-texture3D(volume_texture, ray+e.xyx))
					);

				color_sample
					= mask.xwww * g_x
					+ mask.wxww * g_y
					+ mask.wwxw * g_z
					+ mask.wwwx * (alpha_opacity > 0.0 ? mix((g_x.x + g_y.y + g_z.z) * 0.125, sum3(equalize(texture3D(volume_texture, ray).rgb)), alpha_opacity) : (g_x.x + g_y.y + g_z.z) * 0.125);
				break;

			case 7:
				// Sobel 3D operator with gradients from gradient_texture
				color_sample = mask.xxxw * texture3D(gradient_texture, ray) + mask.wwwx * sum3(texture3D(volume_texture, ray));
				break;

			case 8:
				// k-means
				color_sample = texture2D(transfer_function_2D, texture3D(cluster_texture, ray).xw);
				break;

			case 9:
				// k-means equalized
				color_sample
					= mask.xxxw * texture2D(transfer_function_2D, texture3D(cluster_texture, ray).xw)
					+ mask.wwwx * sum3(equalize(texture3D(volume_texture, ray).rgb));
				break;

			case 10:
				// Simple 2D transfer function with importance
				c = texture3D(volume_texture, ray);
				c.rgb = equalize(c.rgb);
				color_sample
					= mask.xxxw * texture2D(transfer_function_2D, c.xw)
					+ mask.wwwx * sum3(c.rgb) * texture3D(importance_texture, ray).x;
				break;

			case 11:
				// k-means equalized with importance
				color_sample
					= mask.xxxw * texture2D(transfer_function_2D, texture3D(cluster_texture, ray).xw)
					+ mask.wwwx * sum3(equalize(texture3D(volume_texture, ray).rgb)) * texture3D(importance_texture, ray).x;
				break;

			case 12:
				// Sobel 3D operator with importance
				g_x = abs(
					4.0 * (texture3D(volume_texture, ray+e.yww)-texture3D(volume_texture, ray+e.xww))
					+ 2.0 * (texture3D(volume_texture, ray+e.yxw)-texture3D(volume_texture, ray+e.xxw))
					+ 2.0 * (texture3D(volume_texture, ray+e.yyw)-texture3D(volume_texture, ray+e.xyw)) 
					+ 2.0 * (texture3D(volume_texture, ray+e.ywx)-texture3D(volume_texture, ray+e.xwx))
					+ 2.0 * (texture3D(volume_texture, ray+e.ywy)-texture3D(volume_texture, ray+e.xwy))
					+ (texture3D(volume_texture, ray+e.yxx)-texture3D(volume_texture, ray+e.xxx))
					+ (texture3D(volume_texture, ray+e.yyy)-texture3D(volume_texture, ray+e.xyy)) 
					+ (texture3D(volume_texture, ray+e.yyx)-texture3D(volume_texture, ray+e.xyx))
					+ (texture3D(volume_texture, ray+e.yxy)-texture3D(volume_texture, ray+e.xxy))
					);

				g_y = abs(
					4.0 * (texture3D(volume_texture, ray+e.wyw)-texture3D(volume_texture, ray+e.wxw))
					+ 2.0 * (texture3D(volume_texture, ray+e.xyw)-texture3D(volume_texture, ray+e.xxw)) 
					+ 2.0 * (texture3D(volume_texture, ray+e.yyw)-texture3D(volume_texture, ray+e.yxw))
					+ 2.0 * (texture3D(volume_texture, ray+e.wyx)-texture3D(volume_texture, ray+e.wxx))
					+ 2.0 * (texture3D(volume_texture, ray+e.wyy)-texture3D(volume_texture, ray+e.wxy))
					+ (texture3D(volume_texture, ray+e.xyx)-texture3D(volume_texture, ray+e.xxx)) 
					+ (texture3D(volume_texture, ray+e.yyy)-texture3D(volume_texture, ray+e.yxy))
					+ (texture3D(volume_texture, ray+e.yyx)-texture3D(volume_texture, ray+e.yxx))
					+ (texture3D(volume_texture, ray+e.xyy)-texture3D(volume_texture, ray+e.xxy))
					);

				g_z = abs(
					4.0 * (texture3D(volume_texture, ray+e.wwy)-texture3D(volume_texture, ray+e.wwx))
					+ 2.0 * (texture3D(volume_texture, ray+e.xwy)-texture3D(volume_texture, ray+e.xwx)) 
					+ 2.0 * (texture3D(volume_texture, ray+e.ywy)-texture3D(volume_texture, ray+e.ywx))
					+ 2.0 * (texture3D(volume_texture, ray+e.wxy)-texture3D(volume_texture, ray+e.wxx))
					+ 2.0 * (texture3D(volume_texture, ray+e.wyy)-texture3D(volume_texture, ray+e.wyx))
					+ (texture3D(volume_texture, ray+e.xxy)-texture3D(volume_texture, ray+e.xxx)) 
					+ (texture3D(volume_texture, ray+e.yyy)-texture3D(volume_texture, ray+e.yyx))
					+ (texture3D(volume_texture, ray+e.yxy)-texture3D(volume_texture, ray+e.yxx))
					+ (texture3D(volume_texture, ray+e.xyy)-texture3D(volume_texture, ray+e.xyx))
					);

				color_sample
					= mask.xwww * g_x
					+ mask.wxww * g_y
					+ mask.wwxw * g_z
					+ mask.wwwx * texture3D(importance_texture, ray).x * (alpha_opacity > 0.0 ? mix((g_x.x + g_y.y + g_z.z) * 0.125, sum3(equalize(texture3D(volume_texture, ray).rgb)), alpha_opacity) : (g_x.x + g_y.y + g_z.z) * 0.125);
				break;

			case 13:
				// Fusion of two transfer functions
				color_sample = mix(texture3D(transfer_texture, ray), texture3D(transfer_texture2, ray), fusion_factor);
				break;

			default:
				// Raw scalar values without a transfer function
				color_sample = texture3D(volume_texture, ray);
			}

			/************************************************************************/
			/* lighting                                                             */
			/************************************************************************/
			if (lighting_option == 1)
			{
				vec3 ViewDirection  = fvEyePosition - ray;
				vec3 LightDirection = fvLightPosition - ray;

				vec3  fvLightDirection = normalize( LightDirection );
				vec3  fvNormal         = normalize( texture3D(gradient_texture, ray) ).xyz;
				float fNDotL           = dot( fvNormal, fvLightDirection ); 

				vec3  fvReflection     = normalize( ( ( 2.0 * fvNormal ) * fNDotL ) - fvLightDirection ); 
				vec3  fvViewDirection  = normalize( ViewDirection );
				float fRDotV           = max( 0.0, dot( fvReflection, fvViewDirection ) );

				vec4  fvBaseColor      = color_sample;

				vec4  fvTotalAmbient   = fvAmbient * fvBaseColor; 
				vec4  fvTotalDiffuse   = fvDiffuse * fNDotL * fvBaseColor; 
				vec4  fvTotalSpecular  = fvSpecular * ( pow( fRDotV, fSpecularPower ) );

				color_sample = ( fvTotalAmbient + fvTotalDiffuse + fvTotalSpecular );
			}
			/************************************************************************/



			/************************************************************************/
			// color blending
			// version 1, originate from the GPU raycasting tutorial by Peter Trier jan 2007

			// get the alpha sample value
			alpha_sample = color_sample.a * stepsize;

			// calculate the accumulated color by stepsize
			col_acc += (1.0 - alpha_acc) * color_sample * alpha_sample;

			// calculate the accumulated alpha value
			alpha_acc += alpha_sample;
			/************************************************************************/

			/************************************************************************/
			// version 2, uses mix() in GLSL to interpolate the colors
			// It is just a beta version
			// Accumulate RGB : acc.rgb = voxelColor.rgb*voxelColor.a + (1.0 - voxelColor.a)*acc.rgb;
			//acc.rgb = mix(acc.rgb, voxelColor.rgb, voxelColor.a)*LightIntensity;
			// Accumulate Opacity: acc.a = acc.a + (1.0 - acc.a)*voxelColor.a;
			//acc.a = mix(voxelColor.a, 1.0, acc.a);

			//color_sample.a = color_sample.a * stepsize;
			//col_acc.rgb = mix(col_acc.rgb, color_sample.rgb, color_sample.a);
			//col_acc.a = mix(color_sample.a, 1.0, col_acc.a);
			/************************************************************************/

			/************************************************************************/
			// peeling
			if(peeling_option == 1)
			{
				// opacity peeling
				//if(!threshold_reached && isReacheThreshold(col_acc, color_sample))
				//threshold_reached = true;
				if(average(col_acc) > threshold_high && average(color_sample) < threshold_low)
				{
					if (peeling_counter == peeling_layer)
					{
						break;
					}else
					{
						col_acc = vec4(0,0,0,0);
						peeling_counter++;
					}
				}
			}else
			{
				if(peeling_option == 2)
				{
					// feature peeling
					current_value = filter_and_equalize(ray);
					next_value = filter_and_equalize(ray + delta_dir);
					slope = get_slope(current_value, next_value);

					if (slope > 0.0 && state == 0)
					{
						state = 1;
						local_min = ray;
					}else
					{
						if (slope < 0.0 && state == 1)
						{
							local_max = ray;
							slope = get_slope(filter_and_equalize(local_min), current_value);
							if (slope > slope_threshold)
							{
								//importance = get_importance_value(local_min);
								//if (importance > peeling_threshold)
								//{
								//	if (peeling_counter == peeling_layer)
								//	{
								//		break;
								//	}else
								//	{
								//		peeling_counter++;
								//	}
								//}
								if (peeling_counter == peeling_layer)
								{
									break;
								}else
								{
									peeling_counter++;
								}
							}
							state = 0;
						}
					}
				}else
				{
					if(peeling_option == 3)
					{
						// peel the back
						// classification peeling, peel cluster layers
						if(detect_boundary_multisample_5(norm_dir, ray))
						{
							if (peeling_counter < peeling_layer)
							{
								peeling_counter++;
							}else
							{
								break;
							}
						}
					}else
					{
						if(peeling_option == 4)
						{
							// peel the front
							// classification peeling, peel cluster layers
							if(detect_boundary_multisample_5(norm_dir, ray))
							{
								if (peeling_counter < peeling_layer)
								{
									col_acc = vec4(0,0,0,0);
									peeling_counter++;
								}
							}
						}else
						{
							if (peeling_option == 5)
							{
								// graident peeling
								if (transfer_function_option != 5 && transfer_function_option != 6 && transfer_function_option != 11)
								{
									g_x = 2.0 * abs(texture3D(volume_texture, ray+e.yww)-texture3D(volume_texture, ray+e.xww))
										+ abs(texture3D(volume_texture, ray+e.yxw)-texture3D(volume_texture, ray+e.xxw))
										+ abs(texture3D(volume_texture, ray+e.yyw)-texture3D(volume_texture, ray+e.xyw)) 
										+ abs(texture3D(volume_texture, ray+e.ywx)-texture3D(volume_texture, ray+e.xwx))
										+ abs(texture3D(volume_texture, ray+e.ywy)-texture3D(volume_texture, ray+e.xwy));

									g_y = 2.0 * abs(texture3D(volume_texture, ray+e.wyw)-texture3D(volume_texture, ray+e.wxw))
										+ abs(texture3D(volume_texture, ray+e.xyw)-texture3D(volume_texture, ray+e.xxw)) 
										+ abs(texture3D(volume_texture, ray+e.yyw)-texture3D(volume_texture, ray+e.yxw))
										+ abs(texture3D(volume_texture, ray+e.wyx)-texture3D(volume_texture, ray+e.wxx))
										+ abs(texture3D(volume_texture, ray+e.wyy)-texture3D(volume_texture, ray+e.wxy));

									g_z = 2.0 * abs(texture3D(volume_texture, ray+e.wwy)-texture3D(volume_texture, ray+e.wwx))
										+ abs(texture3D(volume_texture, ray+e.xwy)-texture3D(volume_texture, ray+e.xwx)) 
										+ abs(texture3D(volume_texture, ray+e.ywy)-texture3D(volume_texture, ray+e.ywx))
										+ abs(texture3D(volume_texture, ray+e.wxy)-texture3D(volume_texture, ray+e.wxx))
										+ abs(texture3D(volume_texture, ray+e.wyy)-texture3D(volume_texture, ray+e.wyx));
								}

								vec4 gradient_sample
									= mask.xwww * g_x
									+ mask.wxww * g_y
									+ mask.wwxw * g_z;
								gradient_acc += abs(gradient_sample).xyz;

								// opacity peeling
								//if(!threshold_reached && isReacheThreshold(col_acc, color_sample))
								//threshold_reached = true;
								if(length(gradient_acc) > threshold_high && length(gradient_sample.xyz) < threshold_low)
								{
									if (peeling_counter == peeling_layer)
									{
										break;
									}else
									{
										// clear the accumulated color and gradient
										col_acc = vec4(0,0,0,0);
										gradient_acc = vec3(0,0,0);
										peeling_counter++;
									}
								}
							}else
							{
								if (peeling_option == 6)
								{
									// opacity peeling with importance
									if (texture3D(importance_texture, ray).x > 0.5)
									{
										if(average(col_acc) > threshold_high && average(color_sample) < threshold_low)
										{
											if (peeling_counter == peeling_layer)
											{
												break;
											}else
											{
												col_acc = vec4(0,0,0,0);
												peeling_counter++;
											}
										}
									}
								}else
								{
									if (peeling_option == 7)
									{
										// graident peeling with importance
										if (texture3D(importance_texture, ray).x > 0.5)
										{
											if (transfer_function_option != 5 && transfer_function_option != 6 && transfer_function_option != 11)
											{
												g_x = 2.0 * abs(texture3D(volume_texture, ray+e.yww)-texture3D(volume_texture, ray+e.xww))
													+ abs(texture3D(volume_texture, ray+e.yxw)-texture3D(volume_texture, ray+e.xxw))
													+ abs(texture3D(volume_texture, ray+e.yyw)-texture3D(volume_texture, ray+e.xyw)) 
													+ abs(texture3D(volume_texture, ray+e.ywx)-texture3D(volume_texture, ray+e.xwx))
													+ abs(texture3D(volume_texture, ray+e.ywy)-texture3D(volume_texture, ray+e.xwy));

												g_y = 2.0 * abs(texture3D(volume_texture, ray+e.wyw)-texture3D(volume_texture, ray+e.wxw))
													+ abs(texture3D(volume_texture, ray+e.xyw)-texture3D(volume_texture, ray+e.xxw)) 
													+ abs(texture3D(volume_texture, ray+e.yyw)-texture3D(volume_texture, ray+e.yxw))
													+ abs(texture3D(volume_texture, ray+e.wyx)-texture3D(volume_texture, ray+e.wxx))
													+ abs(texture3D(volume_texture, ray+e.wyy)-texture3D(volume_texture, ray+e.wxy));

												g_z = 2.0 * abs(texture3D(volume_texture, ray+e.wwy)-texture3D(volume_texture, ray+e.wwx))
													+ abs(texture3D(volume_texture, ray+e.xwy)-texture3D(volume_texture, ray+e.xwx)) 
													+ abs(texture3D(volume_texture, ray+e.ywy)-texture3D(volume_texture, ray+e.ywx))
													+ abs(texture3D(volume_texture, ray+e.wxy)-texture3D(volume_texture, ray+e.wxx))
													+ abs(texture3D(volume_texture, ray+e.wyy)-texture3D(volume_texture, ray+e.wyx));
											}

											vec4 gradient_sample
												= mask.xwww * g_x
												+ mask.wxww * g_y
												+ mask.wwxw * g_z;
											gradient_acc += abs(gradient_sample).xyz;

											// opacity peeling
											//if(!threshold_reached && isReacheThreshold(col_acc, color_sample))
											//threshold_reached = true;
											if(length(gradient_acc) > threshold_high && length(gradient_sample.xyz) < threshold_low)
											{
												if (peeling_counter == peeling_layer)
												{
													break;
												}else
												{
													// clear the accumulated color and gradient
													col_acc = vec4(0,0,0,0);
													gradient_acc = vec3(0,0,0);
													peeling_counter++;
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}

		} // if(length_acc > clip)

		// the ray position vector
		ray += delta_dir;

		// terminate if opacity > 1 or the ray is outside the volume
		if(length_acc >= len || col_acc.a >= 1.0)
			break;
	}

	// set the luminance of the ray
	col_acc.rgb *= luminance;

	return col_acc;
}

void main(void)
{
	// find the right place to lookup in the backside buffer
	vec2 tex_coord = ((position.xy / position.w) + 1.) / 2.;

	// the start position of the ray is stored in the texturecoordinate
	vec4 start = gl_TexCoord[1];

	// get the back position from the texture coordinate
	vec4 back_position  = texture2D(back, tex_coord);

	// get the back vector from back position
	vec3 backPos = back_position.xyz;

	// get the front vector from start position
	vec3 frontPos = start.xyz;

	// calculate the direction vector
	vec4 dir = back_position - start;

	//determine whether the ray has to be casted
	if (frontPos == backPos) 
	{
		//background need no raycasting
		discard;
	} else 
	{
		//fragCoords are lying inside the boundingbox
		gl_FragColor = directRendering(frontPos, backPos);
	}
}
