//
//  data.cpp
//  test_3ds
//
//  Created by Darth Vader on 10-11-22.
//  Copyright (c) 2010 __MyCompanyName__. All rights reserved.
//

#include "data.h"
#include "material.h"
#include "node.h"
#include "render.h"
#include "scene.h"
#include "shader.h"

#include <OpenGLES/ES2/gl.h>
#include <glm/core/func_geometric.hpp>
#include <glm/core/func_matrix.hpp>


namespace kri	{
namespace data	{
	using namespace shade;

	void Model::accept(const Pointer<kri::Node> &pNode)	{
		model = pNode->getWorld();
	}

	void Camera::accept(const Pointer<kri::Camera> &pCam)	{
		const glm::mat4 mw = pCam->pNode->getWorld();
		projection = pCam->getProjection() * glm::inverse(mw);
		pos = mw[3];
	}

	void Material::accept(const Pointer<kri::Material> &pMat)	{
		if(!pMat)	{
			pTexture.reset();
			return;
		}
		emissive	= pMat->emissive;
		diffuse		= pMat->diffuse;
		specular	= pMat->specular;
		glossiness	= pMat->glossiness;
		pTexture	= pMat->pTexture;
	}

	void Light::accept(const Pointer<kri::Light> &pLight)	{
		projection = pLight->getProjection();
	}


	All::All()	{
		const Unidata ud[] =	{
			{ GL_FLOAT_MAT4,	1,	"mx_model",			&mod.model	},
			{ GL_FLOAT_MAT4,	1,	"mx_cam_proj",		&cam.projection	},
			{ GL_FLOAT_VEC4,	1,	"cam_pos",			&cam.pos		},
			{ GL_FLOAT_VEC4,	1,	"surf_emissive",	&mat.emissive	},
			{ GL_FLOAT_VEC4,	1,	"surf_diffuse",		&mat.diffuse	},
			{ GL_FLOAT_VEC4,	1,	"surf_specular",	&mat.specular	},
			{ GL_FLOAT,			1,	"surf_glossiness",	&mat.glossiness	},
			{ GL_FLOAT_VEC4,	1,	"lit0_pos",			&lit[0].pos		},
			{ GL_FLOAT_VEC4,	1,	"lit0_dir",			&lit[0].dir		},
			{ GL_FLOAT_VEC4,	1,	"lit1_pos",			&lit[1].pos		},
			{ GL_FLOAT_VEC4,	1,	"lit1_dir",			&lit[1].dir		},
			{ GL_SAMPLER_2D,	1,	"unit_color",		&mat.pTexture	},
		};
		dict = new Array<Unidata>( ud, sizeof(ud)/sizeof(ud[0]) );
	}

	All::~All()
	{}

	const ArrayRep<shade::Unidata> All::getBook() const	{
		return *dict;
	}

}
}