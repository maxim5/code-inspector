// Implementation of the GLFrame Class
// Richard S. Wright Jr.
// Code by Richard S. Wright Jr.
// Translated from C++ to Java by Martin Ralovski.

package org.elsys.snake3D.framework.util;

import java.nio.FloatBuffer;

import javax.media.opengl.GL;
import javax.media.opengl.glu.GLU;
import javax.vecmath.*;

/*
 * The GLFrame (OrthonormalFrame) class. Possibly the most useful little piece of 3D graphics
 * code for OpenGL immersive environments.
 * Richard S. Wright Jr.
 */
public class GLFrame
{
	private Vector3f origin; // Where am I?
	private Vector3f forward; // Where am I going?
	private Vector3f up; // Which way is up?

	/*
	 * Default position and orientation. At the origin, looking down the
	 * positive Z axis (right handed coordinate system).
	 */
	public GLFrame()
	{
		origin = new Vector3f();
		forward = new Vector3f();
		up = new Vector3f();

		// At origin
		origin.x = 0.0f;
		origin.y = 0.0f;
		origin.z = 0.0f;

		// Up is up (+Y)
		up.x = 0.0f;
		up.y = 1.0f;
		up.z = 0.0f;

		// Forward is -Z (default OpenGL)
		forward.x = 0.0f;
		forward.y = 0.0f;
		forward.z = -1.0f;
	}

	// ///////////////////////////////////////////////////////////
	// Set Location
	public void setOrigin(Vector3f vPoint)
	{
		origin = vPoint;
	}

	public void setOrigin(float x, float y, float z)
	{
		origin.x = x;
		origin.y = y;
		origin.z = z;
	}

	public Vector3f getOrigin()
	{
		return origin;
	}

	public float getOriginX()
	{
		return origin.x;
	}

	public float getOriginY()
	{
		return origin.y;
	}

	public float getOriginZ()
	{
		return origin.z;
	}

	// ///////////////////////////////////////////////////////////
	// Set Forward Direction
	public void setForward(Vector3f point)
	{
		forward = point;
	}

	public void setForward(float x, float y, float z)
	{
		forward.x = x;
		forward.y = y;
		forward.z = z;
	}

	public Vector3f getForward()
	{
		return forward;
	}

	// ///////////////////////////////////////////////////////////
	// Set Up Direction
	public void setUp(Vector3f point)
	{
		up = point;
	}

	public void setUp(float x, float y, float z)
	{
		up.x = x;
		up.y = y;
		up.z = z;
	}

	public Vector3f getUp()
	{
		return up;
	}

	// ///////////////////////////////////////////////////////////
	// Get Axes
	public Vector3f getZAxis()
	{
		return getForward();
	}

	public Vector3f getYAxis()
	{
		return getUp();
	}

	public Vector3f getXAxis()
	{
		Vector3f vCross = new Vector3f();
		vCross.cross(up, forward);
		return vCross;
	}

	// ///////////////////////////////////////////////////////////
	// Translate along orthonormal axis... world or local
	public void translateWorld(float x, float y, float z)
	{
		origin.set(x, y, z);
	}

	public void translateWorld(Vector3f vec)
	{
		origin.set(vec.x, vec.y, vec.z);
	}

	public void translateLocal(float x, float y, float z)
	{
		moveForward(z);
		moveUp(y);
		moveRight(x);
	}

	public void translateLocal(Vector3f vec)
	{
		moveForward(vec.z);
		moveUp(vec.y);
		moveRight(vec.x);
	}

	// ///////////////////////////////////////////////////////////
	// Move Forward (along Z axis)
	public void moveForward(float fDelta)
	{
		// Move along direction of front direction
		origin.x += forward.x * fDelta;
		origin.y += forward.y * fDelta;
		origin.z += forward.z * fDelta;
	}

	// Move along Y axis
	public void moveUp(float fDelta)
	{
		// Move along direction of up direction
		origin.x += up.x * fDelta;
		origin.y += up.y * fDelta;
		origin.z += up.z * fDelta;
	}

	// Move along X axis
	public void moveRight(float fDelta)
	{
		// Move along direction of right vector
		Vector3f vCross = new Vector3f();
		vCross.cross(up, forward);

		origin.x += vCross.x * fDelta;
		origin.y += vCross.y * fDelta;
		origin.z += vCross.z * fDelta;
	}

	// /////////////////////////////////////////////////////////////////////
	// Just assemble the matrix
	public Matrix4f getMatrix()
	{
		return getMatrix(false);
	}

	public Matrix4f getMatrix(boolean bRotationOnly)
	{
		Matrix4f matrix = new Matrix4f();
		// Calculate the right side (x) vector, drop it right into the matrix
		Vector3f vXAxis = new Vector3f();
		vXAxis.cross(up, forward);

		// Set matrix column does not fill in the fourth value...
		matrix.setColumn(0, new Vector4f(vXAxis));
		matrix.m30 = 0.0f;

		// Y Column
		matrix.setColumn(1, new Vector4f(up));
		matrix.m31 = 0.0f;

		// Z Column
		matrix.setColumn(2, new Vector4f(forward));
		matrix.m32 = 0.0f;

		// Translation (already done)
		if (bRotationOnly == true)
		{
			matrix.m03 = 0.0f;
			matrix.m13 = 0.0f;
			matrix.m23 = 0.0f;
		} else
			matrix.setColumn(3, new Vector4f(origin));

		matrix.m33 = 1.0f;

		return matrix;
	}

	// ///////////////////////////////////////////////////////////
	// Get a 4x4 transformation matrix that describes the ccamera
	// orientation.
	public Matrix4f getCameraOrientation()
	{
		Vector3f x, z;
		x = new Vector3f();
		z = new Vector3f();

		// Make rotation matrix
		// Z vector is reversed
		z.sub(forward);

		// X vector = Y cross Z
		x.cross(up, z);

		// Matrix has no translation information and is
		// transposed.... (rows instead of columns)
		Matrix4f m = new Matrix4f();
		m.m00 = x.x;
		m.m01 = x.y;
		m.m02 = x.z;
		m.m03 = 0.0f;
		m.m10 = up.x;
		m.m11 = up.y;
		m.m12 = up.z;
		m.m13 = 0.0f;
		m.m20 = z.x;
		m.m21 = z.y;
		m.m22 = z.z;
		m.m23 = 0.0f;
		m.m30 = 0.0f;
		m.m31 = 0.0f;
		m.m32 = 0.0f;
		m.m33 = 1.0f;

		return m;
	}

	/*
	 * Converts a matrix to a 16x array.
	 */
	private FloatBuffer matrixToFloatBuffer(Matrix4f matrix)
	{
		FloatBuffer buf = FloatBuffer.wrap(new float[] { matrix.m00,
				matrix.m01, matrix.m02, matrix.m03, matrix.m10, matrix.m11,
				matrix.m12, matrix.m13, matrix.m20, matrix.m21, matrix.m22,
				matrix.m23, matrix.m30, matrix.m31, matrix.m32, matrix.m33 });

		return buf;
	}

	// ///////////////////////////////////////////////////////////
	// Perform viewing or modeling transformations
	// Position as the camera (for viewing). Apply this transformation
	// first as your viewing transformation
	// The default implementation of gluLookAt can be considerably sped up
	// since it uses doubles for everything... then again profile before you
	// tune... ;-) You might get a boost form page fault reduction too... if
	// no other glu routines are used...
	// This will get called once per frame....
	public void applyCameraTransform(GLU glu, GL gl)
	{
		applyCameraTransform(glu, gl, false);
	}

	public void applyCameraTransform(GLU glu, GL gl, boolean bRotOnly)
	{
		// Matrix4f m = GetCameraOrientation();
		// Camera Transform
		// gl.glMultMatrixf(MatrixToFloatBuffer(m));

		// If Rotation only, then do not do the translation
		if (!bRotOnly)
			gl.glTranslatef(-origin.x, -origin.y, -origin.z);

		gl.glLoadIdentity();
		glu.gluLookAt(origin.x, origin.y, origin.z, origin.x + forward.x,
				origin.y + forward.y, origin.z + forward.z, up.x, up.y, up.z);
	}

	// Position as an object in the scene. This places and orients a
	// coordinate frame for other objects (besides the camera)
	// There is ample room for optimization here...
	// This is going to be called alot...
	// Add flag to perform actor rotation only and not the translation
	public void applyActorTransform(GL gl)
	{
		applyActorTransform(gl, false);
	}

	public void applyActorTransform(GL gl, boolean bRotationOnly)
	{
		Matrix4f rotMat = getMatrix();

		// Apply rotation to the current matrix
		gl.glMultMatrixf(matrixToFloatBuffer(rotMat));
	}

	// /////////////////////////////////////////////////////////////////////////////
	// Creates a 4x4 rotation matrix, takes radians NOT degrees
	private Matrix4f rotationMatrix44(float angle, float x, float y, float z)
	{
		float mag, s, c, xx, yy, zz, xy, yz, zx, xs, ys, zs, one_c;

		s = (float) Math.sin(angle);
		c = (float) Math.cos(angle);

		mag = (float) Math.sqrt(x * x + y * y + z * z);

		// Identity matrix
		if (mag == 0.0f)
			return new Matrix4f(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);

		// Rotation matrix is normalized
		x /= mag; y /= mag;	z /= mag;

		xx = x * x; yy = y * y;	zz = z * z;
		xy = x * y;	yz = y * z;	zx = z * x;
		xs = x * s;	ys = y * s;	zs = z * s;
		one_c = 1.0f - c;

		Matrix4f m = new Matrix4f();

		m.m00 = (one_c * xx) + c;
		m.m01 = (one_c * xy) - zs;
		m.m02 = (one_c * zx) + ys;
		m.m03 = 0.0f;
		m.m10 = (one_c * xy) + zs;
		m.m11 = (one_c * yy) + c;
		m.m12 = (one_c * yz) - xs;
		m.m13 = 0.0f;
		m.m20 = (one_c * zx) - ys;
		m.m21 = (one_c * yz) + xs;
		m.m22 = (one_c * zz) + c;
		m.m23 = 0.0f;
		m.m30 = 0.0f;
		m.m31 = 0.0f;
		m.m32 = 0.0f;
		m.m33 = 1.0f;

		return m;
	}

	// Rotate around local X Axes - Note all rotations are in radians
	public void rotateLocalX(float fAngle)
	{
		Vector3f cross = new Vector3f();
		cross.cross(up, forward);

		Matrix4f rotMat = rotationMatrix44(fAngle, cross.x, cross.y, cross.z);

		Vector3f newVect = new Vector3f(
		// 3x3 matrix multiply for rotation only
				rotMat.m00 * forward.x + rotMat.m01 * forward.y + rotMat.m02
						* forward.z, rotMat.m10 * forward.x + rotMat.m11
						* forward.y + rotMat.m12 * forward.z, rotMat.m20
						* forward.x + rotMat.m21 * forward.y + rotMat.m22
						* forward.z);
		forward = newVect;

		Vector3f newUp = new Vector3f(
				// Update pointing up vector
				rotMat.m00 * up.x + rotMat.m01 * up.y + rotMat.m02 * up.z,
				rotMat.m10 * up.x + rotMat.m11 * up.y + rotMat.m12 * up.z,
				rotMat.m20 * up.x + rotMat.m21 * up.y + rotMat.m22 * up.z);
		up = newUp;
	}

	// Rotate around local Y
	public void rotateLocalY(float fAngle)
	{
		// Just Rotate around the up vector
		// Create a rotation matrix around my Up (Y) vector
		Matrix4f rotMat = rotationMatrix44(fAngle, up.x, up.y, up.z);

		Vector3f newForward = new Vector3f(
		// Rotate forward pointing vector (3x3 transform)
				rotMat.m00 * forward.x + rotMat.m01 * forward.y + rotMat.m02
						* forward.z, rotMat.m10 * forward.x + rotMat.m11
						* forward.y + rotMat.m12 * forward.z, rotMat.m20
						* forward.x + rotMat.m21 * forward.y + rotMat.m22
						* forward.z);
		forward = newForward;
	}

	// Rotate around local Z
	public void rotateLocalZ(float fAngle)
	{
		// Only the up vector needs to be rotated
		Matrix4f rotMat = rotationMatrix44(fAngle, forward.x, forward.y,
				forward.z);

		Vector3f newUp = new Vector3f(rotMat.m00 * up.x + rotMat.m01 * up.y
				+ rotMat.m02 * up.z, rotMat.m10 * up.x + rotMat.m11 * up.y
				+ rotMat.m12 * up.z, rotMat.m20 * up.x + rotMat.m21 * up.y
				+ rotMat.m22 * up.z);
		up = newUp;
	}

	// Reset axes to make sure they are orthonormal. This should be called on
	// occasion
	// if the matrix is long-lived and frequently transformed.
	public void normalize()
	{
		Vector3f cross = new Vector3f();

		// Calculate cross product of up and forward vectors
		cross.cross(up, forward);

		// Use result to recalculate forward vector
		forward.cross(cross, up);

		// Also check for unit length...
		up.normalize();
		forward.normalize();
	}

	// Rotate in world coordinates...
	public void rotateWorld(float angle, float x, float y, float z)
	{
		// Create the Rotation matrix
		Matrix4f rotMat = rotationMatrix44(angle, x, y, z);

		Vector3f newUp = new Vector3f(
				// Transform the up axis (3x3 rotation)
				rotMat.m00 * up.x + rotMat.m01 * up.y + rotMat.m02 * up.z,
				rotMat.m10 * up.x + rotMat.m11 * up.y + rotMat.m12 * up.z,
				rotMat.m20 * up.x + rotMat.m21 * up.y + rotMat.m22 * up.z);
		up = newUp;

		Vector3f newForward = new Vector3f(
		// Transform the forward axis
				rotMat.m00 * forward.x + rotMat.m01 * forward.y + rotMat.m02
						* forward.z, rotMat.m10 * forward.x + rotMat.m11
						* forward.y + rotMat.m12 * forward.z, rotMat.m20
						* forward.x + rotMat.m21 * forward.y + rotMat.m22
						* forward.z);
		forward = newForward;
	}

	// Rotate around a local axis
	public void rotateLocal(float angle, float x, float y, float z)
	{

		Vector3f localVect = new Vector3f(x, y, z);

		Vector3f worldVect = localToWorld(localVect);
		rotateWorld(angle, worldVect.x, worldVect.y, worldVect.z);
	}

	// Convert Coordinate Systems
	// This is pretty much, do the transformation represented by the rotation
	// and position on the point
	// Is it better to stick to the convention that the destination always comes
	// first, or use the conventions that "sounds" like the function...
	public Vector3f localToWorld(Vector3f localVect)
	{
		// Create the rotation matrix based on the vectors
		Matrix4f rotMat = getMatrix(true);

		Vector3f world = new Vector3f(
				// Do the rotation (remove 4th column...)
				rotMat.m00 * localVect.x + rotMat.m01 * localVect.y
						+ rotMat.m02 * localVect.z, rotMat.m10 * localVect.x
						+ rotMat.m11 * localVect.y + rotMat.m12 * localVect.z,
				rotMat.m20 * localVect.x + rotMat.m21 * localVect.y
						+ rotMat.m22 * localVect.z);
		// Translate the point
		world.add(origin);

		return world;
	}

	// Change world coordinates into "local" coordinates
	public Vector3f worldToLocal(Vector3f vWorld)
	{
		// //////////////////////////////////////////////
		// Translate the origin
		Vector3f vNewWorld = new Vector3f(vWorld.x - origin.x, vWorld.y
				- origin.y, vWorld.z - origin.z);

		// Create the rotation matrix based on the vectors
		Matrix4f rotMat = getMatrix(true);

		// Do the rotation based on inverted matrix
		rotMat.invert();

		return new Vector3f(rotMat.m00 * vNewWorld.x + rotMat.m01 * vNewWorld.y
				+ rotMat.m02 * vNewWorld.z, rotMat.m10 * vNewWorld.x
				+ rotMat.m11 * vNewWorld.y + rotMat.m12 * vNewWorld.z,
				rotMat.m20 * vNewWorld.x + rotMat.m21 * vNewWorld.y
						+ rotMat.m22 * vNewWorld.z);
	}

	// ///////////////////////////////////////////////////////////////////////////
	// Transform a point by frame matrix
	public Vector3f transformPoint(Vector3f vPointSrc)
	{
		Matrix4f m = getMatrix(); // Rotate and translate
		return new Vector3f(m.m00 * vPointSrc.x + m.m01 * vPointSrc.y + m.m02
				* vPointSrc.z + m.m03,// * v[3];
				m.m10 * vPointSrc.x + m.m11 * vPointSrc.y + m.m12 * vPointSrc.z
						+ m.m13,// * v[3];
				m.m20 * vPointSrc.x + m.m21 * vPointSrc.y + m.m22 * vPointSrc.z
						+ m.m23 // * v[3];
		);
	}

	// //////////////////////////////////////////////////////////////////////////
	// Rotate a vector by frame matrix
	public Vector3f RotateVector(Vector3f vVectorSrc)
	{
		Matrix4f m = getMatrix(true); // Rotate only

		return new Vector3f(m.m00 * vVectorSrc.x + m.m01 * vVectorSrc.y + m.m02
				* vVectorSrc.z, m.m10 * vVectorSrc.x + m.m11 * vVectorSrc.y
				+ m.m12 * vVectorSrc.z, m.m20 * vVectorSrc.x + m.m21
				* vVectorSrc.y + m.m22 * vVectorSrc.z);
	}
}
