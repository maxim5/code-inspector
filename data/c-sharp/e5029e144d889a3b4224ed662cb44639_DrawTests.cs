using System.Collections.Generic;
using Delta.ContentSystem.Rendering;
using Delta.Engine;
using Delta.Graphics.Basics;
using Delta.InputSystem;
using Delta.Rendering.Basics.Drawing;
using Delta.Rendering.Basics.Fonts;
using Delta.Rendering.Basics.Materials;
using Delta.Rendering.Cameras;
using Delta.Utilities.Datatypes;
using Delta.Utilities.Graphics;
using Delta.Utilities.Helpers;
using NUnit.Framework;

namespace Delta.Rendering.BasicTests
{
	/// <summary>
	/// Basic rendering tests, will mostly just test the Ma manager!
	/// </summary>
	[Category("Visual")]
	public class DrawTests
	{
		#region DrawLine (Static)
		/// <summary>
		/// Very simple draw line unit test, which renders a single line from
		/// the top left to the bottom right at amazing speed (15k+ fps, without
		/// SwapBuffer its 300k+ fps, see OpenTKGraphics.Show).
		/// </summary>
		[Test]
		public static void DrawLine()
		{
			Application.Start(delegate
			{
				// Line from top left up to bottom right in quadratic space
				Line.Draw(new Point(0, 0), new Point(1, 1), Color.Red);
			});
		}
		#endregion

		#region DrawLineAndTexture (Static)
		/// <summary>
		/// Test rendering both a material with the default texture and a line.
		/// </summary>
		[Test]
		public static void DrawLineAndTexture()
		{
			Application.Start(delegate
			{
				Material2D.Default.Draw(new Rectangle(0.2f, 0.2f, 0.6f, 0.6f));
				// Line from top left up to bottom right in quadratic space
				Line.Draw(new Point(0, 0), new Point(1, 1), Color.Red);
			});
		}
		#endregion

		#region AddLinesAndTexturesDynamically (Static)
		/// <summary>
		/// Basically the same test as DrawLineAndTexture, but allows you to add
		/// more stuff by clicking (left = new line, right = new material).
		/// </summary>
		[Test]
		public static void AddLinesAndTexturesDynamically()
		{
			List<Point> materialPositions = new List<Point>();
			materialPositions.Add(Point.Half);
			List<Point> lineEndPoints = new List<Point>();
			lineEndPoints.Add(Point.One);

			Application.Start(delegate
			{
				if (Input.Mouse.LeftButtonReleased)
				{
					lineEndPoints.Add(Input.Mouse.Position);
				}
				if (Input.Mouse.RightButtonReleased)
				{
					materialPositions.Add(Input.Mouse.Position);
				}

				foreach (Point pos in materialPositions)
				{
					Material2D.Default.Draw(Rectangle.FromCenter(pos, 0.01f));
				}
				foreach (Point pos in lineEndPoints)
				{
					Line.Draw(new Point(0, 0), pos, Color.Red);
				}
			});
		}
		#endregion

		#region DrawAlternatingLines (Static)
		/// <summary>
		/// DrawAlternatingLines
		/// </summary>
		[Test]
		public static void DrawAlternatingLines()
		{
			Application.Start(delegate
			{
				// We draw here just a cross over the whole screen
				// Horizontal line in the middle of the screen in quadratic space
				Line.Draw(new Point(0, 0.5f), new Point(1, 0.5f),
					Time.Seconds % 2 == 0
						? Color.Red
						: Color.Green);

				// Vertical line in the middle of the screen in quadratic space
				Line.Draw(new Point(0.5f, 0), new Point(0.5f, 1),
					Time.Seconds % 2 == 0
						? Color.Yellow
						: Color.Black);

				// Line from top left up to bottom right in quadratic space
				Line.Draw(new Point(0, 0), new Point(1, 1),
					Time.Seconds % 2 == 0
						? Color.Red
						: Color.Green);
			});
		}
		#endregion

		#region DrawTransparentLines (Static)
		/// <summary>
		/// Draw transparent lines, this is to test if the alpha component of
		/// line drawing works with all graphic modules (was not working on ES20).
		/// </summary>
		[Test]
		public static void DrawTransparentLines()
		{
			Application.Start(delegate
			{
				// Solid red line at top
				Line.Draw(new Point(0.2f, 0.3f), new Point(0.8f, 0.3f),
					Color.Red);

				// 50% faded red line in middle
				Line.Draw(new Point(0.2f, 0.5f), new Point(0.8f, 0.5f),
					new Color(Color.Red, 0.5f));

				// And finally 25% faded red line at bottom
				Line.Draw(new Point(0.2f, 0.7f), new Point(0.8f, 0.7f),
					new Color(Color.Red, 0.25f));
			});
		}
		#endregion

		#region DrawLotsOfRotatedLines (Static)
		/// <summary>
		/// </summary>
		[Test]
		public static void DrawLotsOfRotatedLines()
		{
			// Draw lots of lines to test performance (1000 is currently the limit,
			// we only can store 2000 vertices in the VertexPool for Draw)!
			// Note: 200 lines (400 vertices) is the sweet spot on my netbook, it
			// can still be rendered at 300 fps (same as with less lines, 1 line has
			// 330 fps), but more lines like 400 will bring it down to 150fps. 
			const int NumberOfLines = 2000; //50;//100;//1000;
			// Obviously we only need to draw 180 degrees of lines because we see
			// both sides, which will fill a full 360 degree circle.
			float rotationStep = 180.0f / NumberOfLines;
			Point center = Point.Half;
			Application.Start(delegate
			{
				for (int num = 0; num < NumberOfLines; num++)
				{
					Point rotatedPos = new Point(0, 0.4f).Rotate(num * rotationStep);
					Color color = num % 3 == 0
					              	? Color.Red
					              	: num % 3 == 1
					              	  	? Color.Orange
					              	  	: Color.Yellow;
					Line.Draw(center - rotatedPos, center + rotatedPos, color);
				}
			});
		}
		#endregion

		#region DrawDynamicLines (Static)
		/// <summary>
		/// Draw dynamic lines
		/// </summary>
		[Test]
		public static void DrawDynamicLines()
		{
			Application.Start(delegate
			{
				float sinValue = MathHelper.Sin(Time.CurrentExactTime * 15f);
				Line.Draw(new Point(0.5f + (sinValue * 0.5f), ScreenSpace.DrawArea.Top),
					new Point(0.5f, ScreenSpace.DrawArea.Bottom), Color.Red);
			});
		}
		#endregion

		#region DrawCircleSimple (Static)
		/// <summary>
		/// Draw circle simple
		/// </summary>
		[Test]
		public static void DrawCircleSimple()
		{
			Point circlePosition = new Point(0.5f, 0.5f);

			Application.Start(delegate
			{
				Circle.DrawOutline(circlePosition, 0.1f, Color.Red);
			});
		}
		#endregion

		#region DrawCircles (Static)
		/// <summary>
		/// Draw circles
		/// </summary>
		[Test]
		public static void DrawCircles()
		{
			Point circlePosition = new Point(0.5f, 0.5f);

			Application.Start(delegate
			{
				// Draw a few circles in the middle of the screen
				Circle.DrawOutline(circlePosition, 0.1f, Color.Red);
				Circle.DrawOutline(circlePosition, 0.25f, Color.Red);
				Circle.DrawOutline(circlePosition, 0.5f, Color.Red);
			});
		}
		#endregion

		#region DrawCircleAtMousePos (Static)
		/// <summary>
		/// Draw circle at mouse position
		/// </summary>
		[Test]
		public static void DrawCircleAtMousePos()
		{
			Application.Start(delegate
			{
				Material2D.Default.Draw(new Rectangle(0.2f, 0.2f, 0.6f, 0.6f));
				Circle.DrawOutline(Input.Mouse.Position, 0.05f, Color.Red);
			});
		}
		#endregion

		#region DrawFilledCircle (Static)
		/// <summary>
		/// DrawFilledCircle
		/// </summary>
		[Test]
		public static void DrawFilledCircle()
		{
			Application.Start(delegate
			{
				// Draw a filled circle in the middle of the screen
				Circle.DrawFilled(Point.Half, 0.25f, Color.Red);
			});
		}
		#endregion

		#region DrawCirclePerformance (Static)
		/// <summary>
		/// Draw circle performance test to make sure it is fast. Performance is
		/// pretty amazing, we can draw 1000 circles with reaching up to 3000 fps!
		/// </summary>
		[Test]
		public static void DrawCirclePerformance()
		{
			Application.Start(delegate
			{
				// Test a case with 10 different circles each being drawn 100 times,
				// which is quite a bit of circles (each can have up to 64 segments).
				// Each of these circles is drawn with different sizes and colors.
				for (int i = 0; i < 10; i++)
				{
					for (int x = 0; x < 10; x++)
					{
						for (int y = 0; y < 10; y++)
						{
							Circle.DrawOutline(
								new Point(0.05f + x / 10.0f, 0.05f + y / 10.0f),
								0.025f + i * 0.008f,
								new Color(i / 10.0f, 1.0f - (i / 10.0f), x / 10.0f));
						}
					} // for
				} // for
			});
		}
		#endregion

		#region DrawRectangle (Static)
		/// <summary>
		/// Draw rectangle
		/// </summary>
		[Test]
		public static void DrawRectangle()
		{
			Application.Start(delegate
			{
				Rect.DrawOutline(new Rectangle(0.5f, 0.5f, 0.25f, 0.25f),
					Color.Red);

				Rect.DrawOutline(new Rectangle(0.5f, 0.5f, 0.25f, 0.25f), Color.Red,
					45f);

				// Draw a rectangle located in the middle of the screen
				Rect.DrawOutline(new Rectangle(0.5f, 0.5f, 0.15f, 0.1f),
					Color.Red);
				// And show the whole screen area in green
				//This is 0, 800 for 800x600, but we need 0, 799: ScreenSpace.Area
				Rectangle pixelScreenRect = new Rectangle(0, 0,
					Application.Window.ViewportPixelWidth - 1,
					Application.Window.ViewportPixelHeight - 1);
				Rect.DrawOutline(ScreenSpace.ToQuadraticSpace(pixelScreenRect),
					Color.Green);
			});
		}
		#endregion

		#region DrawRectangleAutoRotated (Static)
		/// <summary>
		/// Draw rectangle auto rotated
		/// </summary>
		[Test]
		public static void DrawRectangleAutoRotated()
		{
			float rotation = 0.0f;

			Application.Start(delegate
			{
				rotation += Time.Delta * 10.0f;

				Rect.DrawOutline(Rectangle.FromCenter(0.5f, 0.5f, 0.5f, 0.25f),
					Color.Red, rotation);
			});
		}
		#endregion

		#region DrawFilledRectangle (Static)
		/// <summary>
		/// Draw filled rectangle
		/// </summary>
		[Test]
		public static void DrawFilledRectangle()
		{
			Application.Start(delegate
			{
				Rect.DrawFilled(new Rectangle(0.5f, 0.5f, 0.25f, 0.25f),
					Color.Red);

				Rect.DrawFilled(new Rectangle(0.25f, 0.35f, 0.2f, 0.2f),
					Color.Green, 45);
			});
		}
		#endregion

		#region DrawLines3D (Static)
		/// <summary>
		/// Draw lines 3d
		/// </summary>
		[Test]
		public static void DrawLines3D()
		{
			BaseCamera cam = new LookAtCamera(new Vector(4, -4, 4));

			Application.Start(delegate
			{
				// Draw a simple 3d axis for basic line testing.
				Line.Draw(Vector.Zero, Vector.UnitX, Color.Red);
				Line.Draw(Vector.Zero, Vector.UnitY, Color.Blue);
				Line.Draw(Vector.Zero, Vector.UnitZ, Color.Green);
			});
		}
		#endregion

		#region DrawBox3D (Static)
		/// <summary>
		/// Draw box 3D
		/// </summary>
		[Test]
		public static void DrawBox3D()
		{
			BaseCamera cam = new LookAtCamera(new Vector(10, 10, 10));
			Vector halfSize = new Vector(3, 3, 3);

			Application.Start(delegate
			{
				Grid.Draw();

				// draw a simple cube from (-3, -3, -3) to (3, 3, 3)
				Box.DrawOutline(-halfSize, halfSize, Color.Red);
			});
		}
		#endregion

		#region GetRayFromScreenPoint (Static)
		/// <summary>
		/// This tests ScreenSpace.GetRayFromScreenPoint. It is here because in
		/// Display or Delta.Engine.Tests there is no 3D visualization.
		/// Its easier to test here.
		/// </summary>
		[Test]
		public static void GetRayFromScreenPoint()
		{
			BaseCamera cam = new LookAtCamera(new Vector(10, 10, 10));
			Vector halfSize = new Vector(3, 3, 3);

			Application.Start(delegate
			{
				Grid.Draw();

				// Get a ray from the middle of the screen
				Point input = Input.Mouse.Position;
				Ray screenRay = ScreenSpace.GetRayFromScreenPoint(input);
				// And convert it back to 2D to make sure it works.
				Vector resultingPosition =
					ScreenSpace.Project(screenRay.Position + screenRay.Direction);
				bool isIntersecting = false;

				// draw a simple cube from (-3, -3, -3) to (3, 3, 3)
				Box.DrawOutline(-halfSize, halfSize,
					isIntersecting
						? Color.Green
						: Color.Red);

				Font.Default.Draw(
					"input=" + input + "\n" +
					"screenRay=" + screenRay + "\n" +
					"resultingPosition=" + resultingPosition,
					Rectangle.FromCenter(new Point(0.5f, 0.3f), Size.Half));
			});
		}
		#endregion

		#region DrawRotatedBox3D (Static)
		/// <summary>
		/// Draw rotated box 3D
		/// </summary>
		[Test]
		public static void DrawRotatedBox3D()
		{
			BaseCamera cam = new LookAtCamera(new Vector(7, -10, 8));
			Vector rotation = new Vector();
			Vector halfSize = new Vector(3, 3, 3);

			Application.Start(delegate
			{
				rotation.X += Time.Delta * 10.0f;
				Box.DrawOutline(-halfSize, halfSize, Color.Red, rotation);
			});
		}
		#endregion

		#region DrawSphere (Static)
		/// <summary>
		/// Draw sphere
		/// </summary>
		[Test]
		public static void DrawSphere()
		{
			BaseCamera cam = new LookAtCamera(new Vector(10, 10, 10));
			Vector center = Vector.Zero;

			Application.Start(delegate
			{
				// draw some sime spheres around each other
				//Sphere.DrawOutline(center, 1f, Color.Red);
				Sphere.DrawOutline(center, 5f, Color.Yellow); //.Blue);
				//Sphere.DrawOutline(center, 15f, Color.Yellow);
				//Sphere.DrawOutline(center, 25f, Color.Red);
				//Sphere.DrawOutline(center, 50f, Color.Blue);

				Sphere.DrawFilled(center, 4f, Color.Red);

				Sphere.DrawFilled(new Vector(10f, 0f, 0f), 4f, Color.Green);
			});
		}
		#endregion

		#region DrawSimpleQuad3D (Static)
		/// <summary>
		/// Draw simple quad 3d
		/// </summary>
		[Test]
		public static void DrawSimpleQuad3D()
		{
			// We create a 3d quad, so we need a camera.
			BaseCamera cam = new LookAtCamera(new Vector(4, -4, 4));

			// Create a geometry data instance which will hold the actual data.
			GeometryData geometryData = new GeometryData("<SimpleQuad3D>", 4,
				VertexFormat.Position3DTextured, 6, true, false);

			// Fill the geometry with actual data.
			geometryData.SetVertexData(0, new Vector(1, 0, 0), Point.One);
			geometryData.SetVertexData(1, new Vector(-1, 0, 0), Point.UnitY);
			geometryData.SetVertexData(2, new Vector(-1.25f, 0, 1), Point.Zero);
			geometryData.SetVertexData(3, new Vector(1.25f, 0, 1), Point.UnitX);

			geometryData.Indices = new ushort[]
			{
				0, 2, 1,
				0, 3, 2,
			};
			// Now create a geometry from the data.
			Geometry geometry = Geometry.Create(geometryData);

			// We need a material to draw the geometry. Simply use the default.
			MaterialColored material = MaterialColored.Default;

			// Now start the application...
			Application.Start(delegate
			{
				// ...and draw the geometry.
				material.Draw(geometry);
			});
		}
		#endregion

		#region DrawGrid (Static)
		/// <summary>
		/// Draw grid
		/// </summary>
		[Test]
		public static void DrawGrid()
		{
			LookAtCamera cam = new LookAtCamera(new Vector(1, -5, 3));

			Application.Start(delegate
			{
				// Draw a grid.
				Grid.Draw();
			});
		}
		#endregion

		#region DrawCulledLines3D (Static)
		/// <summary>
		/// Draw culled lines 3d.
		/// </summary>
		[Test]
		public static void DrawCulledLines3D()
		{
			BaseCamera cam = new LookAtCamera(new Vector(4, -4, 4));

			Application.Start(delegate
			{
				int numberOfTotalLines = 0;
				int numberOfDrawnLines = 0;
				for (float posX = -100f; posX <= 100f; posX += 5f)
				{
					for (float posY = -100f; posY <= 100f; posY += 5f)
					{
						numberOfTotalLines++;
						Vector pos = new Vector(posX, posY, 0f);
						Line.Draw(pos, pos + Vector.UnitZ, Color.Green);
						if (Line.WasLastLineCulled == false)
						{
							numberOfDrawnLines++;
						}
					}
				}

				Application.Window.Title = "Number of lines drawn: " +
				                           numberOfDrawnLines + "/" + numberOfTotalLines +
				                           " | FPS=" + Time.Fps;
			});
		}
		#endregion

		#region DrawFilledRectangleWithFont (Static)
		/// <summary>
		/// Draw filled rectangle with font.  bug #3616
		/// </summary>
		[Test]
		public static void DrawFilledRectangleWithFont()
		{
			Font drawFont = Font.Default;
			Rectangle drawRectangle = new Rectangle(0.5f, 0.5f, 0.25f, 0.25f);
			Application.Start(delegate
			{
				Rect.DrawFilled(drawRectangle, Color.Red);

				drawFont.Draw("Hello World", drawRectangle);
			});
		}
		#endregion

		#region LoadDrawManager (Static)
		/// <summary>
		/// Load draw manager
		/// </summary>
		[Test]
		public static void LoadDrawManager()
		{
		}
		#endregion

		#region DrawMeasure (Static)
		/// <summary>
		/// Draw measure
		/// </summary>
		[Test]
		public static void DrawMeasure()
		{
			//Point startPos = Point.Half;

			Application.Start(delegate
			{
				/*obs, we don't have input here.
				if (Input.Mouse.GetState(InputButton.MouseLeft) == InputState.Pressed)
				{
					startPos = Input.Mouse.Position;
				}

				if (Input.Mouse.LeftButtonIsPressed)
				{
					Graphic.DrawManager.Measure(startPos, Input.Mouse.Position, Color.Red);
				} // if
				 */
				//DrawManager.Measure(ScreenSpace.Area.TopLeft, ScreenSpace.Area.Center,
				//  Color.Red);
			});
		}
		#endregion
	}
}
