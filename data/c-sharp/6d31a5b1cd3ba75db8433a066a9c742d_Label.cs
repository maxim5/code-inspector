//Implementation based on techniques described in the tutorials by Jamie McMahon on http://www.XnaGpa.net
namespace GameLibrary.Controls
{
    public class Label : Control
    {
        public Label()
        {
            TabStop = false;
        }

        public override void Update(Microsoft.Xna.Framework.GameTime gameTime)
        {
        }

        public override void Draw(Microsoft.Xna.Framework.Graphics.SpriteBatch spriteBatch)
        {
            spriteBatch.DrawString(SpriteFont, Text, Position, Color);
        }

        public override void HandleInput()
        {
        }
    }
}
