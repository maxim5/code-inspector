using System;
using System.Runtime.InteropServices;
using SlimDX;
using SlimDX.Direct3D10;
using Buffer = SlimDX.Direct3D10.Buffer;

namespace Graphics.Streams
{
    public abstract class Stream<T> : IDisposable, IStream where T : struct
    {
        private readonly T[] mData;
        protected int ElementSize { get; private set; }
        protected Device Device { get; private set; }
        protected Buffer Buffer { get; private set; }

        protected Stream(Device device, T[] data, BindFlags bindFlags)
        {
            Device = device;
            mData = data;
            ElementSize = Marshal.SizeOf(typeof(T));

            CreateBuffer(bindFlags);
        }

        private void CreateBuffer(BindFlags bindFlags)
        {
            int sizeInBytes = mData.Length * ElementSize;
            var stream = new DataStream(sizeInBytes, canRead: true, canWrite: true);

            foreach (var element in mData)
            {
                stream.Write(element);
            }

            // Important: when specifying initial buffer data like this, the buffer will
            // read from the current DataStream position; we must rewind the stream to 
            // the start of the data we just wrote.
            stream.Position = 0;

            var bufferDescription = new BufferDescription
            {
                BindFlags = bindFlags,
                CpuAccessFlags = CpuAccessFlags.None,
                OptionFlags = ResourceOptionFlags.None,
                SizeInBytes = sizeInBytes,
                Usage = ResourceUsage.Default
            };

            Buffer = new Buffer(Device, stream, bufferDescription);
            stream.Dispose();


        }

        public abstract void OnFrame();

        public void Dispose()
        {
            Buffer.Dispose();
        }
    }
}